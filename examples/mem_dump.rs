#![feature(iter_array_chunks)]

use clap::{arg, builder::TypedValueParser, command, Parser};
use clap_num::maybe_hex;
use cuvave_midi::Message;

use midir::{
    ConnectError, Ignore, InitError, MidiInput, MidiInputPort, MidiOutput, MidiOutputPort,
    PortInfoError, SendError,
};
use std::fmt::Display;
use std::time::Duration;
use std::{
    error::Error,
    path::{Path, PathBuf},
};
use tokio::{
    fs::File,
    io::{self, AsyncWriteExt},
    sync::mpsc::{channel, error::SendError as TokioSendError, Receiver, Sender},
};
use tokio::{spawn, time};

#[derive(Parser, Debug)]
#[command(name = "mem_dump")]
struct Args {
    #[arg(long, default_value_t = 0)]
    input_port: u8,

    #[arg(long, default_value_t = 0)]
    output_port: u8,

    #[arg(short, long, exclusive = true)]
    list: bool,

    #[arg(short, long)]
    output_file: PathBuf,

    #[arg(short, long, value_parser = clap::builder::PossibleValuesParser::new(["4", "5", "0"]).map(|s| s.parse::<u8>().unwrap()),)]
    mode: u8,

    #[arg(long, default_value_t = 0, value_parser=maybe_hex::<u32>)]
    offset: u32,

    #[arg(long)]
    length: usize,
}

#[derive(Debug)]
enum CliError {
    InvalidMidiId(u8),
    MidiInit(InitError),
    PortInfo(PortInfoError),
    OutputConnectError(ConnectError<MidiOutput>),
    Send(SendError),
    TokioSend(TokioSendError<Message>),
    IO(io::Error),
}

impl Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::InvalidMidiId(n) => f.write_fmt(format_args!("Invalid port ID: {n}")),
            CliError::MidiInit(err) => f.write_fmt(format_args!("{err}")),
            CliError::PortInfo(err) => f.write_fmt(format_args!("{err}")),
            CliError::OutputConnectError(err) => f.write_fmt(format_args!("{err}")),
            CliError::Send(err) => f.write_fmt(format_args!("{err}")),
            CliError::IO(err) => f.write_fmt(format_args!("{err}")),
            CliError::TokioSend(err) => f.write_fmt(format_args!("{err}")),
        }
    }
}

impl Error for CliError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }
}

async fn run(
    mode: u8,
    dump_file: impl AsRef<Path>,
    offset: u32,
    dump_len: usize,
    midi_in: MidiInput,
    in_port: u8,
    midi_out: MidiOutput,
    out_port: u8,
) -> Result<(), CliError> {
    let midi_in_port = midi_in.ports()[in_port as usize].clone();
    let midi_out_port = midi_out.ports()[out_port as usize].clone();

    let (out_sender, out_receiver) = channel(16);
    let (in_sender, in_receiver) = channel(16);

    spawn(input_task(in_sender, midi_in, midi_in_port));
    spawn(output_task(out_receiver, midi_out, midi_out_port));
    dump_task(mode, dump_file, offset, dump_len, out_sender, in_receiver).await?;
    Ok(())
}

async fn input_task(
    in_sender: Sender<Message>,
    midi_in: MidiInput,
    midi_in_port: MidiInputPort,
) -> Result<(), ConnectError<MidiInput>> {
    let _conn = midi_in.connect(
        &midi_in_port,
        "input",
        move |_, message, _| {
            in_sender.try_send(Message::from(message).unwrap()).unwrap();
        },
        (),
    )?;

    println!("IN connected");

    loop {
        time::sleep(Duration::from_millis(10)).await;
    }
}

async fn output_task(
    mut out_receiver: Receiver<Message>,
    midi_out: MidiOutput,
    midi_out_port: MidiOutputPort,
) -> Result<(), CliError> {
    let mut out = midi_out
        .connect(&midi_out_port, "input")
        .map_err(CliError::OutputConnectError)?;

    println!("OUT connected");

    loop {
        let message = out_receiver.recv().await;

        if let Some(message) = message {
            out.send(&message.to_vec()).map_err(CliError::Send)?;
            time::sleep(Duration::from_millis(10)).await;
        }
    }
}

async fn receive_data(in_receiver: &mut Receiver<Message>, f: &mut File) -> Result<(), CliError> {
    match in_receiver.recv().await {
        Some(Message::MemoryContent(_, _, _, data)) => {
            f.write_all(&data).await.map_err(CliError::IO)?;
        }
        Some(msg) => {
            panic!("Unexpected message: {msg:?}");
        }
        None => {
            unreachable!("This wasn't supposed to happen!")
        }
    }
    Ok(())
}

async fn dump_task(
    mode: u8,
    dump_file: impl AsRef<Path>,
    offset: u32,
    dump_len: usize,
    out_sender: Sender<Message>,
    mut in_receiver: Receiver<Message>,
) -> Result<(), CliError> {
    let mut f = File::create(dump_file).await.map_err(CliError::IO)?;

    let batches = dump_len / 128;
    let remainder = dump_len % 128;

    for batch in 0..batches {
        out_sender
            .send(Message::ReadMemory(mode, offset + batch as u32 * 0x80, 128))
            .await
            .map_err(CliError::TokioSend)?;
        receive_data(&mut in_receiver, &mut f).await?;
    }

    if remainder > 0 {
        out_sender
            .send(Message::ReadMemory(
                mode,
                offset + batches as u32 * 0x80,
                remainder as u32,
            ))
            .await
            .map_err(CliError::TokioSend)?;
        receive_data(&mut in_receiver, &mut f).await?
    }

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), CliError> {
    let args = Args::parse();
    let mut midi_in = MidiInput::new("cuvave dump in").map_err(CliError::MidiInit)?;
    midi_in.ignore(Ignore::None);

    let midi_out = MidiOutput::new("cuvave dump out").map_err(CliError::MidiInit)?;

    if args.list {
        println!("Available input ports:");
        for (i, p) in midi_in.ports().iter().enumerate() {
            println!(
                "{}: {}",
                i,
                midi_in.port_name(p).map_err(CliError::PortInfo)?
            );
        }

        println!("\nAvailable output ports:");
        for (i, p) in midi_out.ports().iter().enumerate() {
            println!(
                "{}: {}",
                i,
                midi_out.port_name(p).map_err(CliError::PortInfo)?
            );
        }
        return Ok(());
    }

    if args.input_port as usize >= midi_in.port_count() {
        return Err(CliError::InvalidMidiId(args.input_port));
    } else if args.output_port as usize >= midi_out.port_count() {
        return Err(CliError::InvalidMidiId(args.output_port));
    }

    match run(
        args.mode,
        args.output_file,
        args.offset,
        args.length,
        midi_in,
        args.input_port,
        midi_out,
        args.output_port,
    )
    .await
    {
        Ok(_) => (),
        Err(err) => println!("Error: {}", err),
    }
    Ok(())
}
