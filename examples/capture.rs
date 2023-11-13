use cuvave_midi::parser;

use midir::os::unix::VirtualInput;
use midir::{ConnectError, Ignore, MidiInput};
use owo_colors::OwoColorize;
use std::error::Error;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::{spawn, time};

async fn run() -> Result<(), Box<dyn Error>> {
    let mut midi_in_client = MidiInput::new("cuvave spy")?;
    let mut midi_in_device = MidiInput::new("cuvave spy")?;
    midi_in_client.ignore(Ignore::None);
    midi_in_device.ignore(Ignore::None);

    spawn(input_task(midi_in_client, midi_in_device));

    loop {
        time::sleep(Duration::from_millis(100)).await;
    }
}

enum Origin {
    Client,
    Device,
}

async fn input_task(
    midi_in_client: MidiInput,
    midi_in_device: MidiInput,
) -> Result<(), ConnectError<MidiInput>> {
    let queue: Arc<Mutex<Vec<(Origin, Vec<u8>)>>> = Arc::new(Mutex::new(Vec::new()));

    let queue_clone_1 = queue.clone();
    let queue_clone_2 = queue.clone();

    let _device_in = midi_in_device.create_virtual(
        "device-in",
        move |_, message, _| {
            let mut q = queue_clone_1.lock().unwrap();
            q.push((Origin::Device, Vec::from_iter(message.iter().cloned())));
        },
        (),
    )?;

    let _client_in = midi_in_client.create_virtual(
        "client-in",
        move |_, message, _| {
            let mut q = queue_clone_2.lock().unwrap();
            q.push((Origin::Client, Vec::from_iter(message.iter().cloned())));
        },
        (),
    )?;

    loop {
        for (origin, data) in queue.lock().unwrap().drain(..) {
            let origin = match origin {
                Origin::Client => "C".black().on_cyan().to_string(),
                Origin::Device => "D".black().on_yellow().to_string(),
            };
            match parser::demangle_envelope(&data) {
                Ok(envelope) => {
                    // Envelope can be demangled
                    match parser::parse_envelope(&envelope) {
                        Ok(content) => {
                            let content_text = format!("{:x?}", content);
                            match parser::parse_content(&content) {
                                Ok((_, msg)) => {
                                    println!(
                                        "{}{} {}",
                                        origin,
                                        " ".on_bright_green(),
                                        format!("{:?}", msg).green()
                                    );
                                }
                                Err(e) => {
                                    println!(
                                        "{}{} Cannot parse message content: {} [{e:?}]",
                                        origin,
                                        " ".on_red(),
                                        content_text.red()
                                    );
                                }
                            }
                        }
                        Err(e) => {
                            println!(
                                "{}{} Cannot parse message envelope: {} [{e:?}]",
                                origin,
                                " ".on_red(),
                                format!("{:?}", envelope).red()
                            );
                        }
                    }
                }
                Err(e) => {
                    // try to
                    println!(
                        "{}{} Cannot parse message: {} [{e:?}]",
                        origin,
                        " ".on_red(),
                        format!("{:?}", data).red()
                    );
                }
            }
        }
        time::sleep(Duration::from_millis(10)).await;
    }
}

#[tokio::main]
async fn main() {
    match run().await {
        Ok(_) => (),
        Err(err) => println!("Error: {}", err),
    }
}
