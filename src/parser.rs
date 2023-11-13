use super::Message;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, map_res, rest},
    error::{context, VerboseError, VerboseErrorKind},
    sequence::tuple,
    IResult,
};
use serde::{Deserialize, Serialize};
use std::io::Write;

pub const HEADER: [u8; 2] = [0x00, 0x59];

trait LEParseable: Sized {
    fn parse_le(data: &[u8]) -> Self {
        let mut buffer = [0u8; 4];

        for (n, b) in data.iter().enumerate() {
            buffer[n] = *b;
        }

        Self::_parse_le(buffer)
    }

    fn _parse_le(data: [u8; 4]) -> Self;
}

impl LEParseable for u32 {
    fn _parse_le(data: [u8; 4]) -> Self {
        u32::from_le_bytes(data)
    }
}

impl LEParseable for f32 {
    fn _parse_le(data: [u8; 4]) -> Self {
        f32::from_le_bytes(data)
    }
}

pub fn encode<W: Write>(source: &[u8], dest: &mut W) {
    let mut bit_num = 0u32;
    let mut accum = 0u32;
    for b in source {
        accum |= (*b as u32) << (bit_num & 0x1f);
        bit_num += 1;

        loop {
            dest.write_all(&[accum as u8 & 0x7f]).unwrap();
            accum >>= 7;

            if bit_num < 7 {
                break;
            } else {
                bit_num -= 7;
            }
        }
    }

    dest.write_all(&[(accum & 0x7f) as u8]).unwrap();
}

pub fn decode<W: Write>(source: &[u8], dest: &mut W) {
    let mut accum;
    let mut bit_num = 0;
    let mut last_val = 0u8;
    for b in source.iter() {
        accum = *b as u32 & (0xffffffffu32.overflowing_shr(32 - bit_num).0);

        if bit_num > 0 {
            dest.write_all(&[(last_val & 0x7f) | (accum << (8 - bit_num)) as u8])
                .unwrap();
        }
        last_val = *b >> bit_num;

        bit_num = (bit_num + 1) % 8;
    }

    if last_val > 0 {
        dest.write_all(&[last_val]).unwrap();
    }
}

pub fn calc_checksum(data: &[u8]) -> u8 {
    !data.iter().fold(0, |acc, e| (u8::wrapping_add(acc, *e)))
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DeserializeError {
    InputTooShort,
    InvalidHeader,
    InvalidMarkers,
    InvalidChecksum,
    UnknownByteSequence,
    UnexpectedInputSize,
    MessageTypeUnknown,
    ContentParse(String),
}

fn parse_ack(input: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    take(1u8)(input).map(|(input, val)| (input, Message::ACK(val[0] > 0)))
}

fn parse_read_msg(input: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    let (input, (_type, addr, len, data)) = tuple((take(1u8), take(4u8), take(3u8), rest))(input)?;
    //
    let len = u32::parse_le(len);
    let addr = u32::parse_le(addr);

    Ok((
        input,
        if data.is_empty() && len > 0 {
            Message::ReadMemory(_type[0], addr, len)
        } else {
            // if there is an actually returned content, this is a memory read result
            Message::MemoryContent(_type[0], addr, len, data.to_vec())
        },
    ))
}

fn parse_write_msg(input: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    let (input, (_type, addr, len, data)) = tuple((take(1u8), take(4u8), take(3u8), rest))(input)?;
    if data.len() != len[0] as usize {
        return Err(nom::Err::Error(VerboseError {
            errors: vec![(data, VerboseErrorKind::Context("data_length"))],
        }));
    }

    let len = u32::parse_le(len);
    let addr = u32::parse_le(addr);

    Ok((
        input,
        Message::WriteMemory(_type[0], addr, len, data.to_vec()),
    ))
}

fn parse_query_name_ver(input: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    map_res(tuple((take(16u8), rest)), |(v, r)| {
        // TODO: figure out what the rest of the bytes mean!
        // NOTE: reverse-engineering of the code suggests that the name takes 16 bytes, but the cuvave
        // seems to be sending 19 (?)
        std::str::from_utf8(v).map(|m| Message::NameVersion(m.to_owned(), r.to_vec()))
    })(input)
}

fn parse_erase_msg(input: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    map_res(
        tuple((take(1u8), take(4u8))),
        |(_type, addr): (&[u8], &[u8])| -> Result<Message, ()> {
            Ok(Message::Erase(_type[0], u32::parse_le(addr)))
        },
    )(input)
}

pub fn parse_content(data: &[u8]) -> IResult<&[u8], Message, VerboseError<&[u8]>> {
    let (rest, (msg_type, len_bytes)) =
        context("inner_header", tuple((take(1u8), take(3u8))))(data)?;

    let len = u32::parse_le(len_bytes);

    if rest.len() != len as usize {
        return Err(nom::Err::Error(VerboseError {
            errors: vec![(rest, VerboseErrorKind::Context("message_length"))],
        }));
    }

    match (msg_type[0], rest.len()) {
        (0x00, 0) => Ok((&[], Message::Init)),
        (0x00, _) => context("nor_ext", parse_ack)(rest),
        (0x11, 0) => Ok((&[], Message::RequestNameVersion)),
        (0x11, _) => context("query_name_ver", parse_query_name_ver)(rest),
        (0x21, _) => context("cmd_erase", parse_erase_msg)(rest),
        (0x22, _) => context("cmd_write", parse_write_msg)(rest),
        (0x23, _) => context("cmd_read", parse_read_msg)(rest),
        (0x24, 9) => map(
            alt((
                tag([0x4, 0x64, 0x7, 0, 0, 0x1, 0, 0, 0]),
                tag([0x4, 0x68, 0x7, 0, 0, 0x4, 0x8, 0, 0]),
            )),
            |v: &[u8]| {
                if v[1] == 0x64 {
                    Message::Mystery1
                } else {
                    Message::Mystery2
                }
            },
        )(rest),
        _ => Err(nom::Err::Error(VerboseError { errors: vec![] })),
    }
}

pub fn demangle_envelope(data: &[u8]) -> Result<Vec<u8>, DeserializeError> {
    if data.len() < 6 {
        Err(DeserializeError::InputTooShort)
    } else if data[0] != 0xf0 || data[data.len() - 1] != 0xf7 {
        Err(DeserializeError::InvalidMarkers)
    } else {
        let mut decoded = Vec::new();
        decode(&data[1..data.len() - 1], &mut decoded);
        Ok(decoded)
    }
}

pub fn parse_envelope(data: &[u8]) -> Result<Vec<u8>, DeserializeError> {
    let (header, rest) = data.split_at(2);

    if header != HEADER {
        Err(DeserializeError::InvalidHeader)
    } else {
        let mut rest = Vec::from_iter(rest.iter().cloned());

        // checksum
        let checksum = rest.pop().ok_or(DeserializeError::InputTooShort)?;

        if checksum != calc_checksum(&rest[4..]) {
            Err(DeserializeError::InvalidChecksum)
        } else {
            Ok(rest)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{decode, encode};
    use crate::Message;

    #[test]
    fn test_encode_1() {
        let mut result = Vec::new();
        encode(
            &[
                0x00, 0x59, 0x22, 0x09, 0x00, 0x00, 0x05, 0x09, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00,
                0x01, 0x6f,
            ],
            &mut result,
        );

        assert!(
            result
                == [
                    0x0, 0x32, 0x9, 0x49, 0x0, 0x0, 0x40, 0x2, 0x9, 0x0, 0x0, 0x0, 0x18, 0x0, 0x0,
                    0x0, 0x1, 0x5e, 0x01
                ]
        )
    }

    #[test]
    fn test_encode_2() {
        let mut result = Vec::new();
        encode(&[0x00, 0x59, 0x11, 0x00, 0x00, 0x00, 0xff], &mut result);
        assert!(result == [0x00, 0x32, 0x45, 0x00, 0x00, 0x00, 0x40, 0x7f, 0x00])
    }

    #[test]
    fn test_decode_1() {
        let mut result = Vec::new();
        decode(
            &[
                0x0, 0x32, 0x9, 0x49, 0x0, 0x0, 0x40, 0x2, 0x9, 0x0, 0x0, 0x0, 0x18, 0x0, 0x0, 0x0,
                0x1, 0x5e, 0x1,
            ],
            &mut result,
        );

        assert!(
            result
                == [
                    0x00, 0x59, 0x22, 0x09, 0x00, 0x00, 0x05, 0x09, 0x00, 0x00, 0x80, 0x01, 0x00,
                    0x00, 0x01, 0x6f,
                ]
        )
    }

    #[test]
    fn test_decode_2() {
        assert_eq!(
            Message::from(&[240, 0, 50, 1, 0, 0, 0, 64, 127, 0, 247]).unwrap(),
            Message::Init
        );
    }

    #[test]
    fn test_decode_3() {
        let mut dest = Vec::new();
        decode(
            &[
                0, 50, 17, 73, 0, 0, 0, 2, 100, 14, 0, 0, 16, 0, 0, 0, 0, 30, 2,
            ],
            &mut dest,
        );
        assert_eq!(
            Message::from(&[
                240, 0, 50, 17, 73, 0, 0, 0, 2, 100, 14, 0, 0, 16, 0, 0, 0, 0, 30, 2, 247,
            ])
            .unwrap(),
            Message::Mystery1
        );
    }
}
