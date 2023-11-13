use nom::Finish;
use serde::{Deserialize, Serialize};

pub mod parser;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Parameter {
    Type = 9,
    Gain = 1,
    Tone = 2,
    Reverb = 3,
    Feedback = 4,
    Volume = 5,
    Time = 6,
    Mix = 7,
    Modulation = 8,
    Cabinet = 0,
    IRSection = 0xa,
    DelaySection = 0xb,
    ToneSection = 0xc,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum Preset {
    A = 0,
    B = 0x10,
    C = 0x20,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct Settings {
    _type: u8,
    gain: u8,
    tone: u8,
    reverb: u8,
    feedback: u8,
    volume: u8,
    time: u8,
    mix: u8,
    modulation: u8,
    cabinet: u8,
    ir_section: bool,
    delay_section: bool,
    tone_section: bool,
}

impl Settings {
    pub fn from_slice(data: &[u8]) -> Self {
        Self {
            _type: data[0],
            gain: data[1],
            tone: data[2],
            reverb: data[3],
            feedback: data[4],
            volume: data[5],
            time: data[6],
            mix: data[7],
            modulation: data[8],
            cabinet: data[9],
            ir_section: data[10] > 0,
            delay_section: data[11] > 0,
            tone_section: data[12] > 0,
        }
    }
}

#[derive(PartialEq, Serialize, Deserialize)]
pub enum Message {
    Init,
    ACK(bool),
    WriteMemory(u8, u32, u32, Vec<u8>),
    ReadMemory(u8, u32, u32),
    MemoryContent(u8, u32, u32, Vec<u8>),
    RequestNameVersion,
    NameVersion(String, Vec<u8>),
    Erase(u8, u32),
    Mystery1,
    Mystery2,
}

struct DebugBuilder<'t> {
    name: &'t str,
    fields: Vec<(&'t str, &'t dyn std::fmt::Debug, bool)>,
}

impl<'t> DebugBuilder<'t> {
    fn new(name: &'t str) -> Self {
        Self {
            name,
            fields: Vec::new(),
        }
    }

    fn field(self, name: &'t str, val: &'t dyn std::fmt::Debug) -> Self {
        let mut fields = self.fields;
        fields.push((name, val, false));
        Self {
            name: self.name,
            fields,
        }
    }

    fn hex_field(self, name: &'t str, val: &'t dyn std::fmt::Debug) -> Self {
        let mut fields = self.fields;
        fields.push((name, val, true));
        Self {
            name: self.name,
            fields,
        }
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<")?;
        f.write_str(self.name)?;
        for (key, val, is_hex) in &self.fields {
            f.write_str(&if *is_hex {
                format!(" {key}={val:x?}")
            } else {
                format!(" {key}={val:?}")
            })?;
        }

        f.write_str(">")
    }
}

impl std::fmt::Debug for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Init => DebugBuilder::new("Init").fmt(f),
            Self::ACK(val) => DebugBuilder::new("ACK").field("val", val).fmt(f),
            Self::ReadMemory(_type, addr, len) => DebugBuilder::new("ReadMemory")
                .hex_field("type", _type)
                .hex_field("addr", addr)
                .field("len", len)
                .fmt(f),
            Self::MemoryContent(_type, addr, len, data) => DebugBuilder::new("MemoryContent")
                .hex_field("type", _type)
                .hex_field("addr", addr)
                .field("len", len)
                .hex_field("data", &format!("{:x?}", data))
                .fmt(f),
            Self::WriteMemory(_type, addr, len, data) => DebugBuilder::new("WriteMemory")
                .hex_field("type", _type)
                .hex_field("addr", addr)
                .field("len", len)
                .hex_field("data", &format!("{:x?}", data))
                .fmt(f),
            Self::Erase(_type, addr) => DebugBuilder::new("Erase")
                .hex_field("type", _type)
                .hex_field("addr", addr)
                .fmt(f),
            Self::NameVersion(name, mystery) => DebugBuilder::new("NameVersion")
                .field("name", name)
                .hex_field("mystery", mystery)
                .fmt(f),
            Self::RequestNameVersion => DebugBuilder::new("RequestNameVersion").fmt(f),
            Self::Mystery1 => DebugBuilder::new("Mystery1").fmt(f),
            Self::Mystery2 => DebugBuilder::new("Mystery2").fmt(f),
        }
    }
}

impl Message {
    pub fn from(data: &[u8]) -> Result<Message, parser::DeserializeError> {
        let envelope = parser::demangle_envelope(data)?;
        let content = parser::parse_envelope(&envelope)?;
        parser::parse_content(&content)
            .finish()
            .map_err(|e| parser::DeserializeError::ContentParse(format!("{e:?}")))
            .map(|(_, msg)| msg)
    }

    pub fn to_clear_vec(&self) -> Vec<u8> {
        let mut data = Vec::from(parser::HEADER);
        // TODO!
        let (msg_type, content) = match self {
            Message::Init => (0x00, vec![]),
            Message::ACK(val) => (0x00, vec![*val as u8]),
            Message::WriteMemory(cmd, addr, len, data) => {
                let mut res = vec![*cmd];
                res.extend_from_slice(&addr.to_le_bytes());
                res.extend_from_slice(&len.to_le_bytes()[0..3]);
                res.extend(data);
                (0x22, res)
            }
            Message::ReadMemory(cmd, addr, len) => {
                let mut res = vec![*cmd];
                res.extend_from_slice(&addr.to_le_bytes());
                res.extend_from_slice(&len.to_le_bytes()[0..3]);
                (0x23, res)
            }
            Message::MemoryContent(cmd, addr, len, data) => {
                let mut res = vec![*cmd];
                res.extend_from_slice(&addr.to_le_bytes());
                res.extend_from_slice(&len.to_le_bytes()[0..3]);
                res.extend(data);
                (0x23, res)
            }
            Message::NameVersion(name, mystery) => {
                let mut res = name[0..16].as_bytes().to_vec();
                res.extend(mystery);
                (0x11, res)
            }
            Message::Erase(cmd, addr) => {
                let mut res = vec![*cmd];
                res.extend_from_slice(&addr.to_le_bytes());
                (0x21, res)
            }
            Message::RequestNameVersion => (0x11, vec![]),
            Message::Mystery1 => (0x24, vec![0x4, 0x64, 0x7, 0, 0, 0x1, 0, 0, 0]),
            Message::Mystery2 => (0x24, vec![0x4, 0x68, 0x7, 0, 0, 0x4, 0x8, 0, 0]),
        };

        data.push(msg_type);
        data.extend(&content.len().to_le_bytes()[0..3]);
        data.extend(content);

        // add checksum
        let chk = parser::calc_checksum(&data[6..]);
        data.push(chk);

        data
    }

    pub fn to_vec(&self) -> Vec<u8> {
        let data = self.to_clear_vec();

        // encode to funny bit-shifted format this thing uses
        let mut encoded = Vec::new();
        parser::encode(&data, &mut encoded);

        encoded.insert(0, 0xf0);
        encoded.push(0xf7);
        encoded
    }
}

#[derive(Serialize, Deserialize)]
pub struct PresetParameterPair(Preset, Parameter);

impl PresetParameterPair {
    pub fn free(self) -> (Preset, Parameter) {
        (self.0, self.1)
    }
}

impl TryFrom<u8> for Preset {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0x0 => Preset::A,
            0x10 => Preset::B,
            0x20 => Preset::C,
            _ => return Err(()),
        })
    }
}

impl TryFrom<u8> for Parameter {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Parameter::Cabinet,
            1 => Parameter::Gain,
            2 => Parameter::Tone,
            3 => Parameter::Reverb,
            4 => Parameter::Feedback,
            5 => Parameter::Volume,
            6 => Parameter::Time,
            7 => Parameter::Mix,
            8 => Parameter::Modulation,
            9 => Parameter::Type,
            0xa => Parameter::IRSection,
            0xb => Parameter::DelaySection,
            0xc => Parameter::ToneSection,
            _ => return Err(()),
        })
    }
}

impl TryFrom<&str> for Parameter {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "Cabinet" | "cabinet" => Parameter::Cabinet,
            "Gain" | "gain" => Parameter::Gain,
            "Tone" | "tone" => Parameter::Tone,
            "Reverb" | "reverb" => Parameter::Reverb,
            "Feedback" | "feedback" => Parameter::Feedback,
            "Volume" | "volume" => Parameter::Volume,
            "Time" | "time" => Parameter::Time,
            "Mix" | "mix" => Parameter::Mix,
            "Modulation" | "modulation" => Parameter::Modulation,
            "Type" | "_type" => Parameter::Type,
            "IRSection" | "ir_section" => Parameter::IRSection,
            "DelaySection" | "delay_section" => Parameter::DelaySection,
            "ToneSection" | "tone_section" => Parameter::ToneSection,
            _ => return Err(()),
        })
    }
}

impl TryFrom<u8> for PresetParameterPair {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        let preset = (value & 0xf0).try_into()?;
        let param = (value & 0xf).try_into()?;
        Ok(PresetParameterPair(preset, param))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deserialize_set_parameters() {
        let res =
            Message::from(&hex::decode("f000320949000040021b00000018000000013a01f7").unwrap())
                .unwrap();

        assert!(
            res == Message::WriteMemory(
                0x05,
                0x80000000 + Preset::B as u32 + Parameter::DelaySection as u32,
                1,
                vec![1]
            )
        );
    }

    #[test]
    fn parse_parameter() {
        assert_eq!(
            Parameter::Modulation,
            Parameter::try_from("modulation").unwrap()
        );
        assert_eq!(
            Parameter::Modulation,
            Parameter::try_from("Modulation").unwrap()
        );
        assert_eq!(
            Parameter::IRSection,
            Parameter::try_from("ir_section").unwrap()
        );
        assert_eq!(
            Parameter::IRSection,
            Parameter::try_from("IRSection").unwrap()
        );
    }
}
