use crate::parser::datum::{datum, Datum};
use crate::parser::util::ParserErr::CouldNotFullyParse;
use std::fs::File;

#[derive(Debug, PartialEq, Eq)]
pub enum ParserErr {
    CouldNotFullyParse(usize),
    Other(usize, String),
    File,
}

pub fn from_string(code: &String) -> Result<Datum, ParserErr> {
    use ParserErr::*;

    let mut x = code.as_bytes().to_vec();
    x.push(b'\0');

    let length = x.len();

    match datum(x.as_ref()) {
        Ok((left, res)) => {
            if left == b"\0" {
                Ok(res)
            } else {
                Err(CouldNotFullyParse(length - left.len()))
            }
        }
        Err(err) => {
            Err(Other(0, err.to_string()))
        }
    }
}

pub fn from_file(name: &str) -> Result<Datum, ParserErr> {
    use std::io::prelude::*;

    let mut file = File::open(name).map_err(|_| ParserErr::File)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|_| ParserErr::File)?;

    from_string(&contents)
}