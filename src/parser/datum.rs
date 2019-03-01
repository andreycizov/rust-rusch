use nom::{named, alt, tag, recognize, tuple, many0, many1, do_parse, call, map};

use super::root::*;
use super::num::*;

/// TODO: A) convert all enum members to separate structs and return them appropriately from every parser
/// TODO: B) [123]

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Datum {
    /// todo [123] symbol needs a separate tag of it's origin.
    Symbol(String),
    /// the usize argument allows for expanded macros to be expanded in a separate namespace.
    //Symbol(String, usize),
    Bool(bool),
    Char(String),
    Str(String),
    Num(String),
    Vector(Vec<Datum>),
    ByteVector(Vec<Datum>),
    List(Vec<Datum>),
    Pair(Box<Datum>, Box<Datum>),
    LabelSet(String, Box<Datum>),
    LabelGet(String),
}

named!(
    pub datum(&[u8]) -> Datum,
    alt!(
        simple_datum |
        compound_datum |
        do_parse!(
            x: label >>
            tag!("=") >>
            y: datum >>
            ( Datum::LabelSet(x, Box::new(y)) )
        ) |
        do_parse!(
            x: label >>
            tag!("#") >>
            ( Datum::LabelGet(x) )
        )
    )
);

named!(
    simple_datum(&[u8]) -> Datum,
    alt!(
        boolean => { |x| match x {
                Token::Bool(x) => Datum::Bool(x),
                _ => unreachable!()
            }
        } |
        number => { |x| match x {
                Token::Num(x) => Datum::Num(x),
                _ => unreachable!()
            }
        } |
        character => { |x| match x {
                Token::Char(x) => Datum::Char(x),
                _ => unreachable!()
            }
        } |
        string => { |x| match x {
                Token::Str(x) => Datum::Str(x),
                _ => unreachable!()
            }
        } |
        symbol |
        bytevector
    )
);

named!(
    symbol(&[u8]) -> Datum,
    map!(
        call!(identifier),
        |x| match x {
            Token::Ident(x) => Datum::Symbol(x),
            _ => unreachable!()
        }
    )

);

named!(
    compound_datum(&[u8]) -> Datum,
    alt!(
        list |
        vector |
        abbreviation
    )
);

named!(
    list(&[u8]) -> Datum,
    alt!(
        do_parse!(
            tag!("(") >>
            x: many0!(do_parse!(
                intertoken_space >>
                x: datum >>
                intertoken_space >>
                ( x )
            )) >>
            tag!(")") >>
            ( x )
        ) => { |x| Datum::List(x) } |
        do_parse!(
            tag!("(") >>
            a: datum >>
            x: many0!(do_parse!(
                intertoken_space >>
                x: datum >>
                intertoken_space >>
                ( x )
            )) >>
            tag!(".") >>
            y: do_parse!(
                intertoken_space >>
                x: datum >>
                intertoken_space >>
                ( x )
            ) >>
            tag!(")") >>
            ( (a, x, y) )
        ) => { |(a, x, y): (Datum, Vec<Datum>, Datum)| {
            let mut r = Datum::Pair(Box::new(a), Box::new(y));

            for z in x {
                r = Datum::Pair(Box::new(z), Box::new(r));
            }

            r
        } }
    )
);

named!(
    abbreviation(&[u8]) -> Datum,
    do_parse!(
        q: abbrev_previx >>
        intertoken_space >>
        b: datum >>
        ( { Datum::List(vec![q.sym(), b])   } )
    )
);

pub enum Abbr {
    Quote,
    QuasiQuote,
    Unquote,
    UnquoteSplicing,
}

impl Abbr {
    pub fn sym(&self) -> Datum {
        match &self {
            Abbr::Quote => Datum::Symbol("quote".into()),
            Abbr::QuasiQuote => Datum::Symbol("quasiquote".into()),
            Abbr::Unquote => Datum::Symbol("unquote".into()),
            Abbr::UnquoteSplicing => Datum::Symbol("unquote-splicing".into()),
        }
    }
}

named!(
    abbrev_previx(&[u8]) -> Abbr,
    alt!(
        tag!("'") => {|_| Abbr::Quote } |
        tag!("`") => {|_| Abbr::QuasiQuote }  |
        tag!(",") => {|_| Abbr::Unquote }  |
        tag!(",@") => {|_| Abbr::UnquoteSplicing }
    )
);

named!(
    vector(&[u8]) -> Datum,
    map!(
        do_parse!(
            tag!("#(") >>
            its: many0!(do_parse!(
                intertoken_space >>
                x: datum >>
                intertoken_space >>
                ( x )
            )) >>
            tag!(")") >>
            ( its )
        ),
        |x| Datum::Vector(x)
    )
);

named!(
    byte(&[u8]) -> String,
    map!(call!(uinteger), |x| from_u8(x) )
    // todo post-check that the value is 0-255
);

named!(
    bytevector(&[u8]) -> Datum,
    map!(
        do_parse!(
            tag!("#u8(") >>
            its: many0!(do_parse!(
                intertoken_space >>
                x: byte >>
                intertoken_space >>
                ( x )
            )) >>
            tag!(")") >>
            ( its )
        ),
        |x| Datum::ByteVector(x.into_iter().map(|y| Datum::Num(y)).collect())
    )
);

named!(
    label(&[u8]) -> String,
    do_parse!(
        tag!("#") >>
        a: uinteger_10 >>
        ( from_u8(a) )
    )
);