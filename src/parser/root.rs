use nom::{Err as NomErr, Needed as NomNeeded, named, alt, alt_complete,
          tag, do_parse, many0,
          tuple, not, map, one_of, none_of, call, many1, take, take_while1, recognize};

use super::num::number;
use super::datum::datum;


// Page 62


static CR: &str = "\r";

/// Implements the `/(?x) (.*?) (remainder)/` pattern:
/// looks for remainder first, then returns a tuple with the prefix and the remainder.
///
/// Discussion: https://www.reddit.com/r/rust/comments/4yokxd/crash_course_into_nom_kind_of_question/
///
/// Example iterating over an `input`:
///
///         let mut pos = input;
///         while let IResult::Done (tail, (_head, _parsed_remainder)) = take_until_parse_s! (pos, tag! ("remainder")) {
///             pos = tail
///         }
//#[macro_export]
macro_rules! take_until_parse_s (
    ($i: expr, $remainder: ident! ($($args:tt)*)) => ({
        use nom::InputIter;

        let input: &[u8] = $i;

        let mut iterator = $i.iter_indices();

        let ret = loop {
            if let Some((pos, _)) = iterator.next() {
                match $remainder! (&input[pos..], $($args)*) {
                    Ok((more, o)) => {
                        break Ok((more, (&input[0..pos], o)));
                    }
                    Err(_) => continue,
                }
            } else {
                if let Ok((more, o)) = $remainder! (&input[0..0], $($args)*) {
                    break Ok((more, (&input[..], o)))
                } else {
                    break Err(NomErr::Incomplete(NomNeeded::Unknown))
                }
            }
        };

        ret
    });
    ($i: expr, $f: expr) => (
        take_until_parse_s! ($i, call! ($f));
    );
);

//macro_rules! take_while_parse_s (
//    ($i: expr, $remainder: ident! ($($args:tt)*)) => ({
//        let input: &[u8] = $i;
//
//        let mut pos = 0;
//
//        let ret = loop {
//            match $remainder! (&input[pos..], $($args)*) {
//                Ok((more, o)) => {
//                    let eaten = input[pos..].len() - more.len();
//                    pos += eaten;
//                }
//                Err(_) => break Ok((&input[pos..], &input[0..pos]))
//            }
//        };
//
//        ret
//    });
//    ($i: expr, $f: expr) => (
//        take_while_parse_s! ($i, call! ($f));
//    );
//);


#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    //Whitespace(&'a [u8]),
    Ident(String),
    Str(String),
    Num(String),
    Char(String),
    Bool(bool),

    LBr,
    RBr,

    Vec,
    ByteVec,

    Apo,
    Grave,
    CommaAt,
    Comma,
    Dot,
}

use std::str::from_utf8;

pub fn from_u8(x: &[u8]) -> String {
    from_utf8(x).unwrap().to_string()
}

named!(
    pub token(&[u8]) -> Token,
    alt!(
        identifier |
        boolean |
        number |
        character |
        string |
        tag!("(") => { |x| Token::LBr }  |
        tag!(")") => { |x| Token::RBr }  |
        tag!("#(") => { |x| Token::Vec }  |
        tag!("#u8(") => { |x| Token::ByteVec }  |
        tag!("'") => { |x| Token::Apo }  |
        tag!("`") => { |x| Token::Grave }  |
        tag!(",") => { |x| Token::Comma }  |
        tag!(",@") => { |x| Token::CommaAt }  |
        tag!(".") => { |x| Token::Dot }
    )
);

named!(
    pub delimiter,
    alt!(
        whitespace |
        vertical_line |
        tag!("(") |
        tag!(")") |
        tag!("\"") |
        tag!(";")
    )
);
named!(
    pub tokens(&[u8]) -> Vec<Token>,
    do_parse!(
        its: many0!(
            alt!(
                do_parse!(
                    a: token >>
                    intertoken_space >>
                    (a)
                ) |
                token
            )
        ) >>
        tag!("\0") >>
        ( its )
    )
);

named!(
    intraline_whitespace,
    alt_complete!(
        tag!("\t") |
        tag!(" ")
    )
);

named!(
    whitespace,
    alt_complete!(
        intraline_whitespace |
        line_ending
    )
);

named!(
    vertical_line,
    tag!("|")
);

named!(
    line_ending,
    alt!(
        tag!("\r\n") |
        tag!("\n") |
        tag!("\r")
    )
);

named!(
    comment_text,
    map!(
        take_until_parse_s!(
            alt!(
                tag!("#|") |
                tag!("|#")
            )
        ),
        |(a, _)| a
    )
);

named!(
    nested_comment,
    recognize!(do_parse!(
        tag!("#|") >>
        text: comment_text >>
        cont: many0!(comment_cont) >>
        tag!("|#") >>
        ( () )
    ))
);

named!(
    comment_cont,
    recognize!(do_parse!(
        nested: nested_comment >>
        text: comment_text >>
        ( () )
    ))
);

named!(
    comment,
    alt!(
        do_parse!(
            tag!(";") >>
            text: map!(
                take_until_parse_s!(line_ending),
                |(a, b)| a
             ) >>
            ( (text) )
        ) |
        nested_comment |
        do_parse!(
            tag!("#;") >>
            intertoken_space >>
            d: recognize!(datum) >>
            ( d )
        )

    )
);

named!(
    directive,
    alt!(
        tag!("#!fold-case") |
        tag!("#!no-fold-case")
    )
);

named!(
    atmosphere,
    alt!(
        whitespace |
        comment |
        directive
    )
);

named!(
    pub intertoken_space,
    recognize!(many0!(
        atmosphere
    ))
);

// Note that+i,-iand〈infnan〉below are exceptions to the〈peculiar identifier〉rule; they are parsed as numbers, notidentifiers.

named!(
    pub identifier(&[u8]) -> Token,
    map!(
        alt!(
            recognize!(do_parse!(
                initial >>
                x: many0!(subsequent) >>
                ( x )
            )) |
            do_parse!(
                vertical_line >>
                x: recognize!(many0!(symbol_element)) >>
                vertical_line >>
                ( x )
            ) |
            peculiar_identifier
        ),
        |x| Token::Ident(from_u8(x))
    )
);

named!(
    initial,
    alt!(
        letter | special_initial
    )
);

named!(
    letter,
    recognize!(one_of!(&"qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJJKLZXCVBNM"[..]))
);

named!(
    special_initial,
    recognize!(one_of!(&"!$%&*/:<=>?^_~"[..]))
);

named!(
    subsequent,
    alt!(
        initial |
        digit |
        special_subsequent
    )
);

named!(
    pub digit,
    recognize!(one_of!(&"0123456789"[..]))
);

named!(
    pub hex_digit,
    alt!(
        digit |
        recognize!(one_of!(&"abcdef"[..]))
    )
);

named!(
    explicit_sign,
    recognize!(one_of!(&"+-"[..]))
);

named!(
    special_subsequent,
    alt!(
        explicit_sign |
        tag!(".") |
        tag!("@")
    )
);

named!(
    inline_hex_escape,
    do_parse!(
        tag!("\\x") >>
        val: hex_scalar_value >>
        ( val )
    )
);

named!(
    hex_scalar_value,
    recognize!(many0!(hex_digit))
);
named!(
    mnemonic_escape,
    alt!(
        tag!("\\a") => { |_| &b"\x07"[..] } |
        tag!("\\b") => { |_| &b"\x08"[..] }  |
        tag!("\\t") => { |_| &b"\t"[..] } |
        tag!("\\n") => { |_| &b"\n"[..] } |
        tag!("\\r") => { |_| &b"\r"[..] }
    )
);

named!(
    peculiar_identifier,
    alt!(
        explicit_sign |
        recognize!(tuple!(
            explicit_sign,
            sign_subsequent,
            many0!(subsequent)
        )) |
        recognize!(tuple!(
            explicit_sign,
            tag!("."),
            dot_subsequent,
            many0!(subsequent)
        )) |
        recognize!(tuple!(
            tag!("."),
            dot_subsequent,
            many0!(subsequent)
        ))
    )
);

named!(
    dot_subsequent,
    alt!(
        sign_subsequent |
        tag!(".")
    )
);

named!(
    sign_subsequent,
    alt!(
        initial |
        explicit_sign |
        tag!("@")
    )
);

named!(
    symbol_element,
    alt!(
        recognize!(not!(
            alt!(
                tag!("|") |
                tag!("\\")
            )
        )) |
        inline_hex_escape |
        mnemonic_escape |
        tag!("\\|")
    )
);

named!(
    pub boolean(&[u8]) -> Token,
    alt!(
        tag!("#true") => { |_| Token::Bool(true) }  |
        tag!("#false") => { |_| Token::Bool(false) } |
        tag!("#t") => { |_| Token::Bool(true) } |
        tag!("#f") => { |_| Token::Bool(false) }
    )
);

named!(
    pub character(&[u8]) -> Token,
    map!(
        alt!(
            do_parse!(
                tag!("#\\") >>
                char: take!(1) >>
                ( char )
            ) |
            do_parse!(
                tag!("#\\") >>
                char: character_name >>
                ( char )
            ) |
            do_parse!(
                tag!("#\\x") >>
                char: hex_scalar_value >>
                ( char )
            )
        ),
        |x| Token::Char(from_u8(x))
    )
);

named!(
    character_name,
    alt!(
        tag!("alarm") |
        tag!("backspace") |
        tag!("delete") |
        tag!("escape") |
        tag!("newline") |
        tag!("null") |
        tag!("return") |
        tag!("space") |
        tag!("tab")
    )
);

named!(
    pub string(&[u8]) -> Token,
    do_parse!(
        tag!("\"") >>
        strx: many0!(string_element) >>
        tag!("\"") >>
        ( {
            let mut ret = String::new();
            for s in strx {
                 ret += &s;
            }

            Token::Str(ret)
        } )
    )
);

named!(
    string_element(&[u8]) -> String,
    alt!(
        recognize!(none_of!("\"\\")) => { |x| from_u8(x).to_string() } |
        mnemonic_escape => { |x: &[u8]| from_u8(x).to_string() } |
        tag!("\\\"") => { |_| "\"".to_string() } |
        tag!("\\\\") => { |_| "\\".to_string() } |
        do_parse!(
            tag!("\\") >>
            ws1: many0!(intraline_whitespace) >>
            le: line_ending >>
            ws2: many0!(intraline_whitespace) >>
            ( {
                let mut ret = String::new();
                for x in ws1 {
                    ret += &from_u8(x);
                }
                ret += &from_u8(le);

                for x in ws2 {
                    ret += &from_u8(x);
                }

                ret
            } )
        ) |
        // todo convert hex escape to a ASCII character
        inline_hex_escape => { |x: &[u8]| from_u8(x).to_string() }
    )
);


