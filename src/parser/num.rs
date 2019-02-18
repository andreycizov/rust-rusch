use nom::{named, tag, alt, call, map, tuple, many0, many1, one_of, recognize};

use super::root::*;

// initial defs

named!(
   pub number(&[u8]) -> Token,
   alt!(
        //num_2 |
        //num_8 |
        num_10 //|
        //num_16
   )
);


named!(
   pub uinteger,
   alt!(
        //uinteger_2 |
        //uinteger_8 |
        uinteger_10 //|
        //uinteger_16
   )
);


// root defs

named!(
    infnan,
    alt!(
        tag!("+inf.0") |
        tag!("-inf.0") |
        tag!("+nan.0") |
        tag!("-nan.0")
    )
);

named!(
    suffix,
    alt!(
        recognize!(tuple!(
            exponent_marker,
            sign,
            many1!(digit_10)
        )) |
        tag!("")
   )
);

named!(
   exponent_marker,
   tag!("e")
);

named!(
   sign,
   alt!(
        tag!("+") |
        tag!("-") |
        tag!("")
   )
);

named!(
    exactness,
    alt!(
        tag!("#i") |
        tag!("#e") |
        tag!("")
    )
);

named!(
    radix_2,
    tag!("#b")
);

named!(
    radix_8,
    tag!("#o")
);

named!(
    radix_10,
    alt!(
        tag!("#d") |
        tag!("")
    )
);

named!(
    radix_16,
    tag!("#x")
);

named!(
    digit_2,
    recognize!(one_of!(&"01"[..]))
);

named!(
    digit_8,
    recognize!(one_of!(&"01234567"[..]))
);


named!(
    digit_10,
    call!(digit)
);

named!(
    digit_16,
    call!(hex_digit)
);

//

named!(
    num_10(&[u8]) -> Token,
    map!(
        recognize!(tuple!(
            prefix_10,
            complex_10
        )),
        |x| Token::Num(from_u8(x))
    )
);

named!(
    complex_10,
    alt!(
        real_10 |
        recognize!(tuple!(
            real_10,
            tag!("@"),
            real_10
        )) |
        recognize!(tuple!(
            real_10,
            tag!("+"),
            ureal_10,
            tag!("i")
        )) |
        recognize!(tuple!(
            real_10,
            tag!("-"),
            ureal_10,
            tag!("i")
        )) |
        recognize!(tuple!(
            real_10,
            tag!("+"),
            tag!("i")
        )) |
        recognize!(tuple!(
            real_10,
            tag!("-"),
            tag!("i")
        )) |
        recognize!(tuple!(
            real_10,
            infnan,
            tag!("i")
        )) |
        recognize!(tuple!(
            tag!("+"),
            ureal_10,
            tag!("i")
        )) |
        recognize!(tuple!(
            tag!("-"),
            ureal_10,
            tag!("i")
        )) |
        recognize!(tuple!(
            infnan,
            tag!("i")
        )) |
        recognize!(tuple!(
            tag!("+"),
            tag!("i")
        )) |
        recognize!(tuple!(
            tag!("-"),
            tag!("i")
        ))
    )
);


named!(
    real_10,
    alt!(
        recognize!(tuple!(
            sign,
            ureal_10
        )) |
        infnan
    )
);

named!(
    ureal_10,
    alt!(
        recognize!(tuple!(
            uinteger_10,
            tag!("/"),
            uinteger_10
        )) |
        // [1]  decimals are only defined for 10?
        decimal_10 |
        uinteger_10
    )
);

named!(
    pub uinteger_10,
    recognize!(many1!(digit_10))
);

named!(
    prefix_10,
    alt!(
        recognize!(tuple!(
            radix_10,
            exactness
        )) |
        recognize!(tuple!(
            exactness,
            radix_10
        ))
    )
);

// do not redefine for others and remove [1]

named!(
    decimal_10,
    alt!(
        recognize!(tuple!(
            many1!(digit_10),
            tag!("."),
            many0!(digit_10),
            suffix
        )) |
        recognize!(tuple!(
            tag!("."),
            many1!(digit_10),
            suffix
        )) |
        recognize!(tuple!(
            uinteger_10,
            suffix
        ))
    )
);