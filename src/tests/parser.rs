use crate::parser::root::*;
use crate::parser::datum::*;

#[test]
fn test_simple() {
    use Token::*;

    assert_eq!(
        tokens(b"(lambda (x a) (a 2345.67))   a\0"),
        Ok((&b""[..], vec![
            LBr,
            Ident("lambda".into()),
            LBr,
            Ident("x".into()),
            Ident("a".into()),
            RBr,
            LBr,
            Ident("a".into()),
            Num("2345.67".into()),
            RBr,
            RBr,
            Ident("a".into())
        ]))
    );

    assert_eq!(
        string(b"\"\""),
        Ok((&b""[..], Token::Str("".into())))
    );

    assert_eq!(
        string(b"\"a\""),
        Ok((&b""[..], Token::Str("a".into())))
    );

    assert_eq!(
        string(b"\"a\\\"bc\""),
        Ok((&b""[..], Token::Str("a\"bc".into())))
    );

    assert_eq!(
        string(b"\"a  \\\n a asd \""),
        Ok((&b""[..], Token::Str("a  \n a asd ".into())))
    );
}

#[test]
fn test_datum_bool() {
    assert_eq!(
        datum(b"#true\0"),
        Ok((&b"\0"[..], Datum::Bool(true)))
    );

    assert_eq!(
        datum(b"#t\0"),
        Ok((&b"\0"[..], Datum::Bool(true)))
    );

    assert_eq!(
        datum(b"#false\0"),
        Ok((&b"\0"[..], Datum::Bool(false)))
    );

    assert_eq!(
        datum(b"#f\0"),
        Ok((&b"\0"[..], Datum::Bool(false)))
    );
}

#[test]
fn test_datum_lambda() {
    assert_eq!(
        datum(b"a\0"),
        Ok((&b"\0"[..], Datum::Symbol("a".into())))
    );

    assert_eq!(
        datum(b"()\0"),
        Ok((&b"\0"[..], Datum::List(vec![])))
    );

    assert_eq!(
        datum(b"\"aasd\""),
        Ok((&b""[..], Datum::Str("aasd".into())))
    );

    assert_eq!(
        datum(b"#f\0"),
        Ok((&b"\0"[..], Datum::Bool(false)))
    );

    assert_eq!(
        datum(b"(a)"),
        Ok((&b""[..], Datum::List(vec![Datum::Symbol("a".into())])))
    );

    assert_eq!(
        datum(b"(a b)"),
        Ok((&b""[..], Datum::List(vec![Datum::Symbol("a".into()), Datum::Symbol("b".into())])))
    );

    assert_eq!(
        datum(b"(+ \"bc\" pen)"),
        Ok(
            (&b""[..],
             Datum::List(vec![
                 Datum::Symbol("+".into()),
                 Datum::Str("bc".into()),
                 Datum::Symbol("pen".into())
             ]))
        )
    );

    assert_eq!(
        datum(b",(+ \"bc\" pen)"),
        Ok(
            (&b""[..],
             Datum::List(
                 vec![
                     Datum::Symbol("unquote".into()),
                     Datum::List(vec![
                         Datum::Symbol("+".into()),
                         Datum::Str("bc".into()),
                         Datum::Symbol("pen".into())
                     ])
                 ]
             )
            )
        )
    );

    assert_eq!(
        datum(b"(...)"),
        Ok(
            (&b""[..],
             Datum::List(vec![
                 Datum::Symbol("...".into()),
             ]))
        )
    );
}