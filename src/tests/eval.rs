use crate::eval::macros::*;
use crate::parser::datum::Datum;
use crate::parser::util::from_string;
use crate::parser::util::from_file;

#[test]
fn test_eval_syntax_rule_000() {
    assert_eq!(
        eval_syntax_rules(
            Datum::Str("asd".into()),
            Datum::Str("asd".into()),
        ),
        Err(MacrosErr::NotASpec(0))
    );

    assert_eq!(
        eval_syntax_rules(
            Datum::List(vec![Datum::Str("syntax_rules".into())]),
            Datum::Str("asd".into()),
        ),
        Err(MacrosErr::NotASpec(2))
    );

    assert_eq!(
        eval_syntax_rules(
            Datum::List(vec![Datum::Symbol("syntax_rulesa".into())]),
            Datum::Str("asd".into()),
        ),
        Err(MacrosErr::NotASpec(3))
    );
}


#[test]
fn test_eval_syntax_rule_001() {
    let rules = from_file("etc/scheme/my-or.scm").unwrap();

    assert_eq!(
        eval_syntax_rules(
            rules,
            Datum::List(vec![
                Datum::Symbol("my-or".into()),
                Datum::Bool(true),
                Datum::Num("asd".into())
            ])
        ),
        Err(MacrosErr::NotASpec(2))
    );
}

fn load_rule(filename: &str) -> Datum {
    let r=  from_file(filename).unwrap();

    match r.clone() {
        Datum::List(xx) => {
            let (head, tail) = xx.split_first().unwrap();

            match head {
                Datum::Symbol(name) => {
                    if name == "define-syntax" {
                        let (head, tail) = tail.split_first().unwrap();
                        let (head, tail) = tail.split_first().unwrap();

                        return head.clone()
                    } else {
                        return r
                    }
                },
                _ =>  unreachable!()
            }
        },
        _ => unreachable!()
    }
}

#[test]
fn test_eval_syntax_rule_derived_cond() {
    let rules = load_rule("etc/scheme/derived/cond.scm");

    assert_eq!(
        eval_syntax_rules(
            rules,
            Datum::List(vec![
                Datum::Symbol("my-or".into()),
                Datum::Bool(true),
                Datum::Num("asd".into())
            ])
        ),
        Err(MacrosErr::NotASpec(2))
    );
}