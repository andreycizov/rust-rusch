use crate::eval::macros::*;
use crate::parser::datum::Datum;
use crate::parser::util::from_string;
use crate::parser::util::from_file;

#[test]
fn test_eval_syntax_rule_000() {
    assert_eq!(
        eval_syntax_rules(
            &Datum::Str("asd".into()),
            &Datum::Str("asd".into()),
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(0)))
    );

    assert_eq!(
        eval_syntax_rules(
            &Datum::List(vec![Datum::Str("syntax_rules".into())]),
            &Datum::Str("asd".into()),
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(2)))
    );

    assert_eq!(
        eval_syntax_rules(
            &Datum::List(vec![Datum::Symbol("syntax_rulesa".into())]),
            &Datum::Str("asd".into()),
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(3)))
    );
}


#[test]
fn test_eval_syntax_rule_001() {
    let rules = load_rule("etc/scheme/my-or.scm");

    assert_eq!(
        eval_syntax_rules(
            &rules,
            &Datum::List(vec![
                Datum::Symbol("or".into()),
                Datum::Bool(true),
                Datum::Num("asd0".into()),
                Datum::Num("asd1".into()),
                Datum::Num("asd2".into()),
            ])
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(2)))
    );
}

#[test]
fn test_eval_syntax_rule_002() {
    let rules = load_rule("etc/scheme/my-subpat.scm");

    assert_eq!(
        eval_syntax_rules(
            &rules,
            &Datum::List(vec![
                Datum::Symbol("my-or".into()),
                Datum::List(vec![
                    Datum::Num("a1".into()),
                    Datum::Num("a2".into()),
                    Datum::Num("a3".into()),
                ]),
                Datum::List(vec![
                    Datum::Num("b1".into()),
                    Datum::Num("b2".into()),
                    Datum::Num("b3".into()),
                ]),
                Datum::List(vec![
                    Datum::Num("c1".into()),
                    Datum::Num("c2".into()),
                    Datum::Num("c3".into()),
                ]),
            ])
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(2)))
    );
}

#[test]
fn test_eval_syntax_rule_003() {
    let rules = load_rule("etc/scheme/tests/cross.scm");
    let ai = from_file("etc/scheme/tests/a-in.scm").unwrap();
    let ao = from_file("etc/scheme/tests/a-out.scm").unwrap();
    let bi = from_file("etc/scheme/tests/b-in.scm").unwrap();
    let bo = from_file("etc/scheme/tests/b-out.scm").unwrap();

    assert_eq!(
        Ok(ao),
        eval_syntax_rules(
            &rules,
            &ai
        )
    );

    assert_eq!(
        Ok(bo),
        eval_syntax_rules(
            &rules,
            &bi
        )
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
            &rules,
            &Datum::List(vec![
                Datum::Symbol("my-or".into()),
                Datum::Bool(true),
                Datum::Num("asd".into())
            ])
        ),
        Err(EvalErr::Compile(CompileErr::NotASpec(2)))
    );
}