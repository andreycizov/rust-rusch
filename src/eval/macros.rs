use crate::parser::datum::Datum;

#[derive(Debug, PartialEq, Eq)]
pub enum MacrosErr {
    NotASpec(usize),
    NotASpec2(usize, usize),
    NotACall(usize),
}


#[derive(Debug, PartialEq, Eq, Clone)]
enum ListPattern {
    Single(Pattern),
    Wildcard(Pattern),

    // if P is an improper list, we need to match the remains of the expression as a syntaxpattern
    //Remaining(SyntaxPattern),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Pattern {
    Underscore,
    Literal(String),
    Symbol(String),
    Const(Datum),
    List(Vec<ListPattern>),
}

struct Rules {
    ellipsis: String,
    literals: Vec<String>,
    rules: Vec<(Pattern, Datum)>,
}

impl Pattern {
    pub fn new(ellipsis: &String, literals: &Vec<String>, pattern: &Datum) -> Result<Self, MacrosErr> {
        use MacrosErr::NotASpec;
        use MacrosErr::NotASpec2;

        match pattern {
            Datum::Symbol(x) => {
                if x == &"_".to_string() {
                    Ok(Pattern::Underscore)
                } else if literals.contains(&&x) {
                    Ok(Pattern::Literal(x.clone()))
                } else {
                    Ok(Pattern::Symbol(x.clone()))
                }
            }
            Datum::List(its) => {
                let mut rits: Vec<ListPattern> = Vec::new();
                let mut previous: Option<Pattern> = None;

                for it in its {
                    let next = match it {
                        Datum::Symbol(name) => {
                            if name == ellipsis {
                                match &previous {
                                    Some(x) => None,
                                    None => return Err(NotASpec(209))
                                }
                            } else {
                                Some(
                                    Pattern::new(ellipsis, literals, &Datum::Symbol(name.clone()))?
                                )
                            }
                        }
                        x => Some(
                            Pattern::new(ellipsis, literals, x)?
                        )
                    };


                    if let Some(next) = next {
                        if let Some(previous) = previous {
                            rits.push(ListPattern::Single(previous));
                        }

                        previous = Some(next);
                    } else if let Some(previous2) = previous {
                        rits.push(ListPattern::Wildcard(previous2));
                        previous = None
                    }
                }

                if let Some(previous) = previous {
                    rits.push(ListPattern::Single(previous));
                }

                Ok(Pattern::List(rits))
            }
            _ => Ok(Pattern::Const(pattern.clone()))
        }
    }

    pub fn cross(&self, ellipsis: &String, datum: &Datum) -> Option<Vec<(Vec<usize>, String, Datum)>> {
        use Pattern::*;
        match self {
            Pattern::Underscore => Some(vec![]),
            Pattern::Literal(x) => match datum {
                Datum::Symbol(y) => {
                    if x == y {
                        Some(vec![])
                    } else {
                        None
                    }
                }
                _ => None
            }
            Pattern::Symbol(x) => { Some(vec![(vec![], x.clone(), datum.clone())]) },
            Pattern::Const(x) => if x == datum {
                Some(vec![])
            } else {
                None
            }
            Pattern::List(xs) => {
               match datum {
                   Datum::List(ys) => {
                       let capa = ys.len() + 5;
                       let mut matches: Vec<(Vec<usize>, String, Datum)> = Vec::with_capacity(capa);

                       let mut ys = ys.iter();
                       let mut curr= ys.next();

                       for lp in xs {
                           match lp {
                               ListPattern::Single(subpat) => match curr {
                                   Some(curr_some) => match subpat.cross(ellipsis, &curr_some) {
                                       Some(ms) => {
                                           matches.extend(ms);

                                           curr = ys.next();
                                       }
                                       None => return None
                                   }
                                   None => return None
                               }
                               ListPattern::Wildcard(subpat) => {
                                   // todo wildcard patterns will not properly match items
                                   // todo of the same type coming after it.

                                   let mut wi = 0;

                                   loop {
                                       match curr {
                                           Some(x) => match subpat.cross(ellipsis, x) {
                                               Some(ms) => {
                                                   for (mx, my, mz) in ms {
                                                       let mut nmx = Vec::with_capacity(mx.len() + 1);
                                                       nmx.push(wi);
                                                       nmx.extend(mx);

                                                       matches.push((nmx, my, mz));
                                                   }
                                                   curr = ys.next();
                                               }
                                               None => break
                                           }
                                           None => break
                                       }

                                       wi += 1;
                                   }
                               }
                           }
                       }

                       match curr {
                           None => Some(matches),
                           Some(_) => None
                       }
                   }
                   _ => None
               }
            }
        }
    }
}

impl Rules {
    pub fn new(transformer_spec: &Datum) -> Result<Self, MacrosErr> {
        use MacrosErr::NotASpec;
        use MacrosErr::NotASpec2;

        match transformer_spec {
            Datum::List(list) => {
                let (first, list) = list.split_first().ok_or(NotASpec(1))?;

                eprintln!("{:?} {:?}", first, list);

                if let Datum::Symbol(ident) = first {
                    if ident != "syntax-rules" {
                        return Err(NotASpec(3));
                    }
                } else {
                    return Err(NotASpec(2));
                }

                let (second, list) = list.split_first().ok_or(NotASpec(4))?;

                let (ellipsis, literals, list) = if let Datum::Symbol(ident) = second {
                    let ellipsis = ident.clone();

                    let (third, list) = list.split_first().ok_or(NotASpec(5))?;

                    (ellipsis, third, list)
                } else {
                    ("...".to_string(), second, list)
                };

                let literals = if let Datum::List(literals_items) = literals {
                    let r: Result<Vec<&String>, MacrosErr> = literals_items.iter().enumerate().map(|(i, x)| {
                        if let Datum::Symbol(x) = x {
                            Ok(x)
                        } else {
                            Err(NotASpec2(7, i))
                        }
                    }).collect();

                    r?
                } else {
                    return Err(NotASpec(6));
                };

                let literals: Vec<String> = literals.iter().map(|x| (*x).clone()).collect();

                // all non-rule parameters are recognized correctly.

                let rules = list;

                let rules: Result<Vec<(Pattern, Datum)>, MacrosErr> = rules.iter().enumerate().map(|(i, rule)| {
                    if let Datum::List(rule_items) = rule {
                        let (pattern, rule_items) = rule_items.split_first().ok_or(NotASpec2(9, i))?;
                        let (template, rule_items) = rule_items.split_first().ok_or(NotASpec2(10, i))?;


                        if rule_items.len() != 0 {
                            return Err(NotASpec2(11, i));
                        }

                        // check patterns here.

                        let pattern = Pattern::new(&ellipsis, &literals, &pattern).map_err(|x| match x {
                            NotASpec(x) => NotASpec2(x, i),
                            z => z
                        })?;

                        // template must be compiled, too.
                        // because template is an expansion.
                        // and if we compile it we then only need to take the variables somehow.

                        Ok((pattern, template.clone()))
                    } else {
                        return Err(NotASpec2(8, i));
                    }
                }).collect();

                let rules = rules?;


                Ok(Rules { ellipsis, literals, rules })
            }
            _ => {
                Err(NotASpec(0))
            }
        }
    }

//    fn interpolate(&self, vars: Vec<(String, Datum)>, body: &Datum) -> Datum {
//        // interpolate a given body with the given variables.
//    }

    pub fn apply(&self, datum: &Datum) -> Option<Vec<(Vec<usize>, String, Datum)>> {
        // (a ...)
        // ((b ...) ...)
        //

        /// case-matches against the patterns defined by the macro.

        for (pattern, body) in &self.rules {
            // todo we need to cross the template too.

            let body_pat = Pattern::new(&self.ellipsis, &self.literals, body);


            eprintln!("{:?}", pattern);
            eprintln!("{:?}", body);
            eprintln!("{:?}", body_pat);
            eprintln!("__________________________________________________");
            // todo spread backs in and out
            match pattern.cross(&self.ellipsis, datum) {
                Some(x) => return Some(x),
                None => continue
            }
        }

        None
    }
}

pub fn eval_syntax_rules(transformer_spec: Datum, code: Datum) -> Result<Datum, MacrosErr> {
    use MacrosErr::*;

    /// evaluate a single macros against a block of code
    ///
    /// a code is a call to a macros like (ma 2 3 5)

    let sr = Rules::new(&transformer_spec)?;

    eprintln!("{:?}", sr.apply(&code));

    Ok(Datum::List(Vec::<Datum>::new()))
}

fn match_syntax_rule_pattern(literals: &Vec<String>, pattern: &Datum, expression: &Datum) -> (bool, Vec<(String, Datum)>) {
//    match pattern {
//        Datum::Symbol(x) => {
//            if x == &"_".to_string() {
//                (true, vec![])
//            } else if literals.contains(&&x) {
//                (true, vec![]) // match, no binding
//            } else {
//                (true, vec![(x.clone(), expression.clone())]) // match, binding
//            }
//        }
//        Datum::List(x) => {
//            match expression {
//                Datum::List(y) => {
//                    let mut bindings: Vec<(String, Datum)> = Vec::new();
//
//                    /// todo we need to have a lookahead thing here in order to match them correctly.
//                }
//                _ => (false, vec![])
//            }
//        }
//        _ => (false, vec![])
//
//    }

    (false, vec![])
}