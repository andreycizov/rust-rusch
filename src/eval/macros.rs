use crate::parser::datum::Datum;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompileErr {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatternErr(Option<String>, usize);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InterpolateErr {
    Exit,
    Other,
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ListTemplate {
    Single(Template),
    Wildcard(Box<ListTemplate>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Template {
    Symbol(String),
    Const(Datum),
    List(Vec<ListTemplate>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Rules {
    ellipsis: String,
    literals: Vec<String>,
    rules: Vec<(Pattern, Template)>,
}

type PatternMatches = Vec<(Vec<usize>, String, Datum)>;
type PatternVars = HashMap<String, usize>;

impl Template {
    pub fn compile(vars: &PatternVars, ellipsis: &String, template: &Datum) -> Result<Self, CompileErr> {
        use CompileErr::NotASpec;
        use CompileErr::NotASpec2;

        let map_sym = |x: &String| {
            let r = if let Some(_) = vars.get(x) {
                Template::Symbol(x.clone())
            } else {
                Template::Const(Datum::Symbol(x.clone()))
            };

            r
        };

        match template {
            Datum::Symbol(x) => {
                Ok(map_sym(x))
            }
            Datum::List(its) => {
                let mut rits: Vec<ListTemplate> = Vec::new();
                let mut previous: Option<ListTemplate> = None;

                for it in its {
                    match it {
                        Datum::Symbol(s) => {
                            if s == ellipsis {
                                match &previous {
                                    Some(x) => {
                                        previous = Some(ListTemplate::Wildcard(Box::new(x.clone())))
                                    }
                                    None => return Err(NotASpec(301))
                                }
                            } else {
                                // how do we differentiate Template::Symbol from Datum::Symbol here?
                                if let Some(x) = previous.take() {
                                    rits.push(x);
                                }

                                let sym = map_sym(s);

                                previous = Some(ListTemplate::Single(sym))
                            }
                        }
                        xs @ Datum::List(_) => {
                            if let Some(x) = previous.take() {
                                rits.push(x);
                            }

                            previous = Some(ListTemplate::Single(Template::compile(vars, &ellipsis, &xs)?));
                        }
                        other => {
                            if let Some(x) = previous.take() {
                                rits.push(x);
                            }

                            previous = Some(ListTemplate::Single(Template::Const(other.clone())));
                        }
                    };
                }

                if let Some(x) = previous.take() {
                    rits.push(x);
                }

                Ok(Template::List(rits))
            }
            _ => Ok(Template::Const(template.clone()))
        }
    }

    pub fn check(&self, depths: &HashMap<String, usize>, depth: usize) -> Result<(), PatternErr> {
        /// check if the pattern matches all of the variables in the Pattern
        /// check if the depth of the matches is correct, too ?

        match self {
            Template::Symbol(x) => {
                match depths.get(x) {
                    Some(y) => {
                        if *y < depth {
                            return Err(PatternErr(Some(x.clone()), 1001));
                        }
                    }
                    None => {
                        //return Err(PatternErr(Some(x.clone()), 1000))
                    }
                }
            }
            Template::List(xs) => {
                for x in xs {
                    match x {
                        ListTemplate::Single(x) => {
                            x.check(depths, depth)?;
                        }
                        ListTemplate::Wildcard(x) => {
                            let mut curr_item: &ListTemplate = x;
                            let last_depth = depth + 1;

                            let last_item = loop {
                                match curr_item {
                                    ListTemplate::Single(y) => {
                                        break y;
                                    }
                                    ListTemplate::Wildcard(new_subitem) => {
                                        curr_item = new_subitem;
                                    }
                                }
                            };

                            last_item.check(depths, last_depth);
                        }
                    }
                }
            }
            Template::Const(_) => {}
        };

        Ok(())
    }

    pub fn interpolate(
        &self,
        matches: &PatternMatches,
        iteration: &Vec<usize>,
    ) -> Result<Datum, InterpolateErr> {
        match self {
            Template::Const(x) => Ok(x.clone()),
            Template::Symbol(x) => {
                for (iters, name, val) in matches {
                    if name == x {
                        if iters[..] == iteration[..iters.len()] {
                            return Ok(val.clone());
                        }
                    }
                }
                // we need to find all matches of a given variable that match a given iteration and name
                //matches.

                return Err(InterpolateErr::Exit);
            }
            Template::List(xs) => {
                let mut r = Vec::<Datum>::with_capacity(xs.len());

                for x in xs {
                    r.extend_from_slice(&(x.interpolate(matches, iteration)?)[..]);
                }

                Ok(Datum::List(r))
            }
        }
    }
}

impl ListTemplate {
    pub fn interpolate(
        &self,
        matches: &PatternMatches,
        iteration: &Vec<usize>,
    ) -> Result<Vec<Datum>, InterpolateErr> {
        let mut r = Vec::<Datum>::with_capacity(10);

        match self {
            ListTemplate::Single(template) => {
                let r2 = template.interpolate(matches, iteration)?;
                r.push(r2);
            }
            ListTemplate::Wildcard(subtemplate) => {
                let mut r2 = Vec::<usize>::with_capacity(iteration.len() + 1);

                r2.extend_from_slice(&iteration[..]);

                for i in 0.. {
                    r2.push(i);

                    match subtemplate.interpolate(matches, &r2) {
                        Ok(x) => r.extend_from_slice(&x[..]),
                        Err(InterpolateErr::Exit) => {
                            // todo check the items with 0 matches ?
                            if i == 0 {
                                return Err(InterpolateErr::Exit);
                            } else {
                                break;
                            }
                        }
                        x @ Err(_) => return x
                    }

                    r2.pop();
                }
            }
        }

        Ok(r)
    }
}

impl Pattern {
    pub fn compile(ellipsis: &String, literals: &Vec<String>, pattern: &Datum) -> Result<Self, CompileErr> {
        use CompileErr::NotASpec;
        use CompileErr::NotASpec2;

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
                                    Pattern::compile(ellipsis, literals, &Datum::Symbol(name.clone()))?
                                )
                            }
                        }
                        x => Some(
                            Pattern::compile(ellipsis, literals, x)?
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

    pub fn vars(&self) -> Result<PatternVars, PatternErr> {
        // return a list of variables matched by the pattern and their depths.

        let mut r = HashMap::<String, usize>::default();

        match self {
            Pattern::Symbol(x) => {
                r.insert(x.clone(), 0);
            }
            Pattern::List(xs) => {
                for x in xs {
                    match x {
                        ListPattern::Single(x) => {
                            for (k, v) in x.vars()?.iter() {
                                if r.contains_key(k) {
                                    return Err(PatternErr(Some(k.clone()), 1));
                                }

                                r.insert(k.clone(), v.clone());
                            }
                        }
                        ListPattern::Wildcard(x) => {
                            for (k, v) in x.vars()?.iter() {
                                if r.contains_key(k) {
                                    return Err(PatternErr(Some(k.clone()), 2));
                                }

                                r.insert(k.clone(), v + 1);
                            }
                        }
                    }
                }
            }
            _ => {}
        };

        Ok(r)
    }

    pub fn cross(&self, ellipsis: &String, datum: &Datum) -> Option<PatternMatches> {
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
            Pattern::Symbol(x) => { Some(vec![(vec![], x.clone(), datum.clone())]) }
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
                        let mut curr = ys.next();

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
    pub fn compile(transformer_spec: &Datum) -> Result<Self, CompileErr> {
        use CompileErr::NotASpec;
        use CompileErr::NotASpec2;

        match transformer_spec {
            Datum::List(list) => {
                let (first, list) = list.split_first().ok_or(NotASpec(1))?;

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
                    let r: Result<Vec<&String>, CompileErr> = literals_items.iter().enumerate().map(|(i, x)| {
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

                let rules: Result<Vec<(Pattern, Template)>, CompileErr> = rules.iter().enumerate().map(|(i, rule)| {
                    if let Datum::List(rule_items) = rule {
                        let (pattern, rule_items) = rule_items.split_first().ok_or(NotASpec2(9, i))?;
                        let (template, rule_items) = rule_items.split_first().ok_or(NotASpec2(10, i))?;


                        if rule_items.len() != 0 {
                            return Err(NotASpec2(11, i));
                        }

                        // check patterns here.

                        let pattern = Pattern::compile(&ellipsis, &literals, &pattern).map_err(|x| match x {
                            NotASpec(x) => NotASpec2(x, i),
                            z => z
                        })?;

                        let vars = pattern.vars().map_err(|_| NotASpec2(999999, i))?;

                        let template = Template::compile(&vars, &ellipsis, &template).map_err(|x| match x {
                            NotASpec(x) => NotASpec2(x, i),
                            z => z
                        })?;

                        // template must be compiled, too.
                        // because template is an expansion.
                        // and if we compile it we then only need to take the variables somehow.

                        Ok((pattern, template))
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

    pub fn interpolate(&self, datum: &Datum) -> Result<Datum, InterpolateErr> {
        // (a ...)
        // ((b ...) ...)
        //

        /// case-matches against the patterns defined by the macro.

        for (pattern, body) in &self.rules {
            // todo we need to cross the template too.

            //let body_pat = Pattern::new(&self.ellipsis, &self.literals, datum);

            match pattern.cross(&self.ellipsis, datum) {
                Some(x) => {
                    return body.interpolate(&x, &vec![]);
                }
                None => continue
            }
        }

        Err(InterpolateErr::Exit)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvalErr {
    Compile(CompileErr),
    Interpolate(InterpolateErr),
}

impl From<CompileErr> for EvalErr {
    fn from(x: CompileErr) -> Self {
        EvalErr::Compile(x)
    }
}

impl From<InterpolateErr> for EvalErr {
    fn from(x: InterpolateErr) -> Self {
        EvalErr::Interpolate(x)
    }
}

pub fn eval_syntax_rules(transformer_spec: &Datum, code: &Datum) -> Result<Datum, EvalErr> {
    use CompileErr::*;

    /// evaluate a single macros against a block of code
    ///
    /// a code is a call to a macros like (ma 2 3 5)

    let sr = Rules::compile(&transformer_spec)?;
    let sr2 = sr.interpolate(&code)?;

    Ok(sr2)
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