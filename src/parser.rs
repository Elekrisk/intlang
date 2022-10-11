use crate::ast::*;
use num_bigint::BigInt;

fn bin<'a>(
    l: Expression<'a>,
    a: usize,
    op: BinaryOp,
    b: usize,
    r: Expression<'a>,
) -> Expression<'a> {
    Expression::new(ExpressionKind::Binary {
        left: Box::new(l),
        op: op.with_loc((a, b)),
        right: Box::new(r),
    })
}

peg::parser! {
    pub grammar expr_parser() for str {
        rule p() -> usize = position!()

        rule _() -> ()
        = (
              c:[_] {? if c.is_whitespace() { Ok(()) } else {Err("")} }
            / "#" [^ '\n']*
        )* {}

        rule key(k: &'static str) -> ()
        = i:ident() {? if i == k { Ok(()) } else { Err(k) }}

        rule number() -> BigInt
        = n:$("-"? ['0'..='9']+) { n.parse().unwrap() }

        rule ident_start() -> char
        = c:[_] {?
            if c.is_alphabetic() || c == '_' {
                Ok(c)
            } else {
                Err("identifier start")
            }
        }

        rule ident_cont() -> char
        = c:[_] {?
            if c.is_alphanumeric() || c == '_' {
                Ok(c)
            } else {
                Err("identifier cont")
            }
        }

        rule ident() -> &'input str
        = s:$(ident_start() ident_cont()*)

        rule arithmetic() -> Expression<'input>
        = e:precedence! {
            x:(@) _ a:p() "+" b:p() _ y:@ { bin(x, a, BinaryOp::Add, b, y) }
            x:(@) _ a:p() "-" b:p() _ y:@ { bin(x, a, BinaryOp::Sub, b, y) }
            --
            x:(@) _ a:p() "*" b:p() _ y:@ { bin(x, a, BinaryOp::Mul, b, y) }
            x:(@) _ a:p() "/" b:p() _ y:@ { bin(x, a, BinaryOp::Div, b, y) }
            x:(@) _ a:p() "%" b:p() _ y:@ { bin(x, a, BinaryOp::Mod, b, y) }
            --
            a:p() "-" z:p() e:@ { Expression::new(ExpressionKind::Unary { op: UnaryOp::Neg.with_loc((a, z)), expr: Box::new(e) }) }
            --
            f:@ _ "[" _ args:expr()**_ _ a:p() "]" z:p() { Expression::new(ExpressionKind::FuncCall {func:Box::new(f),args, rbracket: ().with_loc((a, z)) }) }
            --
            ifa:p() key("if") ifb:p() cond:expr() key("then") case_true:expr() key("else") case_false:expr() enda:p() key("end") endb:p() {
                Expression::new(ExpressionKind::If {
                    kw_if: ().with_loc((ifa, ifb)),
                    cond: Box::new(cond),
                    case_true: Box::new(case_true),
                    case_false: Box::new(case_false),
                    kw_end: ().with_loc((enda, endb))
                })
            }
            a:p() key("nil") z:p() { Expression::new(ExpressionKind::Nil(().with_loc((a, z)))) }
            a:p() n:number() z:p() { Expression::new(ExpressionKind::Literal(n.with_loc((a, z)))) }
            a:p() i:ident() z:p() { Expression::new(ExpressionKind::Variable(i.with_loc((a, z)))) }
            ba:p() "\\" bz:p() _ params:(a:p() i:ident() z:p() {i.with_loc((a, z))})**_ _ aa:p() ("->"/"→") az:p() _ body:expr() {
                Expression::new(ExpressionKind::Lambda {
                    backslash: ().with_loc((ba, bz)),
                    params,
                    arrow: ().with_loc((aa, az)),
                    body: Box::new(body)
                })
            }
            "(" _ e:expr() _ ")" { e }
        }

        rule expr_inner() -> Expression<'input>
        = a:arithmetic()++(_ "," _) { if a.len() == 1 { let mut a = a; a.remove(0) } else { Expression::new(ExpressionKind::Sequence(a)) } }

        rule where_clause() -> WhereClause<'input>
        = a:p() key("where") z:p() s:(_ s:stmnt() {s})* { WhereClause::new(().with_loc((a, z)), s) }

        rule stmnt() -> Statement<'input>
        = a:p() name:ident() _ ea:p() "=" ez:p() _ expr:expr() _ ";" z:p() {
            Statement::new(name.with_loc(Location::new(a, z)), ().with_loc((ea, ez)), expr)
        }

        pub rule expr() -> Expression<'input>
        = _ e:expr_inner() _ w:where_clause()? _ {
            if let Some(w) = w {
                e.with_where_clause(w)
            } else {
                e
            }
        }
    }
}
