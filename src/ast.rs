use std::{
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
    sync::atomic::{AtomicUsize, Ordering},
};

use miette::SourceSpan;
use num_bigint::BigInt;

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn to(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl From<Location> for SourceSpan {
    fn from(value: Location) -> Self {
        Self::new(value.start.into(), (value.end - value.start).into())
    }
}

impl From<(usize, usize)> for Location {
    fn from(value: (usize, usize)) -> Self {
        Self {
            start: value.0,
            end: value.1,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct T<V> {
    pub id: Id<()>,
    pub loc: Location,
    pub value: V,
}

impl<V> T<V> {
    pub fn new(loc: Location, value: V) -> Self {
        Self { id: gen_id(), loc, value }
    }

    pub fn map<U>(self, f: impl FnOnce(V) -> U) -> T<U> {
        T {
            id: self.id,
            loc: self.loc,
            value: f(self.value),
        }
    }
}

pub trait WithLoc: Sized {
    fn with_loc(self, loc: impl Into<Location>) -> T<Self> {
        T {
            id: gen_id(),
            loc: loc.into(),
            value: self,
        }
    }
}

impl<T> WithLoc for T {}

pub trait Loc {
    fn loc(&self) -> Location;
}

#[derive(Debug)]
pub struct Expression<'a> {
    pub id: Id<Self>,
    pub kind: ExpressionKind<'a>,
    pub where_clause: Option<WhereClause<'a>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct VariableId(pub usize);

#[derive(Debug, Clone, Copy)]
pub enum Variable<'a> {
    ByName(&'a str),
    ById(VariableId),
}

impl<'a> Variable<'a> {
    pub fn try_into_by_name(self) -> Result<&'a str, Self> {
        if let Self::ByName(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

#[derive(Debug)]
pub enum ExpressionKind<'a> {
    Nil(T<()>),
    Literal(T<BigInt>),
    Variable(T<&'a str>),
    Lambda {
        backslash: T<()>,
        params: Vec<T<&'a str>>,
        arrow: T<()>,
        body: Box<Expression<'a>>,
    },
    Unary {
        op: T<UnaryOp>,
        expr: Box<Expression<'a>>,
    },
    Binary {
        left: Box<Expression<'a>>,
        op: T<BinaryOp>,
        right: Box<Expression<'a>>,
    },
    FuncCall {
        func: Box<Expression<'a>>,
        args: Vec<Expression<'a>>,
        rbracket: T<()>,
    },
    If {
        kw_if: T<()>,
        cond: Box<Expression<'a>>,
        case_true: Box<Expression<'a>>,
        case_false: Box<Expression<'a>>,
        kw_end: T<()>,
    },
    Sequence(Vec<Expression<'a>>),
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub struct WhereClause<'a> {
    pub kw_where: T<()>,
    pub stmnts: Vec<Statement<'a>>,
}

#[derive(Debug)]
pub struct Statement<'a> {
    pub id: Id<Self>,
    pub name: T<&'a str>,
    pub equals_sign: T<()>,
    pub expr: Expression<'a>,
}

impl<'a> Expression<'a> {
    pub fn new(kind: ExpressionKind<'a>) -> Self {
        Self {
            id: gen_id(),
            kind,
            where_clause: None,
        }
    }

    pub fn with_where_clause(self, clause: WhereClause<'a>) -> Self {
        Self {
            where_clause: Some(clause),
            ..self
        }
    }
}

impl<'a> Loc for Expression<'a> {
    fn loc(&self) -> Location {
        match &self.kind {
            ExpressionKind::Nil(t) => t.loc,
            ExpressionKind::Literal(v) => v.loc,
            ExpressionKind::Variable(v) => v.loc,
            ExpressionKind::Unary { op, expr } => op.loc.to(expr.loc()),
            ExpressionKind::Binary { left, right, .. } => left.loc().to(right.loc()),
            ExpressionKind::Lambda {
                body, backslash, ..
            } => backslash.loc.to(body.loc()),
            ExpressionKind::FuncCall { func, rbracket, .. } => func.loc().to(rbracket.loc),
            ExpressionKind::If { kw_if, kw_end, .. } => kw_if.loc.to(kw_end.loc),
            ExpressionKind::Sequence(v) => v.first().unwrap().loc().to(v.last().unwrap().loc()),
        }
    }
}

impl<'a> Statement<'a> {
    pub fn new(name: T<&'a str>, equals_sign: T<()>, expr: Expression<'a>) -> Self {
        Self {
            id: gen_id(),
            name,
            equals_sign,
            expr
        }
    }
}

impl<'a> Loc for Statement<'a> {
    fn loc(&self) -> Location {
        self.name.loc.to(self.expr.loc())
    }
}

impl<'a> WhereClause<'a> {
    pub fn new(kw_where: T<()>, stmnts: Vec<Statement<'a>>) -> Self {
        Self { kw_where, stmnts }
    }
}

pub struct Id<T> {
    value: usize,
    _marker: PhantomData<T>,
}

impl <T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("value").field(&self.value).finish()
    }
}

impl<T> Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            _marker: self._marker.clone(),
        }
    }
}

impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
        self._marker.hash(state);
    }
}

impl<T> Id<T> {
    pub fn new(value: usize) -> Self {
        Self {
            value,
            _marker: PhantomData,
        }
    }
}

fn gen_id<T>() -> Id<T> {
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    Id::new(NEXT.fetch_add(1, Ordering::Relaxed))
}
