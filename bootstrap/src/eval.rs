use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use ahash::AHashMap;

use crate::id::Id;
use crate::syntax::Expr;


pub type ExtFn = dyn Fn(&'_ [Expr], Rc<Scope>) -> Value;

#[derive(Clone)]
pub enum Value {
	String(Id),
	Number(f64),
	Bool(bool),
	Nul,
	List(Rc<RefCell<Vec<Value>>>),
	Map(Rc<RefCell<AHashMap<Value, Value>>>),
	Function(Rc<(Rc<Scope>, Vec<Id>, Vec<Expr>)>),
	ExtFn(Rc<ExtFn>),
}

impl std::hash::Hash for Value {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
		match self {
			Value::String(v) => v.hash(state),
			Value::Number(v) => v.to_ne_bytes().hash(state),
			Value::Bool(v) => v.hash(state),
			Value::Nul => {},
			Value::List(v) => Rc::as_ptr(v).hash(state),
			Value::Map(v) => Rc::as_ptr(v).hash(state),
			Value::Function(v) => Rc::as_ptr(v).hash(state),
			Value::ExtFn(v) => Rc::as_ptr(v).hash(state),
		}
	}
}
impl fmt::Debug for Value {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			Self::String(v) => write!(f, "{v:?}"),
			Self::Number(v) => write!(f, "{v}"),
			Self::Bool(v) => write!(f, "{v}"),
			Self::Nul => write!(f, "nul"),
			Self::List(v) => write!(f, "{v:?}"),
			Self::Map(v) => write!(f, "{v:?}"),
			Self::Function(data) => write!(f, "<fn {data:p}>"),
			Self::ExtFn(v) => write!(f, "<fn! {v:p}>"),
		}
	}
}
impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::String(l), Self::String(r)) => l == r,
			(Self::Number(l), Self::Number(r)) => l == r,
			(Self::Bool(l), Self::Bool(r)) => l == r,
			(Self::List(l), Self::List(r)) => Rc::ptr_eq(l, r),
			(Self::Map(l), Self::Map(r)) => Rc::ptr_eq(l, r),
			(Self::Function(l), Self::Function(r)) => Rc::ptr_eq(l, r),
			// clippy says no ptr eq on dyn, i don't care
			(Self::ExtFn(l), Self::ExtFn(r)) => Rc::ptr_eq(l, r),
			_ => false,
		}
	}
}

#[derive(Clone, Debug)]
pub struct Scope {
	vars: RefCell<AHashMap<Id, Value>>,
	parent: Option<Rc<Scope>>,
}

impl Scope {
	fn base() -> Rc<Self> {
		todo!()
	}
	fn child(self: &Rc<Self>) -> Rc<Self> {
		Rc::new(Scope {
			vars: RefCell::new(AHashMap::new()),
			parent: Some(self.clone())
		})
	}
}

fn eval_expr(expr: &Expr, scope: &Rc<Scope>) -> Value {
	todo!()
}

pub fn eval(body: &[Expr]) -> Value {
	let scope = Scope::base().child();
	body.iter().map(|expr| eval_expr(expr, &scope)).last().expect("empty block")
}
