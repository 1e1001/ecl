use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::id::{id, Id};
use crate::syntax::Expr;

#[derive(Debug)]
pub enum EvalError {
	EvalEmptyList,
	InvalidCall,
	ArgCountMismatch,
	NoVariable,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ValueRef(Rc<RefCell<Value>>);
impl ValueRef {
	fn new(v: Value) -> Self {
		Self(Rc::new(RefCell::new(v)))
	}
}
impl fmt::Debug for ValueRef {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", (*self.0).borrow())
	}
}

pub type ExtFn = dyn Fn(&'_ [Expr], Rc<Scope>) -> Result<ValueRef, EvalError>;

pub enum Value {
	String(Id),
	Number(i128),
	Bool(bool),
	Nul,
	List(Vec<ValueRef>),
	Function(Rc<Scope>, Vec<Id>, Vec<Expr>),
	Extern(Rc<ExtFn>),
}
impl fmt::Debug for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::String(v) => write!(f, "{v:?}"),
			Self::Number(v) => write!(f, "{v}"),
			Self::Bool(v) => write!(f, "{v}"),
			Self::Nul => write!(f, "nul"),
			Self::List(v) => write!(f, "{v:?}"),
			Self::Function(scope, _, _) => write!(f, "<fn {scope:p}>"),
			Self::Extern(v) => write!(f, "<fn! {v:p}>"),
		}
	}
}
impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Self::String(l), Self::String(r)) => l == r,
			(Self::Number(l), Self::Number(r)) => l == r,
			(Self::Bool(l), Self::Bool(r)) => l == r,
			(Self::List(l), Self::List(r)) => l == r,
			(Self::Function(l0, l1, l2), Self::Function(r0, r1, r2)) => {
				l0 == r0 && l1 == r1 && l2 == r2
			}
			// no ptr eq on dyn :(
			(Self::Extern(_), Self::Extern(_)) => false,
			_ => false,
		}
	}
}
impl Eq for Value {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope {
	vars: RefCell<HashMap<Id, ValueRef>>,
	parent: Option<Rc<Scope>>,
}

impl Scope {
	fn child(self: Rc<Self>) -> Rc<Self> {
		Rc::new(Scope {
			vars: RefCell::new(HashMap::new()),
			parent: Some(self),
		})
	}
	fn set_var(&self, name: Id, value: ValueRef) {
		self.vars.borrow_mut().insert(name, value);
	}
	fn lookup(&self, name: &Id) -> Option<ValueRef> {
		self.vars.borrow().get(name).cloned().or_else(|| {
			if let Some(parent) = &self.parent {
				parent.lookup(name)
			} else {
				None
			}
		})
	}
	fn base() -> Rc<Self> {
		let mut vars = HashMap::new();
		macro_rules! func {
			($($tt:tt)*) => {
				ValueRef::new(Value::Extern(Rc::new($($tt)*)))
			};
		}
		vars.insert(
			id("println"),
			func!(|args, scope| {
				let mut iter = args.iter();
				if let Some(first) = iter.next() {
					print!("{:?}", eval_exprs(&[first.clone()], scope.clone())?);
					for rest in iter {
						print!(" {:?}", eval_exprs(&[rest.clone()], scope.clone())?);
					}
				}
				println!();
				Ok(ValueRef::new(Value::Nul))
			}),
		);
		Rc::new(Self {
			vars: RefCell::new(vars),
			parent: None,
		})
	}
}

pub fn eval_exprs(body: &[Expr], scope: Rc<Scope>) -> Result<ValueRef, EvalError> {
	let res = body
		.iter()
		.map(|expr| match expr {
			Expr::String(v) => Ok(ValueRef::new(Value::String(v.clone()))),
			Expr::Ident(v) => match &v[..] {
				b"true" => Ok(ValueRef::new(Value::Bool(true))),
				b"false" => Ok(ValueRef::new(Value::Bool(false))),
				b"nul" => Ok(ValueRef::new(Value::Nul)),
				_ => {
					if let Some(v) = std::str::from_utf8(v)
						.ok()
						.and_then(|v| <i128 as std::str::FromStr>::from_str(v).ok())
					{
						Ok(ValueRef::new(Value::Number(v)))
					} else {
						scope.lookup(v).ok_or(EvalError::NoVariable)
					}
				}
			},
			Expr::List(_, body) => {
				let func = eval_exprs(
					&[body.first().ok_or(EvalError::EvalEmptyList)?.clone()],
					scope.clone(),
				)?;
				let res = match &*func.0.borrow_mut() {
					Value::String(_) => Err(EvalError::InvalidCall),
					Value::Number(_) => Err(EvalError::InvalidCall),
					Value::Bool(_) => Err(EvalError::InvalidCall),
					Value::Nul => Err(EvalError::InvalidCall),
					Value::List(_) => Err(EvalError::InvalidCall),
					Value::Function(inner_scope, args, body) => {
						if body.len() != args.len() + 1 {
							return Err(EvalError::ArgCountMismatch);
						}
						let inner_scope = inner_scope.clone().child();
						for (arg, val) in args.iter().zip(body[1..].iter()) {
							inner_scope
								.set_var(arg.clone(), eval_exprs(&[val.clone()], scope.clone())?);
						}
						eval_exprs(&body[1..], inner_scope)
					}
					Value::Extern(f) => f(&body[1..], scope.clone()),
				};
				res
			}
		})
		.collect::<Result<Vec<_>, _>>()?;
	Ok(res.last().ok_or(EvalError::EvalEmptyList)?.clone())
}

pub fn eval(body: &[Expr]) -> ValueRef {
	match eval_exprs(body, Scope::base().child()) {
		Ok(v) => v,
		Err(e) => {
			println!("error: {e:?}");
			panic!("eval error");
		}
	}
}
