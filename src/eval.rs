//! Welcome to Rc hell
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use ahash::AHashMap;

use crate::id::{id, Id};
use crate::syntax::Expr;

#[derive(Debug)]
pub enum EvalError {
	EvalEmptyList,
	InvalidCall,
	ArgCountMismatch,
	NoVariable(Id),
	InvalidSyntax,
	TypeMismatch,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ValueRef(Rc<RefCell<Value>>);

impl std::hash::Hash for ValueRef {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		self.0.borrow().hash(state);
	}
}
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

#[derive(Clone)]
pub enum Value {
	String(Id),
	Number(f64),
	Bool(bool),
	Nul,
	List(Vec<ValueRef>),
	Map(AHashMap<ValueRef, ValueRef>),
	Function(Rc<Scope>, Vec<Id>, Vec<Expr>),
	Extern(Rc<ExtFn>),
}

impl std::hash::Hash for Value {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		core::mem::discriminant(self).hash(state);
		match self {
			Value::String(v) => v.hash(state),
			Value::Number(v) => v.to_ne_bytes().hash(state),
			Value::Bool(v) => v.hash(state),
			Value::Nul => {}
			Value::List(v) => v.hash(state),
			Value::Map(v) => v.iter().for_each(|v| v.hash(state)),
			Value::Function(_, _, v) => v.hash(state),
			Value::Extern(v) => Rc::as_ptr(v).hash(state),
		}
	}
}
impl fmt::Debug for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::String(v) => write!(f, "{v:?}"),
			Self::Number(v) => write!(f, "{v}"),
			Self::Bool(v) => write!(f, "{v}"),
			Self::Nul => write!(f, "nul"),
			Self::List(v) => write!(f, "{v:?}"),
			Self::Map(v) => write!(f, "{v:?}"),
			Self::Function(scope, args, _) => write!(f, "<fn {args:?} {scope:p}>"),
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
			(Self::Map(l), Self::Map(r)) => l == r,
			(Self::Function(_, _, l), Self::Function(_, _, r)) => l.as_ptr() == r.as_ptr(),
			// clippy says no ptr eq on dyn, i don't care
			(Self::Extern(l), Self::Extern(r)) => Rc::ptr_eq(l, r),
			_ => false,
		}
	}
}
impl Eq for Value {}

#[derive(Clone, Debug)]
pub struct Scope {
	vars: RefCell<AHashMap<Id, ValueRef>>,
	parent: Option<Rc<Scope>>,
}

impl Scope {
	fn child(self: Rc<Self>) -> Rc<Self> {
		Rc::new(Scope {
			vars: RefCell::new(AHashMap::new()),
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
		let mut vars = AHashMap::new();
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
		vars.insert(
			id("def"),
			func!(|args, scope| {
				if args.len() < 2 {
					return Err(EvalError::ArgCountMismatch);
				}
				if let Expr::List(_, func_def) = &args[0] {
					if func_def.is_empty() {
						return Err(EvalError::InvalidSyntax);
					}
					let mut body = args.to_vec();
					body.insert(0, Expr::Ident(id("fn")));
					body[1] = Expr::list(func_def[1..].to_vec());
					eval_exprs(
						&[Expr::list(vec![
							Expr::Ident(id("def")),
							func_def[0].clone(),
							Expr::list(body),
						])],
						scope,
					)
				} else if let Expr::Ident(id) = &args[0] {
					if args.len() != 2 {
						return Err(EvalError::ArgCountMismatch);
					}
					let res = eval_exprs(&args[1..=1], scope.clone())?;
					scope.set_var(id.clone(), res.clone());
					Ok(res)
				} else {
					Err(EvalError::InvalidSyntax)
				}
			}),
		);
		vars.insert(
			id("fn"),
			func!(|args, scope| {
				if args.len() < 2 {
					return Err(EvalError::ArgCountMismatch);
				}
				let names: Vec<_> = if let Expr::List(_, names) = &args[0] {
					names
						.iter()
						.map(|v| {
							if let Expr::Ident(v) = v {
								Ok(v.clone())
							} else {
								Err(EvalError::InvalidSyntax)
							}
						})
						.collect::<Result<_, _>>()?
				} else {
					return Err(EvalError::InvalidSyntax);
				};
				Ok(ValueRef::new(Value::Function(
					scope,
					names,
					args[1..].to_vec(),
				)))
			}),
		);
		vars.insert(
			id("if"),
			func!(|args, scope| {
				if args.len() != 3 {
					return Err(EvalError::ArgCountMismatch);
				}
				let res = eval_exprs(&args[0..=0], scope.clone())?;
				if matches!(&*res.0.borrow(), Value::Bool(false)) {
					eval_exprs(&args[2..=2], scope)
				} else {
					eval_exprs(&args[1..=1], scope)
				}
			}),
		);
		vars.insert(
			id("<="),
			func!(|args, scope| {
				if args.is_empty() {
					return Err(EvalError::ArgCountMismatch);
				}
				let mut iter = args.iter().map(|v| eval_exprs(&[v.clone()], scope.clone()));
				let Value::Number(first) = *iter.next().unwrap()?.0.borrow() else {
				return Err(EvalError::TypeMismatch);
			};
				Ok(ValueRef::new(Value::Bool(
					iter.fold(Ok((true, first)), |s, r| {
						let Value::Number(r) = *r?.0.borrow() else {
					return Err(EvalError::TypeMismatch);
				};
						s.and_then(|(v, l)| Ok((v && l <= r, r)))
					})?
					.0,
				)))
			}),
		);
		vars.insert(
			id("-"),
			func!(|args, scope| {
				match &args[..] {
					[] => Err(EvalError::ArgCountMismatch),
					[val] => {
						if let Value::Number(v) = *eval_exprs(&[val.clone()], scope)?.0.borrow() {
							Ok(ValueRef::new(Value::Number(-v)))
						} else {
							Err(EvalError::TypeMismatch)
						}
					}
					_ => todo!(),
				}
			}),
		);
		vars.insert(id("true"), ValueRef::new(Value::Bool(true)));
		vars.insert(id("false"), ValueRef::new(Value::Bool(false)));
		vars.insert(id("nul"), ValueRef::new(Value::Nul));
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
						.and_then(|v| <f64 as std::str::FromStr>::from_str(v).ok())
					{
						Ok(ValueRef::new(Value::Number(v)))
					} else {
						scope
							.lookup(v)
							.ok_or_else(|| EvalError::NoVariable(v.clone()))
					}
				}
			},
			Expr::List(_, body) => {
				let func = eval_exprs(
					&[body.first().ok_or(EvalError::EvalEmptyList)?.clone()],
					scope.clone(),
				)?;
				let res = match &*func.0.borrow() {
					Value::String(_) => Err(EvalError::InvalidCall),
					Value::Number(_) => Err(EvalError::InvalidCall),
					Value::Bool(_) => Err(EvalError::InvalidCall),
					Value::Nul => Err(EvalError::InvalidCall),
					Value::List(_) => Err(EvalError::InvalidCall),
					Value::Map(_) => Err(EvalError::InvalidCall),
					Value::Function(inner_scope, args, inner_body) => {
						if body.len() != args.len() + 1 {
							return Err(EvalError::ArgCountMismatch);
						}
						let inner_scope = inner_scope.clone().child();
						for (arg, val) in args.iter().zip(body[1..].iter()) {
							inner_scope
								.set_var(arg.clone(), eval_exprs(&[val.clone()], scope.clone())?);
						}
						eval_exprs(inner_body, inner_scope)
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
