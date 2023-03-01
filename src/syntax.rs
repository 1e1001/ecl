use std::fmt;

use crate::id::Id;

#[derive(Clone, Debug)]
pub struct Source {
	pub name: Id,
	pub data: Id,
}
impl Source {
	pub fn new(name: Id, data: Id) -> Self {
		Self { name, data }
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListType {
	Paren,
	Bracket,
	Curly,
	Special,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr {
	String(Id),
	Ident(Id),
	List(ListType, Vec<Expr>),
}
impl Expr {
	pub fn list(v: Vec<Expr>) -> Self {
		Self::List(ListType::Special, v)
	}
}
impl fmt::Debug for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use std::fmt::Write;
		match self {
			Self::String(v) => write!(f, "{v:?}"),
			Self::Ident(v) => write!(f, "{v}"),
			Self::List(t, v) => {
				match t {
					ListType::Paren => {},
					ListType::Bracket => f.write_str("#;b")?,
					ListType::Curly => f.write_str("#;c")?,
					ListType::Special => f.write_str("#;s")?,
				}
				f.write_char('(')?;
				let mut iter = v.iter();
				if let Some(first) = iter.next() {
					write!(f, "{first:?}")?;
					for rest in iter {
						write!(f, " {rest:?}")?;
					}
				}
				f.write_char(')')
			}
		}
	}
}
