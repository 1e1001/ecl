#![feature(utf8_chunks)]

use id::id;
use syntax::Source;

use crate::syntax::Expr;

mod id;
mod syntax;
mod parser;
mod eval;

fn parse(source: Source) -> Vec<Expr> {
	match parser::parse_source(source) {
		Ok(v) => v,
		Err(e) => {
			println!("error: {e:?}");
			panic!("parse error");
		},
	}
}

fn main() {
	let path = std::env::args().nth(1).unwrap();
	let file = Source::new(id(path.as_bytes()), id(std::fs::read(path).unwrap()));
	let parsed = parse(file);
	println!("{:?}", eval::eval(&parsed));
}

#[cfg(test)]
mod tests {
	#[test]
	fn all_parse_test() {
		use super::*;
		use crate::syntax::ListType;
		macro_rules! sexp {
			($id:ident) => {
				Expr::Ident(id(stringify!($id)))
			};
			($lit:literal) => {
				Expr::String(id($lit))
			};
			(%) => {
				Expr::Ident(id("%cons"))
			};
			((# $($tt:tt)*)) => {
				Expr::List(ListType::Special, vec![$(sexp!($tt)),*])
			};
			(($($tt:tt)*)) => {
				Expr::List(ListType::Paren, vec![$(sexp!($tt)),*])
			};
			([$($tt:tt)*]) => {
				Expr::List(ListType::Bracket, vec![$(sexp!($tt)),*])
			};
			({$($tt:tt)*}) => {
				Expr::List(ListType::Curly, vec![$(sexp!($tt)),*])
			};
		}
		let parsed = parse(Source::new(id("<test>"), id(r##"
			ident
			''string''
			'raw'raw'raww'str'ra'ing'raw'
			(standard list)
			m[expr list]
			{curly list}
			; line comment
			#;(expr comment)
			#;''block comment''
			() {}
			([embedded_expr])
			postfix\call
			#cons
			#[also[cons]]
			#(again cons)
			these\[are[equivalent]]
			these[are[equivalent]]
		"##)));
		let Expr::List(_, expected) = sexp!((
			ident
			"string"
			"raw'raww'str'ra'ing"
			(standard list)
			[m expr list]
			{curly list}
			()
			{}
			(embedded_expr)
			(# postfix call)
			(# % cons)
			(# % [also cons])
			(# % (again cons))
			(# these [are equivalent])
			[these [are equivalent]]
		)) else { unreachable!() };
		assert_eq!(parsed, expected);
	}
}
