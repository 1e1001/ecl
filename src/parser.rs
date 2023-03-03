//! syntax:
//! ```text
//! item = tight
//!      | item s '[' s [{ item s }] ']'
//!      | item s '\' s tight
//! tight = ident
//!       | ''' marker ''' string ''' marker '''
//!       | '(' s [{ item s }] ')'
//!       | '{' s [{ item s }] '}'
//!       | '[' s item s ']'
//!       | '#' s tight
//!       | '\' s ident s '#' s tight
//!       | '\' s ident s '\'
//! reserved: (){}[]#;'\
//! s = [{ s-inner }]
//! s-inner = space
//!         | ';' comment newline
//!         | '#;' tight
//! newline = '\x0A'..='\x0D'
//!         | '\u{85}'
//!         | '\u{2028}'
//!         | '\u{2029}'
//! space = '\0'..=' '
//!       | '\u{85}'
//!       | '\u{A0}'
//!       | '\u{1680}'
//!       | '\u{2000}'..='\u{200B}'
//!       | '\u{2028}'
//!       | '\u{2029}'
//!       | '\u{202F}'
//!       | '\u{205F}'
//!       | '\u{3000}'
//!       | '\u{FEFF}'
//! ```

use crate::id::{id, Id};
use crate::syntax::{Expr, ListType, Source};

#[derive(Debug)]
pub enum ParseError {
	UnexpectedEof,
	EmptyIdent,
	NoRecursion,
}

#[derive(Debug)]
pub struct Parser {
	source: Source,
	cursor: usize,
}

impl Parser {
	fn peek(&self, off: usize) -> Option<u8> {
		self.source.data.get(self.cursor + off).copied()
	}
	fn peek_utf8(&self) -> Option<Option<char>> {
		match self.peek(0) {
			None => None,
			Some(c @ 0b00000000..=0b01111111) => Some(Some(c as char)),
			Some(c @ 0b11000000..=0b11011111) => Some(
				std::str::from_utf8(&[c, self.peek(1)?])
					.ok()
					.and_then(|v| v.chars().next()),
			),
			Some(c @ 0b11100000..=0b11101111) => Some(
				std::str::from_utf8(&[c, self.peek(1)?, self.peek(2)?])
					.ok()
					.and_then(|v| v.chars().next()),
			),
			Some(c @ 0b11110000..=0b11110111) => Some(
				std::str::from_utf8(&[c, self.peek(1)?, self.peek(2)?, self.peek(3)?])
					.ok()
					.and_then(|v| v.chars().next()),
			),
			Some(_) => Some(None),
		}
	}
	fn adv(&mut self, n: usize) {
		self.cursor += n;
	}
	fn read_space(&mut self) -> Result<(), ParseError> {
		loop {
			match self.peek_utf8() {
				Some(Some(
					c @ ('\0'..=' '
					| '\u{0085}'
					| '\u{00A0}'
					| '\u{1680}'
					| '\u{2000}'..='\u{200B}'
					| '\u{2028}'
					| '\u{2029}'
					| '\u{202F}'
					| '\u{205F}'
					| '\u{3000}'
					| '\u{FEFF}'),
				)) => self.adv(c.len_utf8()),
				Some(Some(';')) => loop {
					match self.peek_utf8() {
						Some(Some('\x0A'..='\x0D' | '\u{85}' | '\u{2028}' | '\u{2029}')) => break,
						None => break,
						_ => self.adv(1),
					}
				},
				Some(Some('#')) if self.peek(1) == Some(b';') => {
					self.adv(2);
					drop(self.read_tight()?);
				}
				_ => return Ok(()),
			}
		}
	}
	fn read_list(&mut self, close: u8) -> Result<Vec<Expr>, ParseError> {
		self.adv(1);
		let mut res = Vec::new();
		loop {
			self.read_space()?;
			match self.peek(0) {
				Some(c) if c == close => {
					self.adv(1);
					return Ok(res);
				}
				Some(_) => res.push(self.read_item()?),
				None => return Err(ParseError::UnexpectedEof),
			}
		}
	}
	fn read_ident(&mut self) -> Result<Id, ParseError> {
		let mut res = Vec::new();
		loop {
			match self.peek(0) {
				None => return Err(ParseError::UnexpectedEof),
				// TODO: add a check for utf-8 whitespace
				Some(
					0..=b' '
					| b'('
					| b')'
					| b'['
					| b']'
					| b'{'
					| b'}'
					| b'#'
					| b';'
					| b'\''
					| b'\\',
				) => break,
				Some(c) => {
					res.push(c);
					self.adv(1);
				}
			}
		}
		if res.is_empty() {
			Err(ParseError::EmptyIdent)
		} else {
			Ok(id(res))
		}
	}
	fn read_tight(&mut self) -> Result<Expr, ParseError> {
		match self.peek(0) {
			Some(b'(') => Ok(Expr::List(ListType::Paren, self.read_list(b')')?)),
			Some(b'[') => {
				self.adv(1);
				self.read_space()?;
				let res = self.read_item()?;
				self.read_space()?;
				if self.peek(0) != Some(b']') {
					return Err(ParseError::EmptyIdent);
				}
				self.adv(1);
				Ok(res)
			}
			Some(b'{') => Ok(Expr::List(ListType::Curly, self.read_list(b'}')?)),
			Some(b'#') => {
				self.adv(1);
				self.read_space()?;
				Ok(Expr::list(vec![
					Expr::Ident(id("%cons")),
					self.read_tight()?,
				]))
			}
			Some(b'\'') => {
				self.adv(1);
				// welcome to cursed parsing land!
				let mut marker = vec![b'\''];
				loop {
					match self.peek(0) {
						Some(c) => {
							marker.push(c);
							self.adv(1);
							if c == b'\'' {
								break;
							}
						}
						None => return Err(ParseError::UnexpectedEof),
					}
				}
				let mut data = Vec::new();
				while {
					let slice = self
						.source
						.data
						.get(self.cursor..self.cursor + marker.len());
					slice.is_some() && slice != Some(&marker)
				} {
					data.push(self.peek(0).unwrap());
					self.adv(1);
				}
				self.adv(marker.len());
				if self.peek(0).is_none() {
					return Err(ParseError::UnexpectedEof);
				}
				Ok(Expr::String(id(data)))
			}
			Some(b'\\') => Err(ParseError::NoRecursion),
			Some(_) => Ok(Expr::Ident(self.read_ident()?)),
			None => Err(ParseError::UnexpectedEof),
		}
	}
	fn read_item(&mut self) -> Result<Expr, ParseError> {
		let mut res = self.read_tight()?;
		loop {
			self.read_space()?;
			match self.peek(0) {
				Some(b'[') => {
					let mut out = self.read_list(b']')?;
					out.insert(0, res);
					res = Expr::List(ListType::Bracket, out);
				}
				Some(b'\\') => {
					self.adv(1);
					self.read_space()?;
					res = Expr::list(vec![res, self.read_tight()?]);
				}
				_ => return Ok(res),
			}
		}
	}
	fn read_file(&mut self) -> Result<Vec<Expr>, ParseError> {
		self.read_space()?;
		let mut res = Vec::new();
		while self.peek(0).is_some() {
			res.push(self.read_item()?);
		}
		Ok(res)
	}
}

pub fn parse(source: Source) -> Vec<Expr> {
	let mut parser = Parser { source, cursor: 0 };
	match parser.read_file() {
		Ok(v) => v,
		Err(e) => {
			println!("error at {}: {e:?}", parser.cursor);
			panic!("parse error");
		}
	}
}
