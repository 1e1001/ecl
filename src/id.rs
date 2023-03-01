use std::str::Utf8Chunks;
use std::sync::Arc;
use std::{fmt, ops};

pub fn id(v: impl Into<Vec<u8>>) -> Id {
	Id::new(v)
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(Arc<[u8]>);
impl Id {
	pub fn new(v: impl Into<Vec<u8>>) -> Self {
		Id(v.into().into())
	}
}
impl ops::Deref for Id {
	type Target = [u8];
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl fmt::Debug for Id {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use std::fmt::Write;
		let needs_escape = std::str::from_utf8(self)
			.map(|v| v.chars().any(|v| matches!(v, '\0'..='\x1F' | '\\' | '\'')))
			.unwrap_or(true);
		if needs_escape {
			f.write_str("%esc\\''")?;
			for chunk in Utf8Chunks::new(self) {
				for c in chunk.valid().chars() {
					match c {
						'\0' => f.write_str("\\0")?,
						'\x07' => f.write_str("\\a")?,
						'\x08' => f.write_str("\\b")?,
						'\x1b' => f.write_str("\\e")?,
						'\n' => f.write_str("\\n")?,
						'\t' => f.write_str("\\t")?,
						'\r' => f.write_str("\\r")?,
						'\x0b' => f.write_str("\\v")?,
						'\\' => f.write_str("\\\\")?,
						'\'' => f.write_str("\\q")?,
						c @ ' '.. => f.write_char(c)?,
						c => write!(f, "\\x{:>02x}", c as u8)?,
					}
				}
				for c in chunk.invalid() {
					write!(f, "\\x{c:>02x}")?;
				}
			}
			f.write_str("''")
		} else {
			write!(f, "''{self}''")
		}
	}
}
impl fmt::Display for Id {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(&String::from_utf8_lossy(self))
	}
}
