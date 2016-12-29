use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;

/// holds strings and convert them into their own unique numbers.
/// This can reduce name competition costs and memory usage.
#[derive(Debug, Default)]
pub struct Interner {
    symbols: Vec<Rc<String>>,
    table: HashMap<String, Symbol>,
}

impl Interner {
    fn new() -> Self {
        Interner::default()
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        match self.table.get(s) {
            Some(&n) => n,
            None => {
                let id = self.symbols.len();
                let sym = Symbol(id);
                self.table.insert(s.to_string(), sym);
                self.symbols.push(Rc::new(s.to_string()));
                sym
            }
        }
    }

    pub fn get(&self, sym: &Symbol) -> Rc<String> {
        self.symbols[sym.0].clone()
    }

    pub fn refresh(&mut self) {
        self.symbols = Vec::new();
        self.table = HashMap::new();
    }
}

pub fn with_interner<T, F: FnOnce(&mut Interner) -> T>(f: F) -> T {
    thread_local! {
        static INTERNER: RefCell<Interner> = RefCell::new(Interner::new());
    }
    INTERNER.with(|i| f(&mut *i.borrow_mut()))
}

/// represents interned strings as small integer type.
/// can be transformed to `String` using `Interner`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(usize);

impl Symbol {
    pub fn intern(s: &str) -> Self {
        with_interner(|i| i.intern(s))
    }

    pub fn as_str(&self) -> InternedString {
        with_interner(|i| InternedString::new(i.get(self)))
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

/// `InternedString` represents already interned symbol.
/// `Interner` can convert `Symbol` to `InternedString`.
/// `InternedString` implements `Deref` for `str`
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct InternedString {
    inner: Rc<String>,
}

impl InternedString {
    pub fn new(s: Rc<String>) -> Self {
        InternedString {
            inner: s,
        }
    }

    pub fn as_str(&self) -> &str {
        &*self.inner
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &str {
        &*self.inner
    }
}

impl<'a> PartialEq<&'a str> for InternedString {
    #[inline(always)]
    fn eq(&self, other: & &'a str) -> bool {
        PartialEq::eq(&self.inner[..], *other)
    }
    #[inline(always)]
    fn ne(&self, other: & &'a str) -> bool {
        PartialEq::ne(&self.inner[..], *other)
    }
}

impl<'a> PartialEq<InternedString > for &'a str {
    #[inline(always)]
    fn eq(&self, other: &InternedString) -> bool {
        PartialEq::eq(*self, &other.inner[..])
    }
    #[inline(always)]
    fn ne(&self, other: &InternedString) -> bool {
        PartialEq::ne(*self, &other.inner[..])
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let dog = Symbol::intern("dog");
        assert_eq!(0, dog.as_usize());
        assert_eq!("dog", dog.as_str());
        let cat = Symbol::intern("cat");
        assert_eq!(1, cat.as_usize());
        assert_eq!("cat", cat.as_str());
        let dog2 = Symbol::intern("dog");
        assert_eq!(0, dog2.as_usize());
        assert_eq!("dog", dog2.as_str());
    }
}
