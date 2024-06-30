use std::fmt;
use std::{num, str::FromStr};

// #[derive(Debug)]
pub struct MyType {
    value: usize,
    // ...
}

impl Default for MyType {
    fn default() -> Self {
        Self { value: 10 }
    }
}

impl From<usize> for MyType {
    fn from(value: usize) -> Self {
        MyType { value }
    }
}

impl TryFrom<u32> for MyType {
    type Error = &'static str;
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        Ok(MyType {
            value: value as usize,
        })
    }
}

impl fmt::Debug for MyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MyType")
            .field("value", &self.value)
            .finish()
    }
}

impl fmt::Display for MyType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl FromStr for MyType {
    type Err = num::ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let value = s.parse::<usize>()?;
        Ok(MyType { value })
    }
}
