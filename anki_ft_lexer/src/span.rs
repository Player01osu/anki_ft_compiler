use std::fmt::{Debug, Display};

#[derive(Clone, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start_row, self.start_col)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} => {}:{}",
            self.start_row, self.start_col, self.end_row, self.end_col
        )
    }
}

impl Span {
    pub fn join(self, other: Span) -> Self {
        assert!(
            (other.end_row >= self.start_row && other.end_col >= self.start_col)
                || other.end_row > self.start_row,
            "{self}\n{other}\nInvalid span join"
        );

        Self {
            start_row: self.start_row,
            start_col: self.start_col,
            end_row: other.end_row,
            end_col: other.end_col,
        }
    }

    fn valid_span(self) -> bool {
        (self.end_row >= self.start_row && self.end_col >= self.start_col)
            || self.end_row > self.start_row
    }
}

