use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

use crate::utils::Span;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    UnexpectedToken(String),
    ExpectedButFound(String, String),
    ExpectedButFoundWithExpectGuard(String, String),
    ExpectedSpan,
    InvalidAssignmentTarget(String),
    SubmoduleDoesNotExist(String),
    GlobalIsNotAtFirst,
}

impl ErrorKind {
    fn to_string(&self, color: Color) -> String {
        match self {
            ErrorKind::UnexpectedToken(token) => {
                format!("Unexpected token: {}", token.fg(color))
            }
            ErrorKind::ExpectedButFound(expected, found) => {
                format!("Expected {} but got {}", expected, found)
            }
            ErrorKind::ExpectedButFoundWithExpectGuard(expected, found) => {
                format!("(Expect Guard) Expected {} but got {}", expected, found)
            }
            ErrorKind::ExpectedSpan => format!("Expected span"),
            ErrorKind::InvalidAssignmentTarget(target) => {
                format!("Invalid assignment target: {}", target.fg(color))
            }
            ErrorKind::SubmoduleDoesNotExist(submodule) => {
                format!("Submodule does not exist: {}", submodule.fg(color))
            }
            ErrorKind::GlobalIsNotAtFirst => {
                format!(
                    "{} can only be used as the first identifier in a path",
                    "global".fg(Color::Blue)
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
    pub severity: ReportKind,
}

impl Error {
    fn color(&self) -> Color {
        return match self.severity {
            ReportKind::Error => Color::Red,
            ReportKind::Warning => Color::Yellow,
            ReportKind::Advice => Color::Magenta,
            ReportKind::Custom(_, _) => Color::White,
        };
    }
    pub fn show(&self, contents: &str, path: &str) -> Option<()> {
        let color = self.color();
        let message = format!("{}", self.kind.clone().to_string(color));

        Some(
            Report::build(self.severity, path, self.span.start)
                .with_message(&message)
                .with_label(
                    Label::new((path, self.span.start..self.span.end))
                        .with_message(&message)
                        .with_color(color),
                )
                .finish()
                .print((path, Source::from(contents)))
                .unwrap(),
        )
    }
}
