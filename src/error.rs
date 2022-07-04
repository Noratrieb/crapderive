use ariadne::{Color, Label, Report, ReportKind, Source};
use dbg_pls::DebugPls;
use logos::Span;

#[derive(Debug, DebugPls)]
// tag::error[]
pub struct CompilerError {
    pub msg: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
    pub help: Option<String>,
}
// end::error[]

impl CompilerError {
    pub fn new(msg: String, span: Span, notes: Vec<(String, Span)>, help: Option<String>) -> Self {
        Self {
            msg,
            span,
            notes,
            help,
        }
    }

    pub fn simple(msg: String, span: Span) -> Self {
        Self::new_notes(msg, span, Vec::new())
    }

    pub fn help(msg: String, span: Span, help: String) -> Self {
        Self::new(msg, span, Vec::new(), Some(help))
    }
    pub fn new_notes(msg: String, span: Span, notes: Vec<(String, Span)>) -> Self {
        Self::new(msg, span, notes, None)
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub fn report(error: CompilerError, filename: &str, src: &str) {
    let mut report = Report::build(ReportKind::Error, filename, 12)
        .with_message(&error.msg)
        .with_label(
            Label::new((filename, error.span))
                .with_message(error.msg)
                .with_color(Color::Red),
        );

    for (note, span) in error.notes {
        report = report.with_label(Label::new((filename, span)).with_message(note));
    }

    if let Some(help) = error.help {
        report = report.with_help(help);
    }

    report
        .finish()
        .print((filename, Source::from(src)))
        .expect("failed to print");
}
