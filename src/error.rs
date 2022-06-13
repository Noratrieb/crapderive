use ariadne::{Color, Label, Report, ReportKind, Source};
use dbg_pls::DebugPls;
use logos::Span;

#[derive(Debug, DebugPls)]
pub struct CompilerError {
    pub msg: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
    pub help: Option<String>,
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
