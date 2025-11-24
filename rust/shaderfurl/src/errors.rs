// {{{ Error reporting macros
/// Helper for creating a report via ariadne
#[macro_export]
macro_rules! report {
    (
        $self:expr,
        $code:expr,
        $span: expr,
        ($($msg:tt)+),
        $(($lspan: expr, $($lmsg:tt)+),)*
    ) => {{
        use ariadne::{Report, ReportKind, Label};
        use $crate::cst::HasSpan;

        let mut report = Report::build(ReportKind::Error, $span.span_of())
            .with_code($code)
            .with_message(format!($($msg)+));

        $(
            report = report.with_label(
                Label::new($lspan.span_of()).with_message(format!($($lmsg)+))
            );
        )*

        $self.push_report(report.finish());
    }};
}

/// A report where the source spans are all identical.
#[macro_export]
macro_rules! compact_report {
    (
        ($self:expr, $code:expr, $span: expr),
        ($($msg:tt)+),
        $(($($lmsg:tt)+),)*
    ) => {{
        $crate::report!(
            $self, $code, $span, ($($msg)+),
            $(($span,$($lmsg)+),)*
        )
    }};
}
// }}}
