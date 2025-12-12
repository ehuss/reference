//! Support for recording and rendering coverage of the grammar.

use grammar::{ExpressionKind, Grammar};

struct SpanInfo {
    bg_color: String,
    padding: usize,
    tooltip: String,
}

#[derive(Default)]
pub struct Coverage {
    /// Count of the repetitions for each expression.
    ///
    /// The index is the expression ID. The value is the number of times a
    /// particular number of repetitions was found for that expression (index
    /// N means it matched N repetitions of the given number of times).
    pub match_count: Vec<Vec<u32>>,

    /// Count of how often an expression failed to match its input.
    ///
    /// The index is the expression ID, the count is the number of times it failed.
    pub no_match_count: Vec<u32>,

    /// Count of how often the expression caused a `ParseError`.
    ///
    /// The index is the expression ID, the count is the number of times it caused an error.
    pub parse_error: Vec<u32>,
}

impl Coverage {
    pub fn cov_match(&mut self, id: u32, count: u32) {
        if self.match_count.len() < (id + 1) as usize {
            self.match_count.resize((id + 1) as usize, Vec::new());
        }
        let ns = self.match_count.get_mut(id as usize).unwrap();
        if ns.len() < (count + 1) as usize {
            ns.resize((count + 1) as usize, 0);
        }
        *ns.get_mut(count as usize).unwrap() += 1;
    }

    pub fn cov_no_match(&mut self, id: u32) {
        if self.no_match_count.len() < (id + 1) as usize {
            self.no_match_count.resize((id + 1) as usize, 0);
        }
        *self.no_match_count.get_mut(id as usize).unwrap() += 1;
    }

    pub fn cov_parse_error(&mut self, id: u32) {
        if self.parse_error.len() < (id + 1) as usize {
            self.parse_error.resize((id + 1) as usize, 0);
        }
        *self.parse_error.get_mut(id as usize).unwrap() += 1;
    }

    pub fn merge(&mut self, other: Coverage) {
        if self.match_count.len() < other.match_count.len() {
            self.match_count.resize(other.match_count.len(), Vec::new());
        }
        for (id, counts) in other.match_count.into_iter().enumerate() {
            let this = self.match_count.get_mut(id).unwrap();
            if this.len() < counts.len() {
                this.resize(counts.len(), 0);
            }
            for (count, value) in counts.into_iter().enumerate() {
                this[count] += value;
            }
        }

        if self.no_match_count.len() < other.no_match_count.len() {
            self.no_match_count.resize(other.no_match_count.len(), 0);
        }
        for (id, value) in other.no_match_count.into_iter().enumerate() {
            self.no_match_count[id] += value;
        }

        if self.parse_error.len() < other.parse_error.len() {
            self.parse_error.resize(other.parse_error.len(), 0);
        }
        for (id, value) in other.parse_error.into_iter().enumerate() {
            self.parse_error[id] += value;
        }
    }

    /// Saves the coverage data to a file called `coverage.html`.
    pub fn save(&self, grammar: &Grammar) {
        let mut html = String::new();
        self.render_html(&mut html, grammar);
        std::fs::write("coverage.html", html).expect("failed to write coverage.html");
    }

    fn get_coverage_status(&self, id: u32, kind: &grammar::ExpressionKind) -> CoverageStatus {
        let match_count = self.match_count.get(id as usize);
        let no_match = self.no_match_count.get(id as usize).copied().unwrap_or(0);
        let parse_error = self.parse_error.get(id as usize).copied().unwrap_or(0);

        let has_matches = match_count
            .map(|counts| counts.iter().any(|&c| c > 0))
            .unwrap_or(false);
        let has_no_match = no_match > 0;

        // Special case logic for specific expression kinds
        match kind {
            ExpressionKind::Optional(_) => {
                // Green means match_count contains both 0 and 1
                if let Some(counts) = match_count {
                    let has_zero = counts.get(0).copied().unwrap_or(0) > 0;
                    let has_one = counts.get(1).copied().unwrap_or(0) > 0;
                    if has_zero && has_one {
                        return CoverageStatus::Green;
                    } else if has_zero || has_one {
                        return CoverageStatus::Yellow;
                    }
                }
                CoverageStatus::Red
            }
            ExpressionKind::Repeat(_) => {
                // Green means match_count contains 0, 1, and more than 1
                if let Some(counts) = match_count {
                    let has_zero = counts.get(0).copied().unwrap_or(0) > 0;
                    let has_one = counts.get(1).copied().unwrap_or(0) > 0;
                    let has_more = counts.iter().skip(2).any(|&c| c > 0);
                    if has_zero && has_one && has_more {
                        return CoverageStatus::Green;
                    } else if has_zero || has_one || has_more {
                        return CoverageStatus::Yellow;
                    }
                }
                CoverageStatus::Red
            }
            ExpressionKind::RepeatPlus(_) => {
                // Green means match_count contains 1 and more than 1 and no_match_count is not zero
                if let Some(counts) = match_count {
                    let has_one = counts.get(1).copied().unwrap_or(0) > 0;
                    let has_more = counts.iter().skip(2).any(|&c| c > 0);
                    if has_one && has_more && has_no_match {
                        return CoverageStatus::Green;
                    } else if (has_one || has_more) || has_no_match {
                        return CoverageStatus::Yellow;
                    }
                }
                CoverageStatus::Red
            }
            ExpressionKind::RepeatRange { min, max, .. } => {
                // Green means match_count contains the minimum and maximum values
                if let Some(counts) = match_count {
                    let min_val = min.unwrap_or(0) as usize;
                    let has_min = counts.get(min_val).copied().unwrap_or(0) > 0;

                    let has_max = if let Some(max_val) = max {
                        counts.get(*max_val as usize).copied().unwrap_or(0) > 0
                    } else {
                        // If max is None, consider it green if there are matches for any count over min
                        counts.iter().skip(min_val + 1).any(|&c| c > 0)
                    };

                    if has_min && has_max {
                        return CoverageStatus::Green;
                    } else if has_min || has_max {
                        return CoverageStatus::Yellow;
                    }
                }
                CoverageStatus::Red
            }
            ExpressionKind::Cut(_) => {
                // Green means match_count contains a nonzero value and parse_error contains a nonzero value
                if has_matches && parse_error > 0 {
                    return CoverageStatus::Green;
                } else if has_matches || parse_error > 0 {
                    return CoverageStatus::Yellow;
                }
                CoverageStatus::Red
            }
            _ => {
                // Default logic for other expression kinds
                if !has_matches && no_match == 0 && parse_error == 0 {
                    CoverageStatus::Red
                } else if has_matches && !has_no_match {
                    CoverageStatus::Yellow
                } else if has_matches && has_no_match {
                    CoverageStatus::Green
                } else {
                    CoverageStatus::Red
                }
            }
        }
    }

    fn render_html(&self, output: &mut String, grammar: &Grammar) {
        output.push_str(HTML_HEADER);

        for name in &grammar.name_order {
            if let Some(prod) = grammar.productions.get(name) {
                self.render_production(prod, output);
            }
        }

        output.push_str(HTML_FOOTER);
    }

    fn render_production(&self, prod: &grammar::Production, output: &mut String) {
        output.push_str("<div class=\"production\">");
        output.push_str(&format!(
            "<span class=\"production-name\">{}</span>",
            html_escape(&prod.name)
        ));
        output.push_str(" → ");
        self.render_expression(&prod.expression, output, 0);
        output.push_str("</div>\n");
    }

    fn render_expression(&self, expr: &grammar::Expression, output: &mut String, depth: usize) {
        if matches!(expr.kind, grammar::ExpressionKind::Break(_)) {
            // TODO: This isn't working as intended. The `<br>` is inside its
            // parent span, which prevents it breaking the entire production.
            // This probably will require a significant change to how
            // rendering works.
            if let grammar::ExpressionKind::Break(indent) = &expr.kind {
                output.push_str("<br>");
                for _ in 0..*indent {
                    output.push_str("&nbsp;");
                }
            }
            return;
        }

        if matches!(expr.kind, grammar::ExpressionKind::Comment(_)) {
            if let grammar::ExpressionKind::Comment(s) = &expr.kind {
                output.push_str(&format!(
                    "<span class=\"comment\">// {}</span>",
                    html_escape(s)
                ));
            }
            return;
        }

        let status = self.get_coverage_status(expr.id, &expr.kind);
        let bg_color = status.color();
        let has_error = self.parse_error.get(expr.id as usize).copied().unwrap_or(0) > 0;

        let padding = depth * 2;
        let tooltip = self.generate_tooltip(expr.id);

        output.push_str(&format!(
            "<span class=\"expr\" style=\"background-color: {}; padding: {}px;\" \
             onmouseover=\"showTooltip(event, '{}')\" \
             onmouseout=\"hideTooltip()\">",
            bg_color,
            padding,
            html_escape(&tooltip).replace("'", "&apos;")
        ));

        if has_error {
            output.push_str("<span class=\"error-marker\">●</span>");
        }

        self.render_expression_kind(
            &expr.kind,
            output,
            depth + 1,
            &SpanInfo {
                bg_color: bg_color.to_string(),
                padding,
                tooltip: html_escape(&tooltip).replace("'", "&apos;"),
            },
        );

        if let Some(suffix) = &expr.suffix {
            output.push_str(&format!("<sub>{}</sub>", html_escape(suffix)));
        }

        output.push_str("</span>");
    }

    fn render_expression_kind(
        &self,
        kind: &grammar::ExpressionKind,
        output: &mut String,
        depth: usize,
        parent_span: &SpanInfo,
    ) {
        use grammar::ExpressionKind;

        match kind {
            ExpressionKind::Grouped(e) => {
                output.push_str("( ");
                self.render_expression(e, output, depth);
                output.push_str(" )");
            }
            ExpressionKind::Alt(es) => {
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        // Close and reopen span if next item is a Break
                        if e.is_break() {
                            output.push_str("</span>");
                            self.render_expression(e, output, depth);
                            // Reopen the parent span
                            output.push_str(&format!(
                                "<span class=\"expr\" style=\"background-color: {}; padding: {}px;\" \
                                 onmouseover=\"showTooltip(event, '{}')\" \
                                 onmouseout=\"hideTooltip()\">",
                                parent_span.bg_color, parent_span.padding, parent_span.tooltip
                            ));
                            continue;
                        } else {
                            output.push_str(" | ");
                        }
                    }
                    self.render_expression(e, output, depth);
                }
            }
            ExpressionKind::Sequence(es) => {
                for (i, e) in es.iter().enumerate() {
                    if e.is_break() {
                        // Close span before Break, then reopen after
                        output.push_str("</span>");
                        self.render_expression(e, output, depth);
                        if i + 1 < es.len() {
                            // Reopen parent span
                            output.push_str(&format!(
                                "<span class=\"expr\" style=\"background-color: {}; padding: {}px;\" \
                                 onmouseover=\"showTooltip(event, '{}')\" \
                                 onmouseout=\"hideTooltip()\">",
                                parent_span.bg_color, parent_span.padding, parent_span.tooltip
                            ));
                        }
                    } else {
                        if i > 0 && !es.get(i - 1).map(|prev| prev.is_break()).unwrap_or(false) {
                            output.push(' ');
                        }
                        self.render_expression(e, output, depth);
                    }
                }
            }
            ExpressionKind::Optional(e) => {
                self.render_expression(e, output, depth);
                output.push_str("<sup>?</sup>");
            }
            ExpressionKind::NegativeLookahead(e) => {
                output.push('!');
                self.render_expression(e, output, depth);
            }
            ExpressionKind::Repeat(e) => {
                self.render_expression(e, output, depth);
                output.push_str("<sup>*</sup>");
            }
            ExpressionKind::RepeatPlus(e) => {
                self.render_expression(e, output, depth);
                output.push_str("<sup>+</sup>");
            }
            ExpressionKind::RepeatRange {
                expr,
                name,
                min,
                max,
                limit,
            } => {
                self.render_expression(expr, output, depth);
                output.push_str("<sup>");
                if let Some(n) = name {
                    output.push_str(&html_escape(n));
                    output.push(':');
                }
                if let Some(m) = min {
                    output.push_str(&m.to_string());
                }
                output.push_str(&format!("{}", limit));
                if let Some(m) = max {
                    output.push_str(&m.to_string());
                }
                output.push_str("</sup>");
            }
            ExpressionKind::RepeatRangeNamed(e, name) => {
                self.render_expression(e, output, depth);
                output.push_str(&format!("<sup>{}</sup>", html_escape(name)));
            }
            ExpressionKind::Nt(nt) => {
                output.push_str(&format!(
                    "<span class=\"nonterminal\">{}</span>",
                    html_escape(nt)
                ));
            }
            ExpressionKind::Terminal(t) => {
                output.push_str(&format!(
                    "<span class=\"terminal\">`{}`</span>",
                    html_escape(t)
                ));
            }
            ExpressionKind::Prose(s) => {
                output.push_str(&format!(
                    "<span class=\"prose\">&lt;{}&gt;</span>",
                    html_escape(s)
                ));
            }
            ExpressionKind::Break(_) | ExpressionKind::Comment(_) => {
                // These are handled in render_expression to avoid coverage spans
                unreachable!("Break and Comment should be handled in render_expression")
            }
            ExpressionKind::Charset(set) => {
                output.push('[');
                for (i, chars) in set.iter().enumerate() {
                    if i > 0 {
                        output.push(' ');
                    }
                    self.render_characters(chars, output);
                }
                output.push(']');
            }
            ExpressionKind::NegExpression(e) => {
                output.push('~');
                self.render_expression(e, output, depth);
            }
            ExpressionKind::Cut(e) => {
                output.push_str("^ ");
                self.render_expression(e, output, depth);
            }
            ExpressionKind::Unicode((_, s)) => {
                output.push_str(&format!("U+{}", html_escape(s)));
            }
        }
    }

    fn render_characters(&self, chars: &grammar::Characters, output: &mut String) {
        use grammar::{Character, Characters};

        match chars {
            Characters::Named(s) => {
                output.push_str(&format!(
                    "<span class=\"nonterminal\">{}</span>",
                    html_escape(s)
                ));
            }
            Characters::Terminal(s) => {
                output.push_str(&format!("`{}`", html_escape(s)));
            }
            Characters::Range(a, b) => {
                let render_ch = |ch: &Character| -> String {
                    match ch {
                        Character::Char(c) => format!("`{}`", html_escape(&c.to_string())),
                        Character::Unicode((_, s)) => format!("U+{}", html_escape(s)),
                    }
                };
                output.push_str(&render_ch(a));
                output.push('-');
                output.push_str(&render_ch(b));
            }
        }
    }

    fn generate_tooltip(&self, id: u32) -> String {
        let mut tooltip = String::new();

        tooltip.push_str(&format!("ID: {}\\n", id));

        if let Some(counts) = self.match_count.get(id as usize) {
            if counts.iter().any(|&c| c > 0) {
                tooltip.push_str("Match counts:\\n");
                for (n, &count) in counts.iter().enumerate() {
                    if count > 0 {
                        let bar = "█".repeat((count.min(50) / 5).max(1) as usize);
                        tooltip.push_str(&format!("  {}: {} {}\\n", n, count, bar));
                    }
                }
            }
        }

        let no_match = self.no_match_count.get(id as usize).copied().unwrap_or(0);
        if no_match > 0 {
            tooltip.push_str(&format!("No match: {}\\n", no_match));
        }

        let parse_error = self.parse_error.get(id as usize).copied().unwrap_or(0);
        if parse_error > 0 {
            tooltip.push_str(&format!("Parse errors: {}\\n", parse_error));
        }

        if tooltip.ends_with("\\n") {
            tooltip.truncate(tooltip.len() - 2);
        }

        tooltip
    }
}

#[derive(Debug, Clone, Copy)]
enum CoverageStatus {
    Red,
    Yellow,
    Green,
}

impl CoverageStatus {
    fn color(&self) -> &'static str {
        match self {
            CoverageStatus::Red => "#ffcccc",
            CoverageStatus::Yellow => "#ffffcc",
            CoverageStatus::Green => "#ccffcc",
        }
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

const HTML_HEADER: &str = r#"<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Grammar Coverage Report</title>
    <style>
        body {
            font-family: monospace;
            padding: 20px;
            background-color: #f5f5f5;
        }
        .production {
            margin: 20px 0;
            padding: 10px;
            background-color: white;
            border: 1px solid #ddd;
            border-radius: 4px;
        }
        .production-name {
            font-weight: bold;
            color: #0066cc;
        }
        .expr {
            display: inline-block;
            border-radius: 6px;
            position: relative;
        }
        .nonterminal {
            color: #0066cc;
        }
        .terminal {
            color: #009900;
        }
        .prose {
            color: #666;
            font-style: italic;
        }
        .comment {
            color: #999;
        }
        .error-marker {
            color: #9370db;
            font-size: 18px;
            vertical-align: super;
            margin-left: 2px;
        }
        #tooltip {
            position: fixed;
            background-color: #333;
            color: white;
            padding: 10px;
            border-radius: 4px;
            font-size: 12px;
            white-space: pre;
            z-index: 1000;
            display: none;
            max-width: 400px;
            pointer-events: none;
        }
        h1 {
            color: #333;
        }
    </style>
</head>
<body>
    <h1>Grammar Coverage Report</h1>
    <div id="tooltip"></div>
    <div class="grammar">
"#;

const HTML_FOOTER: &str = r#"    </div>
    <script>
        function showTooltip(event, text) {
            event.stopPropagation();
            const tooltip = document.getElementById('tooltip');
            tooltip.textContent = text;
            tooltip.style.display = 'block';
            tooltip.style.left = (event.clientX + 10) + 'px';
            tooltip.style.top = (event.clientY + 10) + 'px';
        }
        
        function hideTooltip(event) {
            if (event) {
                event.stopPropagation();
            }
            const tooltip = document.getElementById('tooltip');
            tooltip.style.display = 'none';
        }
    </script>
</body>
</html>
"#;
