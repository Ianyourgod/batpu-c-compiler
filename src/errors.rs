use color_print::cformat;

pub fn inline_warn(position: (usize, usize), message: &str, line: &str, file_name: &str) {
    let line_len = position.0.to_string().len();
    let spacing = " ".repeat(line_len);
    eprintln!("{}", cformat!("<yellow!>warning:</> <bold>{}</>\n{}<blue, bold>--></> tmpcb/{}.i:{}:{}\n{} <blue, bold>|</>\n{} <blue, bold>|</> {}\n{} <blue, bold>|\n{} <blue, bold>=</><bold>note</>: all lines are relative to the preprocessed version of your source", message, spacing, file_name, position.0, position.1, spacing, position.0, line, spacing, spacing));
}

pub fn external_warn(message: &str) {
    eprintln!("{}", cformat!("<yellow!>warning:</> <bold>{}</>", message));
}

pub fn inline_error(position: (usize, usize), message: &str, line: &str, file_name: &str) {
    let line_len = position.0.to_string().len();
    let spacing = " ".repeat(line_len);
    eprintln!("{}", cformat!("<red!>error:</> <bold>{}</>\n{}<blue, bold>--></> tmpcb/{}.i:{}:{}\n{} <blue, bold>|</>\n<blue, bold>{} |</> {}\n{} <blue, bold>|</>\n{} <blue, bold>=</> <bold>note</>: all lines are relative to the preprocessed version of your source", message, spacing, file_name, position.0, position.1, spacing, position.0, line, spacing, spacing));
}

#[allow(dead_code)]
pub fn external_error(message: &str) {
    eprintln!("{}", cformat!("<red!>error:</> <bold>{}</>", message));
}

#[derive(Clone)]
pub struct Error {
    pub ty: ErrorType,
    pub message: String,
    pub line: usize,
}

impl Error {
    pub fn new(ty: ErrorType, message: String, line: usize) -> Error {
        Error {
            ty,
            message,
            line,
        }
    }

    pub fn print(&self, line: &str, file_name: &str) {
        match self.ty {
            ErrorType::Warning => inline_warn((self.line, 0), &self.message, line, file_name),
            ErrorType::Error => inline_error((self.line, 0), &self.message, line, file_name),
        }
    }
}

#[derive(Clone, )]
#[allow(dead_code)]
pub enum ErrorType {
    Warning,
    Error,
}