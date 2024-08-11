mod lexer;
mod parser;
mod code_gen;
mod emitter;

fn main() {
    let input = "int main() { return 5; }".to_string();
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();
    let code_gen = code_gen::CodeGen::new(program);
    let assembly = code_gen.generate();
    let emitter = emitter::Emitter::new(assembly);
    let output = emitter.emit();

    // write to file
    std::fs::write("output.as", output).expect("Failed to write to file");
}
