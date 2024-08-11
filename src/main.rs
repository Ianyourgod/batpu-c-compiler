mod lexer;
mod parser;
mod code_gen;
mod emitter;

fn main() {
    // get file name from command line, read file, compile it
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let input = std::fs::read_to_string(&args[1]).expect("Failed to read file");

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
