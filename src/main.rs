mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod code_gen;
mod emitter;

fn _test_lexer(input: String) {
    let mut lexer = lexer::Lexer::new(input);
    let mut current_token = lexer.next_token();
    while current_token != lexer::TokenType::EOF {
        println!("{:?}", current_token);
        current_token = lexer.next_token();
    }
}

fn compile(input: String) -> String {
    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();
    #[allow(unused_variables)]
    let (program, symbol_table) = semantic_analysis::resolve(program);
    let mut tacky = tacky::Tacky::new(program);
    let program = tacky.emit();
    let assembly = code_gen::convert(program);
    let emitter = emitter::Emitter::new(assembly);
    let output = emitter.emit();
    output
}

fn main() {
    // get file name from command line, read file, compile it
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let input = std::fs::read_to_string(&args[1]).expect("Failed to read file");

    let output = compile(input);

    // write to file
    std::fs::write("output.as", output).expect("Failed to write to file");
}
