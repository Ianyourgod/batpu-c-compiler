use std::{collections::HashMap, process::exit};
use color_print::cformat;

mod lexer;
mod parser;
mod semantic_analysis;
mod tacky;
mod optimizations;
mod code_gen;
mod emitter;

macro_rules! warn {
    ($($arg:tt)*) => {
        eprintln!("{}", cformat!("<yellow>[WARNING] {}</>", format_args!($($arg)*)));
    };
}

#[allow(unused_macros)]
macro_rules! error {
    ($($arg:tt)*) => {
        eprintln!("{}", cformat!("<red>[ERROR] {}</>", format_args!($($arg)*)));
        exit(0);
    };
}

#[derive(Clone)]
pub struct Settings {
    pub link_files: Vec<String>,
    pub input_names: Vec<String>,
    pub output_name: String,
    pub do_not_link: bool,
    pub do_not_assemble: bool,
    pub include_comments: bool,
    pub dont_optimize: bool,
}

fn parse_args() -> Settings {
    let mut args = std::env::args();

    let mut input_names = Vec::new();

    let mut link_files = Vec::new();
    let mut output_name = String::new();
    let mut do_not_link = false;
    let do_not_assemble = false;
    let mut include_comments = false;
    let mut dont_optimize = false;

    args.nth(0);
    while 1 <= args.len() {
        let arg = args.nth(0).unwrap();

        match arg.as_str() {
            "-o" => {
                output_name = args.nth(0).unwrap();
            },
            "-c" => {
                do_not_link = true;
            },
            "-l" => {
                link_files.push(args.nth(0).unwrap());
            },
            "-n" => {
                warn!("The -n flag is deprecated, as the assembler has been removed.");
                //do_not_assemble = true;
            },
            "-f" => {
                include_comments = true;
            },
            "-O0" => {
                dont_optimize = true;
            },
            _ => {
                input_names.push(arg);
            },
        }
    }

    if input_names.is_empty() {
        panic!("No input files provided");
    }

    output_name = if output_name.is_empty() {
        "output.as".to_string()
    } else {
        output_name
    };

    Settings {
        link_files,
        output_name,
        input_names,
        do_not_link,
        do_not_assemble,
        include_comments,
        dont_optimize,
    }
}

fn _test_lexer(input: String) {
    let mut lexer = lexer::Lexer::new(input);
    let mut current_token = lexer.next_token();
    while current_token != lexer::TokenType::EOF {
        current_token = lexer.next_token();
    }
}

/*
fn assemble(inputs: Vec<String>, object_files: Vec<String>, output_name: String, do_not_link: bool) {
    // create dir called "tmpcb" to store files
    std::fs::create_dir_all(".tmpcb").expect("Failed to create directory");

    let mut binding = std::process::Command::new("./bsm");
    let cmd = binding
        .arg("-o")
        .arg(output_name);

    if do_not_link {
        cmd.arg("-c");
    }

    for input in inputs.iter().enumerate() {
        // write to file
        std::fs::write(format!(".tmpcb/input{}.as", input.0), input.1).expect("Failed to write to file");

        cmd.arg(format!(".tmpcb/input{}.as", input.0));
    }

    for object_file in object_files.iter().enumerate() {
        cmd.arg("-l");
        cmd.arg(object_file.1);
    }

    let out = cmd.output().expect("Failed to execute command");

    if out.stdout.len() > 0 {
        println!("ASSEMBLER OUTPUT: {}", String::from_utf8(out.stdout).unwrap());
    }

    // check if it errored
    if !out.status.success() {
        println!("ASSEMBLER ERROR: {}", String::from_utf8(out.stderr).unwrap());
        exit(1);
    }

    // remove dir
    if !do_not_link {
        std::fs::remove_dir_all(".tmpcb").expect("Failed to remove directory");
    }
}
*/

fn compile(input_file: &String, args: Settings) -> String {
    // preprocess input
    // call "gcc -E -P input_file -o .tmpbc/input_file.i"
    let mut binding = std::process::Command::new("gcc");
    let out = binding
        .arg("-E")
        .arg("-P")
        .arg(input_file)
        .arg("-o")
        .arg(format!(".tmpcb/{}.i", input_file))
        .output()
        .expect("Failed to execute command");

    // check if it errored
    if !out.status.success() {
        println!("PREPROCESSOR ERROR: {}", String::from_utf8(out.stderr).unwrap());
        exit(1);
    }

    // read preprocessed file
    let input = std::fs::read_to_string(format!(".tmpcb/{}.i", input_file)).expect("Failed to read file");

    let lexer = lexer::Lexer::new(input);
    let mut parser = parser::Parser::new(lexer);
    let program = parser.parse_program();

    //println!("{:#?}", program.statements.get(15));
    
    #[allow(unused_variables)]
    let (program, symbol_table, type_table) = semantic_analysis::resolve(program);

    //println!("{:#?}", program);

    let mut tacky = tacky::Tacky::new(program, symbol_table.clone(), type_table.clone());
    let program = tacky.emit();
    let aliased_vars = HashMap::new();

    //println!("{:#?}", program);

    let (program, aliased_vars) = if !args.dont_optimize {
        optimizations::optimize(program)
    } else { (program, aliased_vars) };

    //println!("{:#?}", program);

    let assembly = code_gen::convert(program, symbol_table, type_table, aliased_vars, !args.dont_optimize);

    //println!("{:#?}", assembly);

    let emitter = emitter::Emitter::new(assembly);
    let output = emitter.emit(args.include_comments);
    output
}

fn main() {
    // get file name from command line, read file, compile it
    let args = parse_args();

    let mut outputs = Vec::new();
    for input_name in &args.input_names {
        let output = compile(input_name, args.clone());

        outputs.push(output);
    }

    /*if !args.do_not_assemble {
        
        
        assemble(outputs, args.link_files, args.output_name, args.do_not_link);
    } else { */
        // write it to file
        std::fs::write(args.output_name, outputs.join("\n")).expect("Failed to write to file");
    //}
}
