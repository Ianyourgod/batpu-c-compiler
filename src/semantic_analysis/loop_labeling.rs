use crate::parser::nodes;
use super::super::errors;

pub struct LoopLabeling {
    program: nodes::Program,
    unique_counter: u32,
}

impl LoopLabeling {
    pub fn new(program: nodes::Program) -> LoopLabeling {
        LoopLabeling {
            program,
            unique_counter: 0,
        }
    }

    pub fn resolve(&mut self) -> Result<nodes::Program, errors::Error> {
        let mut statements: Vec<nodes::Declaration> = Vec::with_capacity(self.program.statements.len());

        for decl in self.program.statements.clone() {
            let func = match decl {
                nodes::Declaration::VarDecl(_, _) |
                nodes::Declaration::Empty(_) |
                nodes::Declaration::StructDecl(_, _) => { statements.push(decl); continue },
                nodes::Declaration::FuncDecl(fn_decl, _) => fn_decl,
            };

            let mut body: Vec<nodes::BlockItem> = Vec::new();
            let mut current_label = None;

            for instr in &func.body {
                body.push(self.resolve_block_item(instr, &mut current_label)?);
            }

            let line = func.line;

            let func = nodes::FuncDecl {
                name: func.name.clone(),
                params: func.params.clone(),
                body,
                storage_class: func.storage_class.clone(),
                ty: func.ty.clone(),
                line: line,
            };

            statements.push(nodes::Declaration::FuncDecl(func, line));
        }

        Ok(nodes::Program {
            statements,
        })
    }

    fn generate_unique_name(&mut self) -> String {
        let name = format!("loop.{}", self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, current_label: &Option<&str>) -> Result<nodes::BlockItem, errors::Error> {
        Ok(match stmt {
            nodes::BlockItem::Statement(ref inner_stmt, line) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, current_label)?, *line),
            nodes::BlockItem::Declaration(ref decl, line) => nodes::BlockItem::Declaration(decl.clone(), *line),
        })
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, current_label: &Option<&str>) -> Result<nodes::Statement, errors::Error> {
        Ok(match stmt {
            nodes::Statement::If(ref expr, ref block, ref else_, line) => nodes::Statement::If(
                expr.clone(),
                Box::new(self.resolve_statement(block, current_label)?),
                Box::new(if else_.is_some() { Some(self.resolve_statement(&else_.clone().unwrap(), current_label)?) } else { None }),
                *line),
            nodes::Statement::While(ref expr, ref block, _, line) => {
                let label = self.generate_unique_name();
                nodes::Statement::While(expr.clone(), Box::new(self.resolve_statement(block,  &Some(&label))?), label, *line)
            },
            nodes::Statement::DoWhile(ref block, ref expr, _, line) => {
                let label = self.generate_unique_name();
                nodes::Statement::DoWhile(Box::new(self.resolve_statement(block, &Some(&label))?), expr.clone(), label, *line)
            }
            nodes::Statement::For(init, sec, thr, body, _, line) => {
                let label = self.generate_unique_name();
                nodes::Statement::For(init.clone(), sec.clone(), thr.clone(), Box::new(self.resolve_statement(body, &Some(&label))?), label, *line)
            },
            nodes::Statement::Break(_, line) => {
                let cl = match current_label {
                    Some(lbl) => lbl,
                    None => return Err(errors::Error::new(errors::ErrorType::Error, String::from("Encountered break statement outside of loop"), *line)),
                };
                nodes::Statement::Break(cl.to_string(), *line)
            },
            nodes::Statement::Continue(_, line) => {
                let cl = match current_label {
                    Some(lbl) => lbl,
                    None => return Err(errors::Error::new(errors::ErrorType::Error, String::from("Encountered continue statement outside of loop"), *line)),
                };
                nodes::Statement::Continue(cl.to_string(), *line)
            },
            nodes::Statement::Compound(ref block, line) => {
                let mut new_block = Vec::new();
                for item in block {
                    new_block.push(self.resolve_block_item(item, current_label)?);
                }
                nodes::Statement::Compound(new_block, *line)
            },
            nodes::Statement::Return(_, _) | nodes::Statement::Expression(_, _) | nodes::Statement::Empty(_) => stmt.clone(),
        })
    }
}