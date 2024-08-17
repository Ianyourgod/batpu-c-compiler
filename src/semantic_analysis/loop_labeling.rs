use crate::parser::nodes;

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

    pub fn resolve(&mut self) -> nodes::Program {
        let mut statements: Vec<nodes::FuncDecl> = Vec::new();

        for func in self.program.statements.clone() {
            let mut body: Vec<nodes::BlockItem> = Vec::new();
            let mut current_label = "".to_string();
            for instr in &func.body {
                body.push(self.resolve_block_item(instr, &mut current_label));
            }

            let func = nodes::FuncDecl {
                name: func.name.clone(),
                body,
            };

            statements.push(func);
        }

        nodes::Program {
            statements,
        }
    }

    fn generate_unique_name(&mut self) -> String {
        let name = format!("loop.{}", self.unique_counter);
        self.unique_counter += 1;
        name
    }

    fn resolve_block_item(&mut self, stmt: &nodes::BlockItem, current_label: &String) -> nodes::BlockItem {
        match stmt {
            nodes::BlockItem::Statement(ref inner_stmt) => nodes::BlockItem::Statement(self.resolve_statement(inner_stmt, current_label)),
            nodes::BlockItem::Declaration(ref decl) => nodes::BlockItem::Declaration(decl.clone()),
        }
    }

    fn resolve_statement(&mut self, stmt: &nodes::Statement, current_label: &String) -> nodes::Statement {
        match stmt {
            nodes::Statement::If(ref expr, ref block, ref else_) => nodes::Statement::If(
                expr.clone(),
                Box::new(self.resolve_statement(block, current_label)),
                Box::new(if else_.is_some() { Some(self.resolve_statement(&else_.clone().unwrap(), current_label)) } else { None })),
            nodes::Statement::While(ref expr, ref block, _) => {
                let label = self.generate_unique_name();
                nodes::Statement::While(expr.clone(), Box::new(self.resolve_statement(block, &label)), label)
            },
            nodes::Statement::DoWhile(ref block, ref expr, _) => {
                let label = self.generate_unique_name();
                nodes::Statement::DoWhile(Box::new(self.resolve_statement(block, &label)), expr.clone(), label)
            }
            nodes::Statement::For(init, sec, thr, body, _) => {
                let label = self.generate_unique_name();
                nodes::Statement::For(init.clone(), sec.clone(), thr.clone(), Box::new(self.resolve_statement(body, &label)), label)
            },
            nodes::Statement::Break(_) => nodes::Statement::Break(current_label.clone()),
            nodes::Statement::Continue(_) => nodes::Statement::Continue(current_label.clone()),
            nodes::Statement::Compound(ref block) => {
                let mut new_block = Vec::new();
                for item in block {
                    new_block.push(self.resolve_block_item(item, current_label));
                }
                nodes::Statement::Compound(new_block)
            },
            nodes::Statement::Return(_) | nodes::Statement::Expression(_) | nodes::Statement::Empty => stmt.clone(),
        }
    }
}