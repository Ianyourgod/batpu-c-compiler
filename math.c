/*
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
*/
#define TOKEN_EOF 1
#define TOKEN_INT 2
#define TOKEN_ADD 3
#define TOKEN_SUB 4
#define TOKEN_MUL 5
#define TOKEN_DIV 6
#define TOKEN_LPAREN 7
#define TOKEN_RPAREN 8
#define TOKEN_X 9
#define NULL 0

static void mem_write(int addr, int val);
static int mem_read(int addr);
static void exit(int code);
static void* malloc(int size);
static void free(void* pos);

/********************\
|     * LEXER *      |
\********************/

struct Lexer {
    int position;
    char* input;
};

struct Token {
    int type;
    char* value;
};


static int isdigit(char ch) {
    // check if the char-'0' is between 0 and 9
    int as_i = ((int) ch) - ((int) '0');
    return as_i >= 0 && as_i <= 9;
}

static int atoi(char* str) {
    return ((int) *str) - ((int) '0');
}

static struct Token* lexer_get_next_token(struct Lexer* self) {
    char* val = malloc(2);
    struct Token* token = malloc(sizeof (struct Token));
    token->type = TOKEN_EOF;
    token->value = val;

    while (self->input[self->position] != '\0') {
        char cur_char = self->input[self->position];
        if (isdigit(cur_char)) {
            token->type = TOKEN_INT;
            int i = 0;
            while (isdigit(self->input[self->position])) {
                token->value[i] = self->input[self->position];
                i++;
                self->position++;
            }
            break;
        } else if (cur_char == '+') {
            token->type = TOKEN_ADD;
        } else if (cur_char == '-') {
            token->type = TOKEN_SUB;
        } else if (cur_char == '*') {
            token->type = TOKEN_MUL;
        } else if (cur_char == 'x') {
            token->type = TOKEN_X;
        } else if (cur_char == '(') {
            token->type = TOKEN_LPAREN;
        } else if (cur_char == ')') {
            token->type = TOKEN_RPAREN;
        }

        self->position++;
        break;
    }

    return token;
}

/*
static struct Token* lexer_peek_next_token(struct Lexer* self) {
    int position = self->position;
    struct Token* token = lexer_get_next_token(self);
    self->position = position;
    return token;
}

static struct Token* lexer_eat_token(struct Lexer* self, int type) {
    struct Token* token = lexer_get_next_token(self);
    if (token->type != type) {
        //printf("expected: %d, got: %d\n", type, token.type);
        //printf("Invalid syntax\n");
        exit(255);
    }
    return token;
}

static struct Token* lexer_eat_token_value(struct Lexer* self, int type, char* value) {
    struct Token* token = lexer_get_next_token(self);
    // || strcmp(token.value, value) != 0
    if (token->type != type) {
        //printf("Invalid syntax\n");
        exit(255);
    }
    return token;
}
*/

/********************\
|     * PARSER *     |
\********************/

struct Parser {
    struct Lexer* lexer;
    struct Token* current_token;
};

struct Node {
    int type;
    int value;
    struct Node* left;
    struct Node* right;
};

static void parser_eat(struct Parser* self) {
    self->current_token = lexer_get_next_token(self->lexer);

    /*
    if (self->current_token->type == type) {
        self->current_token = lexer_get_next_token(self->lexer);
    } else {
        //printf("Invalid syntax\n");
        exit(255);
    }*/
}

static struct Node* parser_expr(struct Parser* self); // forward declaration because c is a pain

static struct Node* parser_factor(struct Parser* self) {
    int ty = self->current_token->type;
    struct Node* node;
    if (ty == TOKEN_INT) {
        node = (struct Node*)malloc(sizeof(struct Node));
        node->type = TOKEN_INT;
        node->value = atoi(self->current_token->value);
    } else if (ty == TOKEN_LPAREN) {
        parser_eat(self);
        node = parser_expr(self);
    } else if (ty == TOKEN_X) {
        node = (struct Node*)malloc(sizeof(struct Node));
        node->type = TOKEN_X;
    }
    parser_eat(self);
    return node;
}

static struct Node* parser_term(struct Parser* self) {
    struct Node* node = parser_factor(self);

    while ((self->current_token->type == TOKEN_MUL) || (self->current_token->type == TOKEN_DIV)) {
        int token_type = self->current_token->type;
        parser_eat(self);

        struct Node* right = parser_factor(self);

        struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
        new_node->type = token_type;
        new_node->left = node;
        new_node->right = right;

        node = new_node;
    }

    return node;
}

static struct Node* parser_expr(struct Parser* self) {
    struct Node* node = parser_term(self);

    while ((self->current_token->type == TOKEN_ADD) || (self->current_token->type == TOKEN_SUB)) {
        int token_type = self->current_token->type;
        parser_eat(self);

        struct Node* new_node = (struct Node*)malloc(sizeof(struct Node));
        new_node->type = token_type;
        new_node->left = node;
        new_node->right = parser_term(self);

        node = new_node;
    }

    return node;
}

static int interpret(struct Node* node, int x) {
    if (node->type == TOKEN_INT) {
        return node->value;
    } else if (node->type == TOKEN_X) {
        return x;
    } else {
        int left = interpret(node->left, x);
        int right = interpret(node->right, x);
        if (node->type == TOKEN_ADD) {
            return left + right;
        } else if (node->type == TOKEN_SUB) {
            return left - right;
        } else if (node->type == TOKEN_MUL) {
            return left * right;
        }/* else if (node->type == TOKEN_DIV) {
            //return left / right;
        }
    */
    }
}

static void graph(struct Node* ast) {
    mem_write(246, 1);

    // x^2 - y == 0
    //mem_write(240, 1);
    //mem_write(241, 1);
    //mem_write(242, 1);

    for (int x=0;x<32;x++) {
        mem_write(240, x);
        mem_write(241, interpret(ast, x));
        mem_write(242, 1);
    }

    mem_write(245, 1);
}

static int main() {
    char input[6] = { '2', '*', 'x', '+', '4', '\0' };

    // tokenize
    struct Lexer lexer = {0, input};

    // parse the input
    struct Parser parser = { &lexer, lexer_get_next_token(&lexer)};

    struct Node* ast = parser_expr(&parser);

    //return interpret(ast, 1);

    graph(ast);

    return 0;
}