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
#define NULL 0

static void mem_write(int addr, int val);
static int mem_read(int addr);
static void exit(int code);
static void* malloc(int size);
static void free(void* pos);
static int __mult(int a, int b);

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
        if (isdigit(self->input[self->position])) {
            token->type = TOKEN_INT;
            while (isdigit(self->input[self->position])) {
                token->value[0] = self->input[self->position];
                self->position++;
            }
            break;
        }
        if (self->input[self->position] == '+') {
            token->type = TOKEN_ADD;
            self->position++;
            break;
        }
        if (self->input[self->position] == '-') {
            token->type = TOKEN_SUB;
            self->position++;
            break;
        }
        if (self->input[self->position] == '*') {
            token->type = TOKEN_MUL;
            self->position++;
            break;
        }
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
    if (self->current_token->type == TOKEN_INT) {
        struct Node* node = (struct Node*)malloc(sizeof(struct Node));
        node->type = TOKEN_INT;
        node->value = atoi(self->current_token->value);
        parser_eat(self);
        return node;
    }
    if (self->current_token->type == TOKEN_LPAREN) {
        parser_eat(self);
        struct Node* node = parser_expr(self);
        parser_eat(self);
        return node;
    }
    return (struct Node*) self->current_token;
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

/*
static void free_node(struct Node* node) {
    if (node->type != TOKEN_INT) {
        free_node(node->left);
        free_node(node->right);
    }
    free(node);
}
*/

static int interpret(struct Node* node) {
    if (node->type == TOKEN_INT) {
        return node->value;
    } else {
        int left = interpret(node->left);
        int right = interpret(node->right);
        if (node->type == TOKEN_ADD) {
            return left + right;
        } else if (node->type == TOKEN_SUB) {
            return left - right;
        } else if (node->type == TOKEN_MUL) {
            return __mult(left, right);
        }/* else if (node->type == TOKEN_DIV) {
            //return left / right;
        }
    */
    }
    //printf("Invalid syntax\n");
    
    //exit(255);
}

static int main() {
    // get user input
    
    //printf("Enter an expression: ");

    //char input[100];
    //fgets(input, 100, stdin);

    //exit(0);

    char input[6] = { '1', '+', '2', '*', '5', '\0' };

    // tokenize
    struct Lexer lexer = {0, input};

    // parse the input
    struct Parser parser = { &lexer, lexer_get_next_token(&lexer)};

    struct Node* ast = parser_expr(&parser);

    return interpret(ast);
}