/*
static void mem_write(int addr, int val);

static void printf(char* str) {
    mem_write(249, 1);
    while (*str != '\0') {
        mem_write(247, (int)*str);
        mem_write(248, 1);
        str--;
    }
    mem_write(248, 1);
}

static int main() {
    char input[11] = { 'h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd', '\0' };
    printf(input);

    return 0;
}
*/

/*
static void mem_write(int addr, int val);
static void exit(int code);
static void* malloc(int size);
static void free(void* pos);
static int __mult(int a, int b);

#define NULL 0

#define ADD 0
#define SUB 1
#define MUL 2
#define INT 3

struct Node {
    int type;
    int value;
    struct Node* right;
};

static int interpret(struct Node* node) {
    if (node->type == INT) {
        return node->value;
    } else {
        int left = interpret((struct Node*) node->value);
        int right = interpret(node->right);
        if (node->type == ADD) {
            return left + right;
        } else if (node->type == SUB) {
            return left - right;
        } else if (node->type == MUL) {
            return __mult(left, right);
        }
    }
    exit(255);
}

static int main() {
    struct Node one = { INT, 1, NULL };
    struct Node two = { INT, 2, NULL };
    struct Node three = { INT, 3, NULL };
    struct Node add = { ADD, (int) &one, &two };
    struct Node ast = { MUL, (int) &add, &three };

    return interpret(&ast);
}
*/

/*
#define TOKEN_EOF 1
#define TOKEN_INT 2
#define TOKEN_ADD 3
#define TOKEN_SUB 4
#define TOKEN_MUL 5
#define TOKEN_DIV 6
#define TOKEN_LPAREN 7
#define TOKEN_RPAREN 8

static void exit(int code);
static void* malloc(int size);

struct Lexer {
    int position;
    char* input;
};

struct Token {
    int type;
    char* value;
};


static int isdigit(char ch) {
    return ch == '1' || ch == '2';
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
                if (self->position != 0) {
                    exit(((int) (self->input[self->position])) + self->position);
                }
                token->value[0] = self->input[self->position];
                self->position = self->position + 1;
            }
            break;
        }
        if (self->input[self->position] == '+') {
            token->type = TOKEN_ADD;
            self->position++;
            break;
        }
    }

    return token;
}

static int main() {
    char input[2] = { '1', '\0' };
    struct Lexer lexa = { 0, input };

    lexer_get_next_token(&lexa);

    return lexer_get_next_token(&lexa)->type;
}
*/

static void i_do_nothing() {
    return;
}

static int four() {
    return 4;
}

static int i_do_something() {
    int a = four();
    i_do_nothing();
    return a;
}

static int three() {
    return 3;
}

static int main() {
    int la_di_da_di_da = three();
    i_do_something();
    return la_di_da_di_da;
}