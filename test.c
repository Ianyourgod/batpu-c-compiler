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

static void mem_write(int addr, int val);

static void draw_pixel(int x, int y) {
    mem_write(240, x);
    mem_write(241, y);
    mem_write(242, 1);
}

static void draw_buffer() {
    mem_write(245, 1);
}

static void clear_buffer() {
    mem_write(246, 1);
}

static int ton_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j);

static int main() {
    ton_of_args(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
}

static int ton_of_args(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    return a + b + c + d + e + f + g + h + i + j;
}