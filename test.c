static void* malloc(int size);
static void free(void* pos);

typedef struct {
    int a;
} A;

static int main() {
    A a = {1};

    return a.a;
}