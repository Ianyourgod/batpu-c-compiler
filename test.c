static void* malloc(int size);
static void free(void* pos);

typedef struct B {
    int a;
} A;

int main() {
    A a = {1};

    return a.a;
}