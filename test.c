static void* malloc(int size);
static void free(void* pos);

static int main() {
    int* a = malloc(4);
    int* b = malloc(6);

    *a = 10;
    *b = 20;

    free(a);
    free(b);

    return *a + *b;
}