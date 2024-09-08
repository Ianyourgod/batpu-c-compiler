static int mem_read(int *loc);

static int mem_write(int *loc, int val);

static int main() {
    int a = 1;
    mem_write(&a, 2);
    return a;
}