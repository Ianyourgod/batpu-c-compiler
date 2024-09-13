static void* malloc(int size);

static int main() {
    int* a = malloc(4);
    a[0] = 1;
    a[1] = 2;
    a[2] = 3;
    a[3] = 4;

    int res = 0;
    for (int i = 0; i < 4; i++) {
        res += a[i];
    }

    return res;
}

