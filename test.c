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