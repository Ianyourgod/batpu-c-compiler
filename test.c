static int mem_write(int addr, char data);

static int main() {
    mem_write(249, 'a');

    char s[10] = { 'h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd' };

    for (int i = 0; i < 10; i++) {
        mem_write(247, s[i]);
    }

    mem_write(248, 'a');

    return 0;
}


int mem_write(int addr, char data)
{
    return 0;
}