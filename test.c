static void mem_write(int addr, int data);
static int mem_read(int addr);

static int __mult(int a, int b);

static int main() {
    int val = 0;
    int prev = 0;
    int used_prev = 0;

    while (1) {
        int input = mem_read(255);

        if (input == 2) {
            val--;
            mem_write(250, val);
            while (mem_read(255) == 2);
        } else if (input == 8) {
            val++;
            mem_write(250, val);
            while (mem_read(255) == 8);
        } else if (input != 0) {
            if (used_prev) {
                return __mult(prev, val);
            }
            prev = val;
            used_prev = 1;
            val = 0;
            while (mem_read(255) != 0);
            mem_write(250, 0);
        }
    }
}

