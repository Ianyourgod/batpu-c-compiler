static int mem_write(int addr, int data);

/*static int main() {
    mem_write(249, 'a');

    char s[10] = { 'h', 'e', 'l', 'l', 'o', 'w', 'o', 'r', 'l', 'd' };

    for (int i = 0; i < 10; i++) {
        mem_write(247, s[i]);
    }
    
    mem_write(248, 'a');

    return 0;
}*/

static int main() {
    int *screen_draw_x = (int *) 240;
    int *screen_draw_y = (int *) 241;

    for (int y=0;y<32;y++) {
        //for (int x=0;x<32;x++) {
            *screen_draw_x = y;
            *screen_draw_y = y;

            mem_write(242, 1);
        //}
    }

    mem_write(245, 1);
}