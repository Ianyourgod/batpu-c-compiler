#define mem(addr) (*((int*)addr))

#define setPixelX(x) mem(240) = x
#define setPixelY(y) mem(241) = y

#define setPixelXY(x, y) setPixelX(x); setPixelY(y)

#define drawPixel() mem(242) = 0
#define clearPixel() mem(243) = 0

#define drawPixelXY(x, y) setPixelXY(x, y); drawPixel()
#define clearPixelXY(x, y) setPixelXY(x, y); clearPixel()

#define pushBuffer() mem(245) = 0
#define clearBuffer() mem(246) = 0

#define getPixel() mem(244)

#define getTile(map, shifts, x, y) map[y] & shifts[x]
#define setTile(map, shifts, x, y) map[y] = map[y] | shifts[x]
#define clearTile(map, shifts, x, y) map[y] = map[y] & (~shifts[x])

#define controller() mem(255)

int tileAtPix(int* map, int* shifts, int x, int y) {
	return getTile(map, shifts, x / 4, y / 4);
}

int tileAtBottom(int* map, int* shifts, int pX, int pY) {
	return  tileAtPix(map, shifts, pX,     pY - 1) ||
			tileAtPix(map, shifts, pX + 3, pY - 1);
}

int tileAtTop(int* map, int* shifts, int pX, int pY) {
	return  tileAtPix(map, shifts, pX,     pY + 8) ||
			tileAtPix(map, shifts, pX + 3, pY + 8);
}

int tileAtLeft(int* map, int* shifts, int pX, int pY) {
	return  tileAtPix(map, shifts, pX - 1, pY    ) ||
			tileAtPix(map, shifts, pX - 1, pY + 4) ||
			tileAtPix(map, shifts, pX - 1, pY + 8);
}

int tileAtRight(int* map, int* shifts, int pX, int pY) {
	return  tileAtPix(map, shifts, pX + 4, pY    ) ||
			tileAtPix(map, shifts, pX + 4, pY + 4) ||
			tileAtPix(map, shifts, pX + 4, pY + 8);
}

void drawBox(int x, int y) {
	drawPixelXY(x,     y    );
	drawPixelXY(x + 1, y    );
	drawPixelXY(x + 2, y    );
	drawPixelXY(x + 3, y    );
	
	drawPixelXY(x,     y + 3);
	drawPixelXY(x + 1, y + 3);
	drawPixelXY(x + 2, y + 3);
	drawPixelXY(x + 3, y + 3);
	
	drawPixelXY(x,     y + 1);
	drawPixelXY(x,     y + 2);
	
	drawPixelXY(x + 3, y + 1);
	drawPixelXY(x + 3, y + 2);
}

void drawPlayer(int x, int y) {
	drawPixelXY(x,     y    );
	clearPixelXY(x + 1,  y    );
	clearPixelXY(x + 2,  y    );
	drawPixelXY(x + 3,  y    );
	
	clearPixelXY(x,     y + 1);
	drawPixelXY(x + 1,  y + 1);
	drawPixelXY(x + 2,  y + 1);
	clearPixelXY(x + 3,  y + 1);
	
	clearPixelXY(x,     y + 2);
	drawPixelXY(x + 1,  y + 2);
	drawPixelXY(x + 2,  y + 2);
	clearPixelXY(x + 3,  y + 2);
	
	drawPixelXY(x,     y + 3);
	drawPixelXY(x + 1,  y + 3);
	drawPixelXY(x + 2,  y + 3);
	drawPixelXY(x + 3,  y + 3);
	
	clearPixelXY(x,     y + 4);
	drawPixelXY(x + 1,  y + 4);
	drawPixelXY(x + 2,  y + 4);
	clearPixelXY(x + 3,  y + 4);
	
	drawPixelXY(x,     y + 5);
	clearPixelXY(x + 1,  y + 5);
	clearPixelXY(x + 2,  y + 5);
	drawPixelXY(x + 3,  y + 5);
	
	drawPixelXY(x,     y + 6);
	clearPixelXY(x + 1,  y + 6);
	clearPixelXY(x + 2,  y + 6);
	drawPixelXY(x + 3,  y + 6);
	
	clearPixelXY(x,     y + 7);
	drawPixelXY(x + 1,  y + 7);
	drawPixelXY(x + 2,  y + 7);
	clearPixelXY(x + 3,  y + 7);
}

void render(int* map, int* shifts, int pX, int pY) {
	clearBuffer();
	
	int pixelX = 0;
	for (int x = 0; x < 8; x += 1) {
		int pixelY = 0;
		for (int y = 0; y < 8; y += 1) {
			if (getTile(map, shifts, x, y))
				drawBox(pixelX, pixelY);
			
			pixelY += 4;
		}
		pixelX += 4;
	}
	
	drawPlayer(pX, pY);

	pushBuffer();
}

void main() {
	int shifts[8];
	shifts[0] = 128;
	shifts[1] = 64;
	shifts[2] = 32;
	shifts[3] = 16;
	shifts[4] = 8;
	shifts[5] = 4;
	shifts[6] = 2;
	shifts[7] = 1;
	
	int map[8];
	
	for (int x = 0; x < 8; x += 1) {
		for (int y = 0; y < 8; y += 1) {
			if (y == 0)
				setTile(map, shifts, x, y);
			else
				clearTile(map, shifts, x, y);
		}
	}
	
	clearTile(map, shifts, 1, 1);
	
	int pX = 0;
	int pY = 8;
	
	int pVY = 128;
	
	int grounded = 0;
	
	while (1) {
		if (tileAtBottom(map, shifts, pX, pY)) {
			pVY = 128;
			grounded = 1;
		} else {
			pVY -= 1;
			grounded = 0;
		}
		
		int input = controller();
		
		if ((input & 4) && !tileAtRight(map, shifts, pX, pY))
			pX += 1;
		
		if ((input & 1) && !tileAtLeft(map, shifts, pX, pY))
			pX -= 1;

		if (grounded && (input & 16) && !tileAtTop(map, shifts, pX, pY)) {
			pVY = 128 + 3;
			grounded = 0;
		}
		
		if (pVY > 128) {
			pY += pVY - 128;
			while (tileAtTop(map, shifts, pX, pY - 1))
				pY -= 1;
		} else if (pVY < 128) {
			pY -= 128 - pVY;
			while (tileAtBottom(map, shifts, pX, pY + 1)) {
				pY += 1;
				grounded = 1;
			}
		}
		
		render(map, shifts, pX, pY);
	}
}

void goof_around(int h) {
	
}