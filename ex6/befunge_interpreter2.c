// Simple Interpreter for Befunge93 with abstract control

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>


#define MAX_ROWS 25
#define MAX_COLS 80

#define STACK_SIZE 100000

#define CTOI(c) c-'0'
#define NEXT_INSTRUCTION goto next_instruction


// directions
enum directions {
	up,
	down,
	left,
	right
};
typedef enum directions direction;

// stack
struct stack {
	signed long int data[STACK_SIZE];
	int top;
};
typedef struct stack stack;

// global variables
char t[MAX_ROWS*MAX_COLS];
int i, j, stringmode;
direction d;
stack s;

// stack functions
void stack_init() {
	s.top = 0;
}

void push(signed long int v) {
	s.data[s.top++] = v;
}

signed long int pop() {
	return s.top ? s.data[--s.top] : 0;
}

// input functions
void fillline(char *line, FILE *f) {
	int i = 0;

	char c = fgetc(f);
	while (c != '\n' && c != -1) {
		line[i++] = c;
		c = fgetc(f);
	}

	while (i < MAX_COLS) {
		line[i++] = ' ';
	}
}

void read_program(char* filename) {
	FILE *f;
	f = fopen(filename, "r");

	for (int i = 0; i < MAX_ROWS; i++) {
		fillline(t+i*MAX_COLS, f);
	}

	fclose(f);
}

// main, etc
void next() {
	switch (d) {
		case up:
			i = (i ? i : MAX_ROWS) - 1;
			return;
		case down:
			i = (i + 1) % MAX_ROWS;
			return;
		case left:
			j = (j ? j : MAX_COLS) - 1;
			return;
		case right:
			j = (j + 1) % MAX_COLS;
			return;
	}
}

int main(int argc, char** argv) {

	// read input file
	read_program(argv[1]);

	//initialize global variables
	stack_init();
	stringmode = 0;
	d = right;
	i = 0; j = -1;

	//time seed
	srand(time(NULL));

	//local variables
	signed long int v, v1, v2, x, y;
	char c;

	while (1) {
	next_instruction:
		next();
		c = t[i*MAX_COLS + j];
		if (stringmode && c != '"') {
			push(c);
		} else if (isdigit(c)) {
			push(CTOI(c));
		} else {
			switch (c) {
				case '+':
					v2 = pop();
					v1 = pop();
					push(v1 + v2);
					NEXT_INSTRUCTION;
				case '-':
					v2 = pop();
					v1 = pop();
					push(v1 - v2);
					NEXT_INSTRUCTION;
				case '*':
					v2 = pop();
					v1 = pop();
					push(v1 * v2);
					NEXT_INSTRUCTION;
				case '/':
					v2 = pop();
					v1 = pop();
					push(v1 / v2);
					NEXT_INSTRUCTION;
				case '%':
					v2 = pop();
					v1 = pop();
					push(v1 % v2);
					NEXT_INSTRUCTION;
				case '!':
					v = pop();
					push(v ? 0 : 1);
					NEXT_INSTRUCTION;
				case '`':
					v2 = pop();
					v1 = pop();
					push(v1 > v2);
					NEXT_INSTRUCTION;
				case '>':
					d = right;
					NEXT_INSTRUCTION;
				case '<':
					d = left;
					NEXT_INSTRUCTION;
				case '^':
					d = up;
					NEXT_INSTRUCTION;
				case 'v':
					d = down;
					NEXT_INSTRUCTION;
				case '?':
					d = rand() % 4;
					NEXT_INSTRUCTION;
				case '_':
					v = pop();
					d = v ? left : right;
					NEXT_INSTRUCTION;
				case '|':
					v = pop();
					d = v ? up : down;
					NEXT_INSTRUCTION;
				case 34:
					stringmode = 1 - stringmode;
					NEXT_INSTRUCTION;
				case ':':
					v = pop();
					push(v);
					push(v);
					NEXT_INSTRUCTION;
				case 92:
					v2 = pop();
					v1 = pop();
					push(v2);
					push(v1);
					NEXT_INSTRUCTION;
				case '$':
					pop();
					NEXT_INSTRUCTION;
				case '.':
					v = pop();
					printf("%ld ", v);
					NEXT_INSTRUCTION;
				case ',':
					v = pop();
					printf("%c", v);
					NEXT_INSTRUCTION;
				case '#':
					next();
					NEXT_INSTRUCTION;
				case 'g':
					y = pop();
					x = pop();
					push(t[y*MAX_COLS + x]);
					NEXT_INSTRUCTION;
				case 'p':
					y = pop();
					x = pop();
					v = pop();
					t[y*MAX_COLS + x] = v;
					NEXT_INSTRUCTION;
				case '&':
					scanf("%ld", &v);
					push(v);
					NEXT_INSTRUCTION;
				case '~':
					scanf("%c", &v);
					push(v);
					NEXT_INSTRUCTION;
				case ' ':
					NEXT_INSTRUCTION;
				case '@':
					return 0;
				default:
					printf("Unsupported command : %c\n", c);
			}
		}
	}
	return 1;
}
