// Directly Threaded Interpreter for Befunge93

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#define MAX_ROWS 25
#define MAX_COLS 80
#define INDEX(i, j) i*MAX_COLS + j

#define STACK_SIZE 100000

#define CTOI(c) c - '0'

#define NEXT_PC pc += INDEX((i-i_prev), (j-j_prev))
#define NEXT_INSTRUCTION goto **(void **) (pc)


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
void *lt[MAX_ROWS*MAX_COLS], **pc;
char t[MAX_ROWS*MAX_COLS];
int i, j, i_prev, j_prev;
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
	i_prev = i;
	j_prev = j;
	switch (d) {
		case up:
			i = (i ? i : MAX_ROWS) - 1;
			break;
		case down:
			i = (i + 1) % MAX_ROWS;
			break;
		case left:
			j = (j ? j : MAX_COLS) - 1;
			break;
		case right:
			j = (j + 1) % MAX_COLS;
	}
	NEXT_PC;
}

int main(int argc, char** argv) {

	static void *label_table[128] = {
		&&error_label,//0
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,//31
		&&null_label,//32
		&&not_label,//33
		&&stringmode_label,//34
		&&bridge_label,//35
		&&pop_label,//36
		&&mod_label,//37
		&&input_int_label,//38
		&&error_label,//39
		&&error_label,//40
		&&error_label,//41
		&&multiply_label,//42
		&&add_label,//43
		&&output_char_label,//44
		&&subtract_label,//45
		&&output_int_label,//46
		&&divide_label,//47
		&&number_label,//48
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,
		&&number_label,//57
		&&dup_label,//58
		&&error_label,//59
		&&left_label,//60
		&&error_label,//61
		&&right_label,//62
		&&random_label,//63
		&&exit_label,//64
		&&error_label,//65
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,//90
		&&error_label,//91
		&&swap_label,//91
		&&error_label,//93
		&&up_label,//94
		&&horizontal_if_label,//95
		&&greater_label,//96
		&&error_label,//97
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&get_label,//103
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&put_label,//112
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,
		&&down_label,//118
		&&error_label,
		&&error_label,
		&&error_label,
		&&error_label,//122
		&&error_label,//123
		&&vertical_if_label,//124
		&&error_label,//125
		&&input_char_label,//126
		&&error_label//127
	};

	// read input file
	read_program(argv[1]);

	// replace with labels
	for (i = 0; i < MAX_ROWS; i++) {
		for (j = 0; j < MAX_COLS; j++) {
			lt[INDEX(i, j)] = label_table[(int)t[INDEX(i, j)]];
		}
	}

	//initialize global variables
	stack_init();
	d = right;
	i = 0; j = 0;
	pc = lt;

	//time seed
	srand(time(NULL));

	//local variables
	signed long int v, v1, v2, x, y;
	char c;

	while (1) {
		switch (t[0]) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			number_label:
				push(CTOI(t[INDEX(i, j)]));
				next(); NEXT_INSTRUCTION;
			case '+':
			add_label:
				push(pop() + pop());
				next(); NEXT_INSTRUCTION;
			case '-':
			subtract_label:
				v2 = pop();
				v1 = pop();
				push(v1 - v2);
				next(); NEXT_INSTRUCTION;
			case '*':
			multiply_label:
				push(pop() * pop());
				next(); NEXT_INSTRUCTION;
			case '/':
			divide_label:
				v2 = pop();
				v1 = pop();
				push(v1 / v2);
				next(); NEXT_INSTRUCTION;
			case '%':
			mod_label:
				v2 = pop();
				v1 = pop();
				push(v1 % v2);
				next(); NEXT_INSTRUCTION;
			case '!':
			not_label:
				push(pop() ? 0 : 1);
				next(); NEXT_INSTRUCTION;
			case '`':
			greater_label:
				v2 = pop();
				v1 = pop();
				push(v1 > v2);
				next(); NEXT_INSTRUCTION;
			case '>':
			right_label:
				d = right;
				next(); NEXT_INSTRUCTION;
			case '<':
			left_label:
				d = left;
				next(); NEXT_INSTRUCTION;
			case '^':
			up_label:
				d = up;
				next(); NEXT_INSTRUCTION;
			case 'v':
			down_label:
				d = down;
				next(); NEXT_INSTRUCTION;
			case '?':
			random_label:
				d = rand() % 4;
				next(); NEXT_INSTRUCTION;
			case '_':
			horizontal_if_label:
				d = pop() ? left : right;
				next(); NEXT_INSTRUCTION;
			case '|':
			vertical_if_label:
				d = pop() ? up : down;
				next(); NEXT_INSTRUCTION;
			case '"':
			stringmode_label:
				next();
				while(t[INDEX(i, j)] != '"') {
					push(t[INDEX(i, j)]);
					next();
				}
				next(); NEXT_INSTRUCTION;
			case ':':
			dup_label:
				v = pop();
				push(v);
				push(v);
				next(); NEXT_INSTRUCTION;
			case 92:
			swap_label:
				v2 = pop();
				v1 = pop();
				push(v2);
				push(v1);
				next(); NEXT_INSTRUCTION;
			case '$':
			pop_label:
				pop();
				next(); NEXT_INSTRUCTION;
			case '.':
			output_int_label:
				printf("%ld ", pop());
				next(); NEXT_INSTRUCTION;
			case ',':
			output_char_label:
				printf("%c", (char)pop());
				next(); NEXT_INSTRUCTION;
			case '#':
			bridge_label:
				next();
				next(); NEXT_INSTRUCTION;
			case 'g':
			get_label:
				y = pop();
				x = pop();
				push(t[INDEX(y, x)]);
				next(); NEXT_INSTRUCTION;
			case 'p':
			put_label:
				y = pop();
				x = pop();
				t[INDEX(y, x)] = pop();
				lt[INDEX(y, x)] = label_table[(int)t[INDEX(y, x)]];
				next(); NEXT_INSTRUCTION;
			case '&':
			input_int_label:
				scanf("%ld", &v);
				push(v);
				next(); NEXT_INSTRUCTION;
			case '~':
			input_char_label:
				scanf("%c", &c);
				push(c);
				next(); NEXT_INSTRUCTION;
			case ' ':
			null_label:
				next(); NEXT_INSTRUCTION;
			case '@':
			exit_label:
				return 0;
			default:
			error_label:
				if(isprint(t[INDEX(i, j)]))
					printf("Unsupported command : %c\n", t[INDEX(i, j)]);
				else
					printf("Not printable character : %d\n", t[INDEX(i, j)]);
				next(); NEXT_INSTRUCTION;
		}
	}
	return 1;
}
