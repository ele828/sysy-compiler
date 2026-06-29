// NOTE: intentionally not use include guard or pragma once, since sysy does not
// support header files and this file will only be used as prelude for sysy
// program.

/* Input & output functions */
int getint();
int getch();
int getarray(int a[]);
float getfloat();
int getfarray(float a[]);

void putint(int a);
void putch(int a);
void putarray(int n, int a[]);
void putfloat(float a);
void putfarray(int n, float a[]);

// void putf(char a[], ...);

// Starts and stops the timer.
// Nested calls to these functions are not allowed.
void starttime();
void stoptime();
