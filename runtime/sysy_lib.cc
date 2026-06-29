#include "runtime/sysy_lib.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

/* Input & output functions */
int getint() {
  int t;
  scanf("%d", &t);
  return t;
}
int getch() {
  char c;
  scanf("%c", &c);
  return static_cast<int>(c);
}
float getfloat() {
  float n;
  scanf("%a", &n);
  return n;
}

int getarray(int a[]) {
  int n;
  scanf("%d", &n);
  for (int i = 0; i < n; i++) scanf("%d", &a[i]);
  return n;
}

int getfarray(float a[]) {
  int n;
  scanf("%d", &n);
  for (int i = 0; i < n; i++) {
    scanf("%a", &a[i]);
  }
  return n;
}
void putint(int a) { printf("%d", a); }
void putch(int a) { printf("%c", a); }
void putarray(int n, int a[]) {
  printf("%d:", n);
  for (int i = 0; i < n; i++) printf(" %d", a[i]);
  printf("\n");
}
void putfloat(float a) { printf("%a", a); }
void putfarray(int n, float a[]) {
  printf("%d:", n);
  for (int i = 0; i < n; i++) {
    printf(" %a", a[i]);
  }
  printf("\n");
}

void putf(char a[], ...) {
  va_list args;
  va_start(args, a);
  vfprintf(stdout, a, args);
  va_end(args);
}

static struct timespec timer_start, timer_end;

typedef struct {
  int h, m, s, us;
} sysy_timer_t;

#define TIMER_COUNT_MAX 1024
static sysy_timer_t timers[TIMER_COUNT_MAX] = {};
static int timer_idx = 0;

static void put_timer(const char* name, const sysy_timer_t* t) {
  printf("%s: %dH-%dM-%dS-%dus", name, t->h, t->m, t->s, t->us);
  printf("\n");
}

void starttime() { clock_gettime(CLOCK_MONOTONIC, &timer_start); }

void stoptime() {
  clock_gettime(CLOCK_MONOTONIC, &timer_end);

  // abort if timer_idx is invalid
  if (timer_idx >= TIMER_COUNT_MAX) {
    printf("Too many timers\n");
    abort();
  }

  sysy_timer_t* t = &timers[timer_idx];
  t->us += 1000000 * (timer_end.tv_sec - timer_start.tv_sec) +
           (timer_end.tv_nsec - timer_start.tv_nsec) / 1000;
  t->s += t->us / 1000000;
  t->us %= 1000000;
  t->m += t->s / 60;
  t->s %= 60;
  t->h += t->m / 60;
  t->m %= 60;

  timer_idx++;
}

void __attribute((destructor)) after_main() {
  if (timer_idx <= 0) return;
  sysy_timer_t total = {};
  for (int i = 0; i < timer_idx; i++) {
    sysy_timer_t* t = &timers[i];
    put_timer("Timer", t);
    total.us += t->us;
    total.s += t->s;
    total.m += t->m;
    total.h += t->h;
  }
  put_timer("TOTAL", &total);
}
