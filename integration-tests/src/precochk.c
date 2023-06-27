/*
 * Copyright (c) 2009, Armin Biere, JKU, Linz, Austria.  All rights reserved.
 *
 * Modifications:
 * Maarten Flippo, 2023: When an unsatisfied clause is encountered, exit with
 *                       code 2. This separates parsing failures from an
 *                       incorrect solution file.
 * Maarten Flippo, 2023: Do not use zlib, assume files are uncompressed. This
 *                       makes building on Windows easier for now.
 */
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "io.h"

static void die_with_code(int code, const char *msg, ...) {
  va_list ap;
  fputs("*** precochk: ", stdout);
  va_start(ap, msg);
  vfprintf(stdout, msg, ap);
  va_end(ap);
  fputc('\n', stdout);
  fflush(stdout);
  exit(code);
}

static void die(const char *msg, ...) {
  va_list ap;
  va_start(ap, msg);
  die_with_code(1, msg, ap);
  va_end(ap);
}

static void msg(const char *msg, ...) {
  va_list ap;
  fputs("c [precochk] ", stdout);
  va_start(ap, msg);
  vprintf(msg, ap);
  va_end(ap);
  fputc('\n', stdout);
  fflush(stdout);
}

int main(int argc, char **argv) {
  int m, n, ch, lit, sign, c, sat, l, *stack, *top, *end;
  signed char *vals, *mark;
  FILE *dimacs, *solution;
  if (argc != 3)
    die("usage: precochk <dimacs> <solution>");
  if (!(solution = fopen(argv[2], "r")))
    die("can not read '%s'", argv[2]);
  msg("searching solution line in '%s'", argv[2]);
SKIP1:
  ch = getc_unlocked(solution);
  if (ch == EOF)
    die("missing solution line");
  if (ch == 'c') {
    while ((ch = getc_unlocked(solution)) != '\n' && ch != EOF)
      ;
    goto SKIP1;
  }
  if (ch != 's')
    die("expected 'c' or 's'");
  if (getc_unlocked(solution) != ' ')
    die("invalid solution line");
  ch = getc_unlocked(solution);
  if ((ch != 'S' && ch != 'U') ||
      (ch == 'S' && (getc_unlocked(solution) != 'A' || getc_unlocked(solution) != 'T' ||
                     getc_unlocked(solution) != 'I' || getc_unlocked(solution) != 'S' ||
                     getc_unlocked(solution) != 'F' || getc_unlocked(solution) != 'I' ||
                     getc_unlocked(solution) != 'A' || getc_unlocked(solution) != 'B' ||
                     getc_unlocked(solution) != 'L' || getc_unlocked(solution) != 'E')) ||
      (ch == 'U' && (getc_unlocked(solution) != 'N' || getc_unlocked(solution) != 'S' ||
                     getc_unlocked(solution) != 'A' || getc_unlocked(solution) != 'T' ||
                     getc_unlocked(solution) != 'I' || getc_unlocked(solution) != 'S' ||
                     getc_unlocked(solution) != 'F' || getc_unlocked(solution) != 'I' ||
                     getc_unlocked(solution) != 'A' || getc_unlocked(solution) != 'B' ||
                     getc_unlocked(solution) != 'L' || getc_unlocked(solution) != 'E')) ||
      getc_unlocked(solution) != '\n')
    die("invalid solution line");
  msg("found solution line 's %sSATISFIABLE'", ch == 'S' ? "" : "UN");
  if (ch == 'U') {
    msg("unsatisfiable thus nothing to be done");
    fclose(solution);
    exit(20);
  }
  msg("searching dimacs header in '%s'", argv[1]);
  if (!(dimacs = fopen(argv[1], "r")))
    die("can not read '%s'", argv[1]);
SKIP2:
  ch = getc_unlocked(dimacs);
  if (ch == EOF)
    die("missing dimacs header");
  if (ch == 'c') {
    while ((ch = getc_unlocked(dimacs)) != '\n' && ch != EOF)
      ;
    goto SKIP2;
  }
  if (ch != 'p')
    die("expected 'c' or 'p'");
  if (getc_unlocked(dimacs) != ' ' || getc_unlocked(dimacs) != 'c' || getc_unlocked(dimacs) != 'n' ||
      getc_unlocked(dimacs) != 'f' || getc_unlocked(dimacs) != ' ')
    die("invalid header");
  ch = getc_unlocked(dimacs);
  if (!isdigit(ch))
    die("invalid header");
  m = ch - '0';
  while (isdigit(ch = getc_unlocked(dimacs)))
    m = 10 * m + (ch - '0');
  if (ch != ' ')
    die("invalid header");
  ch = getc_unlocked(dimacs);
  if (!isdigit(ch))
    die("invalid header");
  n = ch - '0';
  while (isdigit(ch = getc_unlocked(dimacs)))
    n = 10 * n + (ch - '0');
  if (ch != ' ' && ch != '\n')
    die("invalid header");
  msg("found dimacs header 'p cnf %d %d'", m, n);
  vals = malloc(m + 1);
  memset(vals + 1, 0, m);
  msg("searching for values in '%s'", argv[2]);
  c = 0;
SKIP3:
  ch = getc_unlocked(solution);
  if (ch == EOF) {
    if (c)
      die("zero value sentinel missing");
    else
      die("no values found");
  }
  if (ch == 'c') {
    while ((ch = getc_unlocked(solution)) != '\n' && ch != EOF)
      ;
    goto SKIP3;
  }
  if (ch != 'v')
    die("expected 'c' or 'v'");
  if (getc_unlocked(solution) != ' ')
    die("invalid value line");
  ch = getc_unlocked(solution);
VAL:
  if (ch == '-') {
    sign = -1;
    ch = getc_unlocked(solution);
    if (ch == '0')
      die("expected non zero digit");
  } else
    sign = 1;
  if (!isdigit(ch))
    die("expected digit");
  lit = ch - '0';
  while (isdigit(ch = getc_unlocked(solution)))
    lit = 10 * lit + (ch - '0');
  if (ch != ' ' && ch != '\n')
    die("expected space or new line");
  if (!lit)
    goto CHECK;
  if (lit > m)
    die("value %d exceeds maximal index %d", sign * lit, m);
  if (vals[lit])
    die("multiple values for %d", lit);
  vals[lit] = sign;
  c++;
  while (ch == ' ')
    ch = getc_unlocked(solution);
  if (ch == '\n')
    goto SKIP3;
  goto VAL;
CHECK:
  assert(c <= m);
  if (c == m)
    msg("found all %d values", c);
  else
    msg("found %d out of %d values (%d missing)", c, m, m - c);
  mark = malloc(m + 1);
  memset(mark + 1, 0, m);
  stack = top = end = 0;
SKIP4:
  ch = getc_unlocked(solution);
  if (ch == 'c') {
    while ((ch = getc_unlocked(solution)) != '\n' && ch != EOF)
      ;
    goto SKIP4;
  }
  if (ch == 'v')
    die("invalid new value block");
  if (ch != EOF)
    die("invalid line after values");
  fclose(solution);
  msg("solution file closed");
  sat = c = l = 0;
LIT:
  ch = getc_unlocked(dimacs);
  if (ch == ' ' || ch == '\n')
    goto LIT;
  if (ch == 'c') {
    while ((ch = getc_unlocked(dimacs)) != '\n' && ch != EOF)
      ;
    goto LIT;
  }
  if (ch == EOF) {
    if (l)
      die("zero literal sentinel missing");
    if (c < n)
      die("clauses missing");
    goto DONE;
  }
  if (ch == '-') {
    sign = -1;
    ch = getc_unlocked(dimacs);
    if (ch == '0')
      die("expected non zero digit");
  } else
    sign = 1;
  if (!isdigit(ch))
    die("expected digit");
  if (c == n)
    die("too many clauses");
  lit = ch - '0';
  while (isdigit(ch = getc_unlocked(dimacs)))
    lit = 10 * lit + (ch - '0');
  if (ch != ' ' && ch != '\n')
    die("expected space or new line");
  if (lit) {
    l++;
    if (!sat) {
      if (vals[lit] == sign || mark[lit] == -sign)
        sat = 1;
      else {
        if (top == end) {
          int count = top - stack, size = count ? 2 * count : 1;
          stack = realloc(stack, size * sizeof(int));
          top = stack + count;
          end = stack + size;
        }

        *top++ = lit;
        mark[lit] = sign;
      }
    }
  } else {
    c++;
    if (!sat)
      die_with_code(2, "clause %d unsatisfied", c);
    l = sat = 0;
    while (top > stack)
      mark[*--top] = 0;
  }
  goto LIT;
DONE:
  assert(c == n);
  fclose(dimacs);
  free(vals);
  free(stack);
  free(mark);
  msg("checked %d clauses", c);
  msg("satisfiable and solution correct", c);
  return 0;
}
