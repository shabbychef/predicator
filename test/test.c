/************************************************
 ** orient3.c
 **
 ** Aleksandar Nanevski
 **
 ** Reads a file with points and runs
 ** orient3d on them
 ************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>      
#include <fpu_control.h>
#include "predicates.h"

REAL orient2d (REAL *pa, REAL *pb, REAL *pc);
REAL incircle (REAL *pa, REAL *pb, REAL *pc, REAL *pd);
REAL orient3d (REAL *pa, REAL *pb, REAL *pc, REAL *pd);
REAL insphere (REAL *pa, REAL *pb, REAL *pc, REAL *pd, REAL *pe);

int orient2 (REAL ax, REAL ay, REAL bx, REAL by, REAL cx, REAL cy);
int insphere2 (REAL ax, REAL ay, REAL bx, REAL by, REAL cx, REAL cy, REAL dx, REAL dy);
int orient3 (REAL ax, REAL ay, REAL az, REAL bx, REAL by, REAL bz, REAL cx, REAL cy, REAL cz, REAL dx, REAL dy, REAL dz);
int insphere3 (REAL ax, REAL ay, REAL az, REAL bx, REAL by, REAL bz, REAL cx, REAL cy, REAL cz, REAL dx, REAL dy, REAL dz, REAL ex, REAL ey, REAL ez);
int minsphere3 (REAL ax, REAL ay, REAL az, REAL bx, REAL by, REAL bz, REAL cx, REAL cy, REAL cz, REAL dx, REAL dy, REAL dz);


void formatOrient2 (char *line, long *i, double a[], double b[], double c[], int *r)
{
  *i = strtol(line, &line, 10);
  a[0] = strtod(line, &line);
  a[1] = strtod(line, &line);
  b[0] = strtod(line, &line);
  b[1] = strtod(line, &line);
  c[0] = strtod(line, &line);
  c[1] = strtod(line, &line);
  *r = atoi(line);
}


void formatInSphere2 (char *line, long *i, double a[], double b[], double c[], double d[], int *r)
{
  *i = strtol(line, &line, 10);
  a[0] = strtod(line, &line);
  a[1] = strtod(line, &line);
  b[0] = strtod(line, &line);
  b[1] = strtod(line, &line);
  c[0] = strtod(line, &line);
  c[1] = strtod(line, &line);
  d[0] = strtod(line, &line);
  d[1] = strtod(line, &line);
  *r = atoi(line);
}



void formatOrient3 (char *line, long *i, double a[], double b[], double c[], double d[], int *r)
{
  *i = strtol(line, &line, 10);
  a[0] = strtod(line, &line);
  a[1] = strtod(line, &line);
  a[2] = strtod(line, &line);
  b[0] = strtod(line, &line);
  b[1] = strtod(line, &line);
  b[2] = strtod(line, &line);
  c[0] = strtod(line, &line);
  c[1] = strtod(line, &line);
  c[2] = strtod(line, &line);
  d[0] = strtod(line, &line);
  d[1] = strtod(line, &line);
  d[2] = strtod(line, &line);
  *r = atoi(line);
}




void formatInSphere3 (char *line, long *i, double a[], double b[], double c[], double d[], double e[], int *r)
{
  *i = strtol(line, &line, 10);
  a[0] = strtod(line, &line);
  a[1] = strtod(line, &line);
  a[2] = strtod(line, &line);
  b[0] = strtod(line, &line);
  b[1] = strtod(line, &line);
  b[2] = strtod(line, &line);
  c[0] = strtod(line, &line);
  c[1] = strtod(line, &line);
  c[2] = strtod(line, &line);
  d[0] = strtod(line, &line);
  d[1] = strtod(line, &line);
  d[2] = strtod(line, &line);
  e[0] = strtod(line, &line);
  e[1] = strtod(line, &line);
  e[2] = strtod(line, &line);
  *r = atoi(line);
}


void formatMinSphere3 (char *line, long *i, double a[], double b[], double c[], double d[], int *r)
{
  *i = strtol(line, &line, 10);
  a[0] = strtod(line, &line);
  a[1] = strtod(line, &line);
  a[2] = strtod(line, &line);
  b[0] = strtod(line, &line);
  b[1] = strtod(line, &line);
  b[2] = strtod(line, &line);
  c[0] = strtod(line, &line);
  c[1] = strtod(line, &line);
  c[2] = strtod(line, &line);
  d[0] = strtod(line, &line);
  d[1] = strtod(line, &line);
  d[2] = strtod(line, &line);
  *r = atoi(line);
}





main()
{
  double a[3], b[3], c[3], d[3], e[3], rr;
  double a1[3], b1[2], c1[3];
  FILE *in, *out;
  char buf[512];
  int i, r, s;
  long n;

  int y;
  /* FPU constrol word for double precision (64 bits) and overflow and underflow exceptions */
  int double_mode = 4722;
  /* FPU constrol word for extended precision (80 bits) and overflow and underflow exceptions */
  int extended_mode = double_mode+256; 

  /* set FPU control word for double precision */
  _FPU_SETCW(double_mode);
  exactinit();


  printf ("Testing Shewchuk's Orient2\n");

  in = fopen("orient.2d", "r");
  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    rr = orient2d(a, b, c);
    if (((rr > 0.0) && (r > 0))|| 
        ((rr < 0.0) && (r < 0))||
        ((rr == 0.0) && (r == 0)));
      else
    {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);
  
  
  printf ("Testing Orient2\n");

  orient2_init();
  in = fopen("orient.2d", "r");
  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    s = orient2(a[0], a[1], b[0], b[1], c[0], c[1]);
    if (s != r) {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);


  printf ("Testing Shewchuk's InCircle\n");

  insphere2_init();
  in = fopen("insphere.2d", "r");
  while (fgets(buf, 512, in)) {
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    rr = incircle(a, b, c, d);
    if (((rr > 0.0) && (r > 0))|| 
        ((rr < 0.0) && (r < 0))||
        ((rr == 0.0) && (r == 0)));
    else {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);



  printf ("Testing InSphere2\n");

  insphere2_init();
  in = fopen("insphere.2d", "r");
  while (fgets(buf, 512, in)) {
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    s = insphere2(a[0], a[1], b[0], b[1], c[0], c[1], d[0], d[1]);
    if (s != r) {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);


  printf ("Testing Shewhuck's Orient3\n");

  orient3_init();
  in = fopen("orient.3d", "r");
  while (fgets(buf, 512, in)) {
    formatOrient3 (buf, &n, a, b, c, d, &r);
    rr = orient3d(a, b, c, d);
    if (((rr > 0.0) && (r > 0))|| 
        ((rr < 0.0) && (r < 0))||
        ((rr == 0.0) && (r == 0)));
    else {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);



  printf ("Testing Orient3\n");

  orient3_init();
  in = fopen("orient.3d", "r");
  while (fgets(buf, 512, in)) {
    formatOrient3 (buf, &n, a, b, c, d, &r);
    s = orient3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], d[0], d[1], d[2]);
    if (s != r) {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);


  printf ("Testing Shewhuck's InSphere\n");

  orient3_init();
  in = fopen("insphere.3d", "r");
  while (fgets(buf, 512, in)) {
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    rr = insphere(a, b, c, d, e);
    if (((rr > 0.0) && (r > 0))|| 
        ((rr < 0.0) && (r < 0))||
        ((rr == 0.0) && (r == 0)));
    else {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);


  printf ("Testing InSphere3\n");

  orient3_init();
  in = fopen("insphere.3d", "r");
  while (fgets(buf, 512, in)) {
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    s = insphere3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], d[0], d[1], d[2], e[0], e[1], e[2]);
    if (s != r) {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);



  printf ("Testing MinSphere3\n");

  minsphere3_init();
  in = fopen("minsphere.3d", "r");
  while (fgets(buf, 512, in)) {
    formatOrient3 (buf, &n, a, b, c, d, &r);
    s = minsphere3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], d[0], d[1], d[2]);
    if (s != r) {
      printf ("Error at the test tuple %d\n", n);
      exit(-1);
    }
  }
  printf ("OK\n");
  fclose(in);
  
}
