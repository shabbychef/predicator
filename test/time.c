/************************************************
 ** time.c
 **
 ** Aleksandar Nanevski
 **
 ** Timings of everything
 ************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>      
#include <fpu_control.h>
#include "etime.h"
#include "predicates.h"

REAL orient2d (REAL *pa, REAL *pb, REAL *pc);
REAL incircle (REAL *pa, REAL *pb, REAL *pc, REAL *pd);
REAL orient3d (REAL *pa, REAL *pb, REAL *pc, REAL *pd);
REAL insphere (REAL *pa, REAL *pb, REAL *pc, REAL *pd, REAL *pe);

REAL orient2 (REAL ax, REAL ay, REAL bx, REAL by, REAL cx, REAL cy); 
REAL insphere2 (REAL ax, REAL ay, REAL bx, REAL by, REAL cx, REAL cy, REAL dx, REAL dy);
REAL orient3 (REAL ax, REAL ay, REAL az, REAL bx, REAL by, REAL bz, REAL cx, REAL cy, REAL cz, REAL dx, REAL dy, REAL dz);
REAL insphere3 (REAL ax, REAL ay, REAL az, REAL bx, REAL by, REAL bz, REAL cx, REAL cy, REAL cz, REAL dx, REAL dy, REAL dz, REAL ex, REAL ey, REAL ez);
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

/*** Shewchuk's orient2d ***/
 
double time_orient2d(void)
{
  double a[3], b[3], c[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("orient.2d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    for (i = 0; i < 10000; i++) rr = orient2d(a, b, c);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_orient2d(REAL *pa, REAL *pb, REAL *pc)
{
    return 1.0;
}

double dummy_orient2d(void)
{
  double a[3], b[3], c[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("orient.2d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    for (i = 0; i < 10000; i++) rr = const_orient2d(a, b, c);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

/*** My orient2 ***/

double time_orient2(void)
{
  double a[3], b[3], c[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;

  in = fopen("orient.2d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    for (i = 0; i < 10000; i++) s = orient2(a[0], a[1], b[0], b[1], c[0], c[1]);
  }
  Te = get_etime();  
  fclose(in);
  return (Te - Ts);
}

double const_orient2(double a1, double a2, double a3, double a4, double a5, double a6)
{
    return 1.0;
}

double dummy_orient2(void)
{
  double a[3], b[3], c[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("orient.2d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient2 (buf, &n, a, b, c, &r);
    for (i = 0; i < 10000; i++) s = const_orient2 (a[0], a[1], b[0], b[1], c[0], c[1]);
  }
  Te = get_etime();  
  fclose(in);
  return (Te - Ts);
}



/*** Shewchuk's incircle ***/
 
double time_incircle(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("insphere.2d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 1000; i++) rr = incircle(a, b, c, d);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_incircle(REAL *pa, REAL *pb, REAL *pc, REAL *pd)

{
    return 1.0;
}

double dummy_incircle(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("insphere.2d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 1000; i++) rr = const_incircle(a, b, c, d);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}



/*** My InSphere2 ***/
 
double time_insphere2(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("insphere.2d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 1000; i++) rr = insphere2 (a[0], a[1], b[0], b[1], c[0], c[1], d[0], d[1]);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_insphere2(REAL a1, REAL a2, REAL b1, REAL b2, REAL c1, REAL c2, REAL d1, REAL d2)
{
    return 1.0;
}

double dummy_insphere2(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("insphere.2d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere2 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 1000; i++) rr = const_insphere2(a[0], a[1], b[0], b[1], c[0], c[1], d[0], d[1]);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}




/*** Shewchuk's orient3d ***/
 
double time_orient3d(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("orient.3d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient3 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 10000; i++) rr = orient3d(a, b, c, d);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_orient3d(REAL *pa, REAL *pb, REAL *pc, REAL *pd)

{
    return 1.0;
}

double dummy_orient3d(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("orient.3d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient3 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 10000; i++) rr = const_orient3d(a, b, c, d);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}



/*** My orient3 ***/
 
double time_orient3(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("orient.3d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient3 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 10000; i++) rr = orient3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], d[0], d[1], d[2]);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_orient3(a0, a1, a2, b0, b1, b2, c0, c1, c2, d0, d1, d2)
REAL a0, a1, a2, b0, b1, b2, c0, c1, c2, d0, d1, d2;
{
    return 1.0;
}

double dummy_orient3(void)
{
  double a[3], b[3], c[3], d[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("orient.3d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatOrient3 (buf, &n, a, b, c, d, &r);
    for (i = 0; i < 10000; i++) rr = const_orient3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], d[0], d[1], d[2]);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}




/*** Shewchuk's insphere ***/
 
double time_insphere(void)
{
  double a[3], b[3], c[3], d[3], e[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("insphere.3d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    for (i = 0; i < 1000; i++) rr = insphere(a, b, c, d, e);
  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_insphere(REAL *pa, REAL *pb, REAL *pc, REAL *pd, REAL *pe)

{
    return 1.0;
}

double dummy_insphere(void)
{
  double a[3], b[3], c[3], d[3], e[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("insphere.3d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    for (i = 0; i < 1000; i++) rr = const_insphere(a, b, c, d, e);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}




/*** My insphere3 ***/
 
double time_insphere3(void)
{
  double a[3], b[3], c[3], d[3], e[3], rr, Ts, Te;
  char buf[512];
  FILE *in;
  int i, r, s;
  long n;
  

  in = fopen("insphere.3d", "r");
  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    for (i = 0; i < 1000; i++) rr = insphere3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], 
					      d[0], d[1], d[2], e[0], e[1], e[2]);

  }    
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}

double const_insphere3(a0, a1, a2, b0, b1, b2, c0, c1, c2, d0, d1, d2, e0, e1, e2)
REAL a0, a1, a2, b0, b1, b2, c0, c1, c2, d0, d1, d2, e0, e1, e2;
{
    return 1.0;
}

double dummy_insphere3(void)
{
  double a[3], b[3], c[3], d[3], e[3], rr, Ts, Te;
  char buf[512];
  int i, r, s;
  FILE *in;
  long n;

  in = fopen("insphere.3d", "r");

  init_etime();
  Ts = get_etime();

  while (fgets(buf, 512, in)) { 
    formatInSphere3 (buf, &n, a, b, c, d, e, &r);
    for (i = 0; i < 1000; i++) rr = const_insphere3(a[0], a[1], a[2], b[0], b[1], b[2], c[0], c[1], c[2], 
						    d[0], d[1], d[2], e[0], e[1], e[2]);
  }
  Te = get_etime();  
  fclose(in);

  return (Te - Ts);
}



main()
{
  double a[3], b[3], c[3], d[3], e[3], rr, secs, offset;
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

  

  printf ("Timing Shewchuk's Orient2\n");
  secs = time_orient2d();
  offset = dummy_orient2d();
  printf ("10000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
   

  printf ("Timing Orient2\n");
  orient2_init();
  secs = time_orient2();
  offset = dummy_orient2();
  printf ("10000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);


  printf ("Timing Shewchuk's InCircle\n");
  secs = time_incircle();
  offset = dummy_incircle();
  printf ("1000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
  


  printf ("Timing InSphere2\n");
  insphere2_init();
  secs = time_insphere2();
  offset = dummy_insphere2();
  printf ("1000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
  

  printf ("Timing Shewchuk's Orient3d\n");
  secs = time_orient3d();
  offset = dummy_orient3d();
  printf ("10000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
  
 
  printf ("Timing Orient3\n");
  orient3_init();
  secs = time_orient3();
  offset = dummy_orient3();
  printf ("10000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
  


  printf ("Timing Shewchuk's Insphere\n");
  secs = time_insphere();
  offset = dummy_insphere();
  printf ("1000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);


  printf ("Timing InSphere3\n");
  insphere3_init();
  secs = time_insphere3();
  offset = dummy_insphere3();
  printf ("1000 * secs: %.16e s  offset: %.16e s  diff: %.16e s\n", secs, offset, secs-offset);
}
