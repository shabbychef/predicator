/*****************************************************************************/
/* Additional Routines for Arbitrary Precision Floating-point Arithmetic     */
/*                                                                           */
/* Needed for the Expression Compiler                                        */
/*                                                                           */
/* Aleksandar Nanevski                                                       */
/*****************************************************************************/

#include "predicates.h"


int shrink_expansion(b, elen, e, h)                /* e and h can be the same. */
int elen;
REAL *e;
REAL b;
REAL *h;
{
  REAL Q;
  INEXACT REAL Qnew;
  int eindex;
  REAL enow;
  INEXACT REAL bvirt;
  REAL avirt, bround, around;

  Q = b;
  for (eindex = 0; eindex < elen; eindex++) {
    enow = e[eindex];
    Two_Diff(Q, enow, Qnew, h[eindex]);
    Q = Qnew;
  }
  h[eindex] = Q;
  return eindex + 1;
}



int fast_expansion_diff_zeroelim(elen, e, flen, f, h)   /* h cannot be e or f */
int elen;
REAL *e;
int flen;
REAL *f;
REAL *h;
{
  REAL Q;
  INEXACT REAL Qnew;
  INEXACT REAL hh;
  INEXACT REAL bvirt;
  REAL avirt, bround, around;
  int eindex, findex, hindex;
  REAL enow, fnow;

  enow = e[0];
  fnow = -f[0];
  eindex = findex = 0;
  if ((fnow > enow) == (fnow > -enow)) {
    Q = enow;
    enow = e[++eindex];
  } else {
    Q = fnow;
    fnow = -f[++findex];
  }
  hindex = 0;
  if ((eindex < elen) && (findex < flen)) {
    if ((fnow > enow) == (fnow > -enow)) {
      Fast_Two_Sum(enow, Q, Qnew, hh);
      enow = e[++eindex];
    } else {
      Fast_Two_Sum(fnow, Q, Qnew, hh);
      fnow = -f[++findex];
    }
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
    while ((eindex < elen) && (findex < flen)) {
      if ((fnow > enow) == (fnow > -enow)) {
        Two_Sum(Q, enow, Qnew, hh);
        enow = e[++eindex];
      } else {
        Two_Sum(Q, fnow, Qnew, hh);
        fnow = -f[++findex];
      }
      Q = Qnew;
      if (hh != 0.0) {
        h[hindex++] = hh;
      }
    }
  }
  while (eindex < elen) {
    Two_Sum(Q, enow, Qnew, hh);
    enow = e[++eindex];
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
  }
  while (findex < flen) {
    Two_Sum(Q, fnow, Qnew, hh);
    fnow = -f[++findex];
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
  }
  if ((Q != 0.0) || (hindex == 0)) {
    h[hindex++] = Q;
  }
  return hindex;
}



int mult_expansion_zeroelim(elen, e, blen, b, h)      /* h cannot be e nor b */
int elen, blen;
REAL *e, *b, *h;
{
  int i, j, k, tlen, slen, llen, newt_len;
  int *l;
  REAL **t, *newt;

  /* array of pointers to blen expansions */
  tlen = blen * sizeof (REAL *); 
  t = malloc (tlen);

  /* array of lengths of blen expansions */
  llen = blen * sizeof (int);
  l = malloc ((size_t) llen);

  /* size of the scale_expansion(e, b[i]) */
  slen = 2 * elen * sizeof (REAL);


  /* initialize */
  for (i = 0; i < blen; i++) {
    t[i] = malloc ((size_t) slen);
    l[i] = scale_expansion_zeroelim(elen, e, b[i], t[i]); 
  }

  /* distill */
  for (i = blen; i > 1; i = (i >> 1) + (i & 1)) {
    for (j = k = 0; j+1 < i; j+=2, k++) {
      newt = malloc ((size_t) (l[j] + l[j+1]) * sizeof(double));
      newt_len = fast_expansion_sum_zeroelim(l[j], t[j], l[j+1], t[j+1], newt);
      free (t[j]); free (t[j+1]);
      t[k] = newt; l[k] = newt_len;
    }
    
    if (j + 1 == i) {
      t[k] = t[j]; l[k] = l[j];
    }
  }

  /* copy the result */
  for (i = 0; i < l[0]; i++) h[i] = t[0][i];
  newt_len = l[0];
  
  free (t[0]); free (l); free (t);
  
  return newt_len;
}

/*    
  

  REAL s[256], t[256];

  h[0] = 0;

  for (eindex = 0; eindex < elen; eindex++) {
    enow = e[eindex];
    slen = scale_expansion(blen, b, enow, s);
    tlen = fast_expansion_sum(hlen, h, slen, s, t);
    h[0] = hlen = 0;
    for (tindex = 0; tindex < tlen; tindex++) {
      tt = t[tindex];
      if (tt != 0) {
	h[hlen++] = tt;
      }
    }
  }

  return hlen;
}

*/



int expansion_double(elen, e, h)
int elen;
REAL *e, *h;
{
  int eindex;
  h[0]=0;
  for (eindex = 0; eindex < elen; eindex++) {
    h[eindex] = 2.0 * e[eindex];
  }
  return elen;
}



int expansion_absolute(elen, e, h)
int elen;
REAL *e, *h;
{
  int eindex;
  if (e[0] >= 0) 
    for (eindex = 0; eindex < elen; eindex++) h[eindex] = e[eindex];
  else
    for (eindex = 0; eindex < elen; eindex++) h[eindex] = - e[eindex];

  return elen;
}



int expansion_uminus(elen, e, h)
int elen;
REAL *e, *h;
{
  int eindex;
    for (eindex = 0; eindex < elen; eindex++) h[eindex] = - e[eindex];
  return elen;
}
   
