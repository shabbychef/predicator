#include <fpu_control.h>
#include "predicates.h"

REAL errORIENTA, errORIENTB, errORIENTC, errORIENTCX;

void orient2_init () 
{
int round_up = 6770, round_near = 4722;
double E = epsilon;
_FPU_SETCW(round_up); 
errORIENTA = (3.0 + 13.0*E) * E ;
errORIENTB = (2.0 + 12.0*E) * E ;
errORIENTC = (7.0 + 42.0*E) * E * E ;
errORIENTCX = (2.0 + 7.0*E) * E ;
_FPU_SETCW(round_near); 
}

double orient2(ax, ay, bx, by, cx, cy)
REAL ax, ay, bx, by, cx, cy;
{
INEXACT REAL c, abig, ahi, alo, bhi, blo, errORIENT1, errORIENT2, errORIENT3, _i, bvirt, avirt, bround, around, _j, _0;
REAL yCX, yCE, yBX, yBE, yAE, t8, t7, t6, t5, t4, t3, t2, t, dP5, dC5, dC4, dC3, dC2, dC, d5, d4, d3, d2, d, abyC, aby, abxC, abx;
REAL t16[2], t15[2], t14[2], t12[2], t11[2], t10[2], dB4[2], dB2[2];
REAL t17[256], t13[256], t9[256], dD5[256], dD4[256], dD2[256], dB5[256];
int t17_len, t13_len, t9_len, dD5_len, dD4_len, dD2_len, dB5_len;

abx = (REAL)(ax - bx);
aby = (REAL)(ay - by);
d = (REAL)(cx - bx);
d2 = (REAL)(aby * d);
d3 = (REAL)(by - cy);
d4 = (REAL)(abx * d3);
d5 = (REAL)(d2 + d4);
t = Absolute(d2);
t2 = Absolute(d4);
dP5 = (REAL)(t + t2);
yAE = (REAL)(errORIENTA * dP5);

if (d5 > yAE) return 1.0;
else {
 if (-d5 > yAE) return -1.0;
else {
Two_Product(aby, d, dB2[1], dB2[0]);
Two_Product(abx, d3, dB4[1], dB4[0]);
Two_Two_Sum(dB2[1], dB2[0], dB4[1], dB4[0], dB5[3], dB5[2], dB5[1], dB5[0]); 
dB5_len = 4;
yBX = estimate(dB5_len, dB5);
yBE = (REAL)(errORIENTB * dP5);
if (yBX > yBE) return 1.0;
else {
 if (-yBX > yBE) return -1.0;
else {
Two_Diff_Tail(ax, bx, abx, abxC);
Two_Diff_Tail(ay, by, aby, abyC);
Two_Diff_Tail(cx, bx, d, dC);
t3 = (REAL)(aby * dC);
t4 = (REAL)(abyC * d);
dC2 = (REAL)(t3 + t4);
Two_Diff_Tail(by, cy, d3, dC3);
t5 = (REAL)(abx * dC3);
t6 = (REAL)(abxC * d3);
dC4 = (REAL)(t5 + t6);
dC5 = (REAL)(dC2 + dC4);
yCX = (REAL)(yBX + dC5);
t7 = (REAL)(errORIENTCX * yBX);
t8 = (REAL)(errORIENTC * dP5);
yCE = (REAL)(t7 + t8);
if (yCX > yCE) return 1.0;
else {
 if (-yCX > yCE) return -1.0;
else {
Two_Product(aby, dC, t10[1], t10[0]);
Two_Product(abyC, d, t11[1], t11[0]);
Two_Two_Sum(t10[1], t10[0], t11[1], t11[0], t9[3], t9[2], t9[1], t9[0]); 
t9_len = 4;
Two_Product(abyC, dC, t12[1], t12[0]);
dD2_len = fast_expansion_sum_zeroelim(2, t12, t9_len, t9, dD2);
Two_Product(abx, dC3, t14[1], t14[0]);
Two_Product(abxC, d3, t15[1], t15[0]);
Two_Two_Sum(t14[1], t14[0], t15[1], t15[0], t13[3], t13[2], t13[1], t13[0]); 
t13_len = 4;
Two_Product(abxC, dC3, t16[1], t16[0]);
dD4_len = fast_expansion_sum_zeroelim(2, t16, t13_len, t13, dD4);
dD5_len = fast_expansion_sum_zeroelim(dD4_len, dD4, dD2_len, dD2, dD5);
t17_len = fast_expansion_sum_zeroelim(dD5_len, dD5, dB5_len, dB5, t17);
if (t17[t17_len - 1] > 0.0) return 1.0; else
if (t17[t17_len - 1] < 0.0) return -1.0; else return 0.0;
}
};
}
};
}
};

}
