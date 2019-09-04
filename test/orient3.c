#include <fpu_control.h>
#include "predicates.h"

REAL errA, errB, errC, errCX;

void orient3_init () 
{
int round_up = 6770, round_near = 4722;
double E = epsilon;
_FPU_SETCW(round_up); 
errA = (7.0 + 43.0*E) * E ;
errB = (3.0 + 26.0*E) * E ;
errC = (23.0 + 235.0*E) * E * E ;
errCX = (2.0 + 7.0*E) * E ;
_FPU_SETCW(round_near); 
}

REAL orient3(ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz)
REAL ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz;
{
INEXACT REAL c, abig, ahi, alo, bhi, blo, err1, err2, err3, _i, bvirt, avirt, bround, around, _j, _0;
REAL zMinorP3, zMinorC3, zMinorC2, zMinorC, zMinor3, zMinor2, zMinor, yMinorP3, yMinorC3, yMinorC2, yMinorC, yMinor3, yMinor2, yMinor, yCX, yCE, yBX, yBE, yAE, xMinorP3, xMinorC3, xMinorC2, xMinorC, xMinor3, xMinor2, xMinor, t29, t28, t27, t26, t25, t24, t23, t22, t21, t20, t19, t18, t17, t16, t15, t14, t13, t12, t11, t10, t9, t8, t7, t6, t5, t4, t3, t2, t, dczC, dcz, dcyC, dcy, dcxC, dcx, dP5, dP4, dP3, dP2, dP, dC5, dC4, dC3, dC2, dC, d5, d4, d3, d2, d, bczC, bcz, bcyC, bcy, bcxC, bcx, aczC, acz, acyC, acy, acxC, acx;
REAL zMinorB2[2], zMinorB[2], yMinorB2[2], yMinorB[2], xMinorB2[2], xMinorB[2], t53[2], t52[2], t51[2], t49[2], t48[2], t47[2], t45[2], t44[2], t43[2], t41[2], t40[2], t39[2], t37[2], t36[2], t35[2], t33[2], t32[2], t31[2];
REAL zMinorD3[256], zMinorD2[256], zMinorD[256], zMinorB3[256], yMinorD3[256], yMinorD2[256], yMinorD[256], yMinorB3[256], xMinorD3[256], xMinorD2[256], xMinorD[256], xMinorB3[256], t66[256], t65[256], t64[256], t63[256], t62[256], t61[256], t60[256], t59[256], t58[256], t57[256], t56[256], t55[256], t54[256], t50[256], t46[256], t42[256], t38[256], t34[256], t30[256], dD5[256], dD4[256], dD3[256], dD2[256], dD[256], dB5[256], dB4[256], dB3[256], dB2[256], dB[256];
int zMinorD3_len, zMinorD2_len, zMinorD_len, zMinorB3_len, yMinorD3_len, yMinorD2_len, yMinorD_len, yMinorB3_len, xMinorD3_len, xMinorD2_len, xMinorD_len, xMinorB3_len, t66_len, t65_len, t64_len, t63_len, t62_len, t61_len, t60_len, t59_len, t58_len, t57_len, t56_len, t55_len, t54_len, t50_len, t46_len, t42_len, t38_len, t34_len, t30_len, dD5_len, dD4_len, dD3_len, dD2_len, dD_len, dB5_len, dB4_len, dB3_len, dB2_len, dB_len;

bcx = (REAL)(bx - cx);
acx = (REAL)(ax - cx);
bcy = (REAL)(by - cy);
acy = (REAL)(ay - cy);
bcz = (REAL)(bz - cz);
acz = (REAL)(az - cz);
xMinor = (REAL)(bcy * acz);
xMinor2 = (REAL)(acy * bcz);
xMinor3 = (REAL)(xMinor - xMinor2);
t = Absolute(xMinor);
t2 = Absolute(xMinor2);
xMinorP3 = (REAL)(t + t2);
yMinor = (REAL)(bcz * acx);
yMinor2 = (REAL)(acz * bcx);
yMinor3 = (REAL)(yMinor - yMinor2);
t3 = Absolute(yMinor);
t4 = Absolute(yMinor2);
yMinorP3 = (REAL)(t3 + t4);
zMinor = (REAL)(bcx * acy);
zMinor2 = (REAL)(acx * bcy);
zMinor3 = (REAL)(zMinor - zMinor2);
t5 = Absolute(zMinor);
t6 = Absolute(zMinor2);
zMinorP3 = (REAL)(t5 + t6);
dcx = (REAL)(dx - cx);
dcy = (REAL)(dy - cy);
dcz = (REAL)(dz - cz);
d = (REAL)(dcx * xMinor3);
t7 = Absolute(dcx);
dP = (REAL)(t7 * xMinorP3);
d2 = (REAL)(dcy * yMinor3);
t8 = Absolute(dcy);
dP2 = (REAL)(t8 * yMinorP3);
d3 = (REAL)(dcz * zMinor3);
t9 = Absolute(dcz);
dP3 = (REAL)(t9 * zMinorP3);
d4 = (REAL)(d2 + d3);
dP4 = (REAL)(dP2 + dP3);
d5 = (REAL)(d + d4);
dP5 = (REAL)(dP + dP4);
yAE = (REAL)(errA * dP5);

if (d5 > yAE) return 1.0;
else {
 if (-d5 > yAE) return -1.0;
else {
Two_Product(bcy, acz, xMinorB[1], xMinorB[0]);
Two_Product(acy, bcz, xMinorB2[1], xMinorB2[0]);
Two_Two_Diff(xMinorB[1], xMinorB[0], xMinorB2[1], xMinorB2[0], xMinorB3[3], xMinorB3[2], xMinorB3[1], xMinorB3[0]); 
xMinorB3_len = 4;
Two_Product(bcz, acx, yMinorB[1], yMinorB[0]);
Two_Product(acz, bcx, yMinorB2[1], yMinorB2[0]);
Two_Two_Diff(yMinorB[1], yMinorB[0], yMinorB2[1], yMinorB2[0], yMinorB3[3], yMinorB3[2], yMinorB3[1], yMinorB3[0]); 
yMinorB3_len = 4;
Two_Product(bcx, acy, zMinorB[1], zMinorB[0]);
Two_Product(acx, bcy, zMinorB2[1], zMinorB2[0]);
Two_Two_Diff(zMinorB[1], zMinorB[0], zMinorB2[1], zMinorB2[0], zMinorB3[3], zMinorB3[2], zMinorB3[1], zMinorB3[0]); 
zMinorB3_len = 4;
dB_len = scale_expansion_zeroelim(xMinorB3_len, xMinorB3, dcx, dB);
dB2_len = scale_expansion_zeroelim(yMinorB3_len, yMinorB3, dcy, dB2);
dB3_len = scale_expansion_zeroelim(zMinorB3_len, zMinorB3, dcz, dB3);
dB4_len = fast_expansion_sum_zeroelim(dB3_len, dB3, dB2_len, dB2, dB4);
dB5_len = fast_expansion_sum_zeroelim(dB4_len, dB4, dB_len, dB, dB5);
yBX = estimate(dB5_len, dB5);
yBE = (REAL)(errB * dP5);
if (yBX > yBE) return 1.0;
else {
 if (-yBX > yBE) return -1.0;
else {
Two_Diff_Tail(bx, cx, bcx, bcxC);
Two_Diff_Tail(ax, cx, acx, acxC);
Two_Diff_Tail(by, cy, bcy, bcyC);
Two_Diff_Tail(ay, cy, acy, acyC);
Two_Diff_Tail(bz, cz, bcz, bczC);
Two_Diff_Tail(az, cz, acz, aczC);
t10 = (REAL)(bcy * aczC);
t11 = (REAL)(bcyC * acz);
xMinorC = (REAL)(t10 + t11);
t12 = (REAL)(acy * bczC);
t13 = (REAL)(acyC * bcz);
xMinorC2 = (REAL)(t12 + t13);
xMinorC3 = (REAL)(xMinorC - xMinorC2);
t14 = (REAL)(bcz * acxC);
t15 = (REAL)(bczC * acx);
yMinorC = (REAL)(t14 + t15);
t16 = (REAL)(acz * bcxC);
t17 = (REAL)(aczC * bcx);
yMinorC2 = (REAL)(t16 + t17);
yMinorC3 = (REAL)(yMinorC - yMinorC2);
t18 = (REAL)(bcx * acyC);
t19 = (REAL)(bcxC * acy);
zMinorC = (REAL)(t18 + t19);
t20 = (REAL)(acx * bcyC);
t21 = (REAL)(acxC * bcy);
zMinorC2 = (REAL)(t20 + t21);
zMinorC3 = (REAL)(zMinorC - zMinorC2);
Two_Diff_Tail(dx, cx, dcx, dcxC);
Two_Diff_Tail(dy, cy, dcy, dcyC);
Two_Diff_Tail(dz, cz, dcz, dczC);
t22 = (REAL)(dcx * xMinorC3);
t23 = (REAL)(dcxC * xMinor3);
dC = (REAL)(t22 + t23);
t24 = (REAL)(dcy * yMinorC3);
t25 = (REAL)(dcyC * yMinor3);
dC2 = (REAL)(t24 + t25);
t26 = (REAL)(dcz * zMinorC3);
t27 = (REAL)(dczC * zMinor3);
dC3 = (REAL)(t26 + t27);
dC4 = (REAL)(dC2 + dC3);
dC5 = (REAL)(dC + dC4);
yCX = (REAL)(yBX + dC5);
t28 = (REAL)(errCX * yBX);
t29 = (REAL)(errC * dP5);
yCE = (REAL)(t28 + t29);
if (yCX > yCE) return 1.0;
else {
 if (-yCX > yCE) return -1.0;
else {
Two_Product(bcy, aczC, t31[1], t31[0]);
Two_Product(bcyC, acz, t32[1], t32[0]);
Two_Two_Sum(t31[1], t31[0], t32[1], t32[0], t30[3], t30[2], t30[1], t30[0]); 
t30_len = 4;
Two_Product(bcyC, aczC, t33[1], t33[0]);
xMinorD_len = fast_expansion_sum_zeroelim(2, t33, t30_len, t30, xMinorD);
Two_Product(acy, bczC, t35[1], t35[0]);
Two_Product(acyC, bcz, t36[1], t36[0]);
Two_Two_Sum(t35[1], t35[0], t36[1], t36[0], t34[3], t34[2], t34[1], t34[0]); 
t34_len = 4;
Two_Product(acyC, bczC, t37[1], t37[0]);
xMinorD2_len = fast_expansion_sum_zeroelim(2, t37, t34_len, t34, xMinorD2);
xMinorD3_len = fast_expansion_diff_zeroelim(xMinorD_len, xMinorD, xMinorD2_len, xMinorD2, xMinorD3);
Two_Product(bcz, acxC, t39[1], t39[0]);
Two_Product(bczC, acx, t40[1], t40[0]);
Two_Two_Sum(t39[1], t39[0], t40[1], t40[0], t38[3], t38[2], t38[1], t38[0]); 
t38_len = 4;
Two_Product(bczC, acxC, t41[1], t41[0]);
yMinorD_len = fast_expansion_sum_zeroelim(2, t41, t38_len, t38, yMinorD);
Two_Product(acz, bcxC, t43[1], t43[0]);
Two_Product(aczC, bcx, t44[1], t44[0]);
Two_Two_Sum(t43[1], t43[0], t44[1], t44[0], t42[3], t42[2], t42[1], t42[0]); 
t42_len = 4;
Two_Product(aczC, bcxC, t45[1], t45[0]);
yMinorD2_len = fast_expansion_sum_zeroelim(2, t45, t42_len, t42, yMinorD2);
yMinorD3_len = fast_expansion_diff_zeroelim(yMinorD_len, yMinorD, yMinorD2_len, yMinorD2, yMinorD3);
Two_Product(bcx, acyC, t47[1], t47[0]);
Two_Product(bcxC, acy, t48[1], t48[0]);
Two_Two_Sum(t47[1], t47[0], t48[1], t48[0], t46[3], t46[2], t46[1], t46[0]); 
t46_len = 4;
Two_Product(bcxC, acyC, t49[1], t49[0]);
zMinorD_len = fast_expansion_sum_zeroelim(2, t49, t46_len, t46, zMinorD);
Two_Product(acx, bcyC, t51[1], t51[0]);
Two_Product(acxC, bcy, t52[1], t52[0]);
Two_Two_Sum(t51[1], t51[0], t52[1], t52[0], t50[3], t50[2], t50[1], t50[0]); 
t50_len = 4;
Two_Product(acxC, bcyC, t53[1], t53[0]);
zMinorD2_len = fast_expansion_sum_zeroelim(2, t53, t50_len, t50, zMinorD2);
zMinorD3_len = fast_expansion_diff_zeroelim(zMinorD_len, zMinorD, zMinorD2_len, zMinorD2, zMinorD3);
t55_len = scale_expansion_zeroelim(xMinorD3_len, xMinorD3, dcx, t55);
t56_len = scale_expansion_zeroelim(xMinorB3_len, xMinorB3, dcxC, t56);
t54_len = fast_expansion_sum_zeroelim(t56_len, t56, t55_len, t55, t54);
t57_len = scale_expansion_zeroelim(xMinorD3_len, xMinorD3, dcxC, t57);
dD_len = fast_expansion_sum_zeroelim(t57_len, t57, t54_len, t54, dD);
t59_len = scale_expansion_zeroelim(yMinorD3_len, yMinorD3, dcy, t59);
t60_len = scale_expansion_zeroelim(yMinorB3_len, yMinorB3, dcyC, t60);
t58_len = fast_expansion_sum_zeroelim(t60_len, t60, t59_len, t59, t58);
t61_len = scale_expansion_zeroelim(yMinorD3_len, yMinorD3, dcyC, t61);
dD2_len = fast_expansion_sum_zeroelim(t61_len, t61, t58_len, t58, dD2);
t63_len = scale_expansion_zeroelim(zMinorD3_len, zMinorD3, dcz, t63);
t64_len = scale_expansion_zeroelim(zMinorB3_len, zMinorB3, dczC, t64);
t62_len = fast_expansion_sum_zeroelim(t64_len, t64, t63_len, t63, t62);
t65_len = scale_expansion_zeroelim(zMinorD3_len, zMinorD3, dczC, t65);
dD3_len = fast_expansion_sum_zeroelim(t65_len, t65, t62_len, t62, dD3);
dD4_len = fast_expansion_sum_zeroelim(dD3_len, dD3, dD2_len, dD2, dD4);
dD5_len = fast_expansion_sum_zeroelim(dD4_len, dD4, dD_len, dD, dD5);
t66_len = fast_expansion_sum_zeroelim(dD5_len, dD5, dB5_len, dB5, t66);
if (t66[t66_len - 1] > 0.0) return 1.0; else
if (t66[t66_len - 1] < 0.0) return -1.0; else return 0.0;
}
};
}
};
}
};

}
