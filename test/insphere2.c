#include <fpu_control.h>
#include "predicates.h"

REAL errA, errB, errC, errCX;

void insphere2_init () 
{
int round_up = 6770, round_near = 4722;
double E = epsilon;
_FPU_SETCW(round_up); 
errA = (10.0 + 76.0*E) * E ;
errB = (4.0 + 41.0*E) * E ;
errC = (41.0 + 505.0*E) * E * E ;
errCX = (2.0 + 7.0*E) * E ;
_FPU_SETCW(round_near); 
}

REAL insphere2(ax, ay, bx, by, cx, cy, dx, dy)
REAL ax, ay, bx, by, cx, cy, dx, dy;
{
INEXACT REAL c, abig, ahi, alo, bhi, blo, err1, err2, err3, _i, bvirt, avirt, bround, around, _j, _0;
REAL zMinorP3, zMinorC3, zMinorC2, zMinorC, zMinor3, zMinor2, zMinor, yMinorP3, yMinorC3, yMinorC2, yMinorC, yMinor3, yMinor2, yMinor, yCX, yCE, yBX, yBE, yAE, xMinorP3, xMinorC3, xMinorC2, xMinorC, xMinor3, xMinor2, xMinor, t34, t33, t32, t31, t30, t29, t28, t27, t26, t25, t24, t23, t22, t21, t20, t19, t18, t17, t16, t15, t14, t13, t12, t11, t10, t9, t8, t7, t6, t5, t4, t3, t2, t, dcyC, dcy, dcxC, dcx, dcSquareC3, dcSquareC2, dcSquareC, dcSquare3, dcSquare2, dcSquare, dP5, dP4, dP3, dP2, dP, dC5, dC4, dC3, dC2, dC, d5, d4, d3, d2, d, bcyC, bcy, bcxC, bcx, bcSquareC3, bcSquareC2, bcSquareC, bcSquare3, bcSquare2, bcSquare, acyC, acy, acxC, acx, acSquareC3, acSquareC2, acSquareC, acSquare3, acSquare2, acSquare;
REAL zMinorB2[2], zMinorB[2], t76[2], t75[2], t74[2], t73[2], t72[2], t71[2], t70[2], t69[2], t68[2], t66[2], t65[2], t64[2], t46[2], t45[2], t44[2], t43[2], t42[2], t41[2], t40[2], t39[2], t38[2], t37[2], t36[2], t35[2], dcSquareB2[2], dcSquareB[2], bcSquareB2[2], bcSquareB[2], acSquareB2[2], acSquareB[2];
REAL zMinorD3[256], zMinorD2[256], zMinorD[256], zMinorB3[256], yMinorD3[256], yMinorD2[256], yMinorD[256], yMinorB3[256], yMinorB2[256], yMinorB[256], xMinorD3[256], xMinorD2[256], xMinorD[256], xMinorB3[256], xMinorB2[256], xMinorB[256], t89[256], t88[256], t87[256], t86[256], t85[256], t84[256], t83[256], t82[256], t81[256], t80[256], t79[256], t78[256], t77[256], t67[256], t63[256], t62[256], t61[256], t60[256], t59[256], t58[256], t57[256], t56[256], t55[256], t54[256], t53[256], t52[256], t51[256], t50[256], t49[256], t48[256], t47[256], dcSquareD3[256], dcSquareD2[256], dcSquareD[256], dcSquareB3[256], dD5[256], dD4[256], dD3[256], dD2[256], dD[256], dB5[256], dB4[256], dB3[256], dB2[256], dB[256], bcSquareD3[256], bcSquareD2[256], bcSquareD[256], bcSquareB3[256], acSquareD3[256], acSquareD2[256], acSquareD[256], acSquareB3[256];
int zMinorD3_len, zMinorD2_len, zMinorD_len, zMinorB3_len, yMinorD3_len, yMinorD2_len, yMinorD_len, yMinorB3_len, yMinorB2_len, yMinorB_len, xMinorD3_len, xMinorD2_len, xMinorD_len, xMinorB3_len, xMinorB2_len, xMinorB_len, t89_len, t88_len, t87_len, t86_len, t85_len, t84_len, t83_len, t82_len, t81_len, t80_len, t79_len, t78_len, t77_len, t67_len, t63_len, t62_len, t61_len, t60_len, t59_len, t58_len, t57_len, t56_len, t55_len, t54_len, t53_len, t52_len, t51_len, t50_len, t49_len, t48_len, t47_len, dcSquareD3_len, dcSquareD2_len, dcSquareD_len, dcSquareB3_len, dD5_len, dD4_len, dD3_len, dD2_len, dD_len, dB5_len, dB4_len, dB3_len, dB2_len, dB_len, bcSquareD3_len, bcSquareD2_len, bcSquareD_len, bcSquareB3_len, acSquareD3_len, acSquareD2_len, acSquareD_len, acSquareB3_len;

bcx = (REAL)(bx - cx);
acx = (REAL)(ax - cx);
bcy = (REAL)(by - cy);
acy = (REAL)(ay - cy);
acSquare = (REAL)(acx * acx);
acSquare2 = (REAL)(acy * acy);
acSquare3 = (REAL)(acSquare + acSquare2);
bcSquare = (REAL)(bcx * bcx);
bcSquare2 = (REAL)(bcy * bcy);
bcSquare3 = (REAL)(bcSquare + bcSquare2);
xMinor = (REAL)(bcy * acSquare3);
xMinor2 = (REAL)(acy * bcSquare3);
xMinor3 = (REAL)(xMinor - xMinor2);
t = Absolute(xMinor);
t2 = Absolute(xMinor2);
xMinorP3 = (REAL)(t + t2);
yMinor = (REAL)(bcSquare3 * acx);
yMinor2 = (REAL)(acSquare3 * bcx);
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
dcSquare = (REAL)(dcx * dcx);
dcSquare2 = (REAL)(dcy * dcy);
dcSquare3 = (REAL)(dcSquare + dcSquare2);
d = (REAL)(dcx * xMinor3);
t7 = Absolute(dcx);
dP = (REAL)(t7 * xMinorP3);
d2 = (REAL)(dcy * yMinor3);
t8 = Absolute(dcy);
dP2 = (REAL)(t8 * yMinorP3);
d3 = (REAL)(dcSquare3 * zMinor3);
dP3 = (REAL)(dcSquare3 * zMinorP3);
d4 = (REAL)(d2 + d3);
dP4 = (REAL)(dP2 + dP3);
d5 = (REAL)(d + d4);
dP5 = (REAL)(dP + dP4);
yAE = (REAL)(errA * dP5);

if (d5 > yAE) return 1.0;
else {
 if (-d5 > yAE) return -1.0;
else {
Square(acx, acSquareB[1], acSquareB[0]);
Square(acy, acSquareB2[1], acSquareB2[0]);
Two_Two_Sum(acSquareB[1], acSquareB[0], acSquareB2[1], acSquareB2[0], acSquareB3[3], acSquareB3[2], acSquareB3[1], acSquareB3[0]); 
acSquareB3_len = 4;
Square(bcx, bcSquareB[1], bcSquareB[0]);
Square(bcy, bcSquareB2[1], bcSquareB2[0]);
Two_Two_Sum(bcSquareB[1], bcSquareB[0], bcSquareB2[1], bcSquareB2[0], bcSquareB3[3], bcSquareB3[2], bcSquareB3[1], bcSquareB3[0]); 
bcSquareB3_len = 4;
xMinorB_len = scale_expansion_zeroelim(acSquareB3_len, acSquareB3, bcy, xMinorB);
xMinorB2_len = scale_expansion_zeroelim(bcSquareB3_len, bcSquareB3, acy, xMinorB2);
xMinorB3_len = fast_expansion_diff_zeroelim(xMinorB_len, xMinorB, xMinorB2_len, xMinorB2, xMinorB3);
yMinorB_len = scale_expansion_zeroelim(bcSquareB3_len, bcSquareB3, acx, yMinorB);
yMinorB2_len = scale_expansion_zeroelim(acSquareB3_len, acSquareB3, bcx, yMinorB2);
yMinorB3_len = fast_expansion_diff_zeroelim(yMinorB_len, yMinorB, yMinorB2_len, yMinorB2, yMinorB3);
Two_Product(bcx, acy, zMinorB[1], zMinorB[0]);
Two_Product(acx, bcy, zMinorB2[1], zMinorB2[0]);
Two_Two_Diff(zMinorB[1], zMinorB[0], zMinorB2[1], zMinorB2[0], zMinorB3[3], zMinorB3[2], zMinorB3[1], zMinorB3[0]); 
zMinorB3_len = 4;
Square(dcx, dcSquareB[1], dcSquareB[0]);
Square(dcy, dcSquareB2[1], dcSquareB2[0]);
Two_Two_Sum(dcSquareB[1], dcSquareB[0], dcSquareB2[1], dcSquareB2[0], dcSquareB3[3], dcSquareB3[2], dcSquareB3[1], dcSquareB3[0]); 
dcSquareB3_len = 4;
dB_len = scale_expansion_zeroelim(xMinorB3_len, xMinorB3, dcx, dB);
dB2_len = scale_expansion_zeroelim(yMinorB3_len, yMinorB3, dcy, dB2);
dB3_len = mult_expansion_zeroelim(dcSquareB3_len, dcSquareB3, zMinorB3_len, zMinorB3, dB3);
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
t9 = (REAL)(acx * acxC);
acSquareC = (REAL)(2.0 * t9);
t10 = (REAL)(acy * acyC);
acSquareC2 = (REAL)(2.0 * t10);
acSquareC3 = (REAL)(acSquareC + acSquareC2);
t11 = (REAL)(bcx * bcxC);
bcSquareC = (REAL)(2.0 * t11);
t12 = (REAL)(bcy * bcyC);
bcSquareC2 = (REAL)(2.0 * t12);
bcSquareC3 = (REAL)(bcSquareC + bcSquareC2);
t13 = (REAL)(bcy * acSquareC3);
t14 = (REAL)(bcyC * acSquare3);
xMinorC = (REAL)(t13 + t14);
t15 = (REAL)(acy * bcSquareC3);
t16 = (REAL)(acyC * bcSquare3);
xMinorC2 = (REAL)(t15 + t16);
xMinorC3 = (REAL)(xMinorC - xMinorC2);
t17 = (REAL)(bcSquare3 * acxC);
t18 = (REAL)(bcSquareC3 * acx);
yMinorC = (REAL)(t17 + t18);
t19 = (REAL)(acSquare3 * bcxC);
t20 = (REAL)(acSquareC3 * bcx);
yMinorC2 = (REAL)(t19 + t20);
yMinorC3 = (REAL)(yMinorC - yMinorC2);
t21 = (REAL)(bcx * acyC);
t22 = (REAL)(bcxC * acy);
zMinorC = (REAL)(t21 + t22);
t23 = (REAL)(acx * bcyC);
t24 = (REAL)(acxC * bcy);
zMinorC2 = (REAL)(t23 + t24);
zMinorC3 = (REAL)(zMinorC - zMinorC2);
Two_Diff_Tail(dx, cx, dcx, dcxC);
Two_Diff_Tail(dy, cy, dcy, dcyC);
t25 = (REAL)(dcx * dcxC);
dcSquareC = (REAL)(2.0 * t25);
t26 = (REAL)(dcy * dcyC);
dcSquareC2 = (REAL)(2.0 * t26);
dcSquareC3 = (REAL)(dcSquareC + dcSquareC2);
t27 = (REAL)(dcx * xMinorC3);
t28 = (REAL)(dcxC * xMinor3);
dC = (REAL)(t27 + t28);
t29 = (REAL)(dcy * yMinorC3);
t30 = (REAL)(dcyC * yMinor3);
dC2 = (REAL)(t29 + t30);
t31 = (REAL)(dcSquare3 * zMinorC3);
t32 = (REAL)(dcSquareC3 * zMinor3);
dC3 = (REAL)(t31 + t32);
dC4 = (REAL)(dC2 + dC3);
dC5 = (REAL)(dC + dC4);
yCX = (REAL)(yBX + dC5);
t33 = (REAL)(errCX * yBX);
t34 = (REAL)(errC * dP5);
yCE = (REAL)(t33 + t34);
if (yCX > yCE) return 1.0;
else {
 if (-yCX > yCE) return -1.0;
else {
Two_Product(acx, acxC, t36[1], t36[0]);
t35[1] = 2.0 * t36[1]; t35[0] = 2.0 * t36[0];
Square(acxC, t37[1], t37[0]);
Two_Two_Sum(t35[1], t35[0], t37[1], t37[0], acSquareD[3], acSquareD[2], acSquareD[1], acSquareD[0]); 
acSquareD_len = 4;
Two_Product(acy, acyC, t39[1], t39[0]);
t38[1] = 2.0 * t39[1]; t38[0] = 2.0 * t39[0];
Square(acyC, t40[1], t40[0]);
Two_Two_Sum(t38[1], t38[0], t40[1], t40[0], acSquareD2[3], acSquareD2[2], acSquareD2[1], acSquareD2[0]); 
acSquareD2_len = 4;
acSquareD3_len = fast_expansion_sum_zeroelim(acSquareD2_len, acSquareD2, acSquareD_len, acSquareD, acSquareD3);
Two_Product(bcx, bcxC, t42[1], t42[0]);
t41[1] = 2.0 * t42[1]; t41[0] = 2.0 * t42[0];
Square(bcxC, t43[1], t43[0]);
Two_Two_Sum(t41[1], t41[0], t43[1], t43[0], bcSquareD[3], bcSquareD[2], bcSquareD[1], bcSquareD[0]); 
bcSquareD_len = 4;
Two_Product(bcy, bcyC, t45[1], t45[0]);
t44[1] = 2.0 * t45[1]; t44[0] = 2.0 * t45[0];
Square(bcyC, t46[1], t46[0]);
Two_Two_Sum(t44[1], t44[0], t46[1], t46[0], bcSquareD2[3], bcSquareD2[2], bcSquareD2[1], bcSquareD2[0]); 
bcSquareD2_len = 4;
bcSquareD3_len = fast_expansion_sum_zeroelim(bcSquareD2_len, bcSquareD2, bcSquareD_len, bcSquareD, bcSquareD3);
t48_len = scale_expansion_zeroelim(acSquareD3_len, acSquareD3, bcy, t48);
t49_len = scale_expansion_zeroelim(acSquareB3_len, acSquareB3, bcyC, t49);
t47_len = fast_expansion_sum_zeroelim(t49_len, t49, t48_len, t48, t47);
t50_len = scale_expansion_zeroelim(acSquareD3_len, acSquareD3, bcyC, t50);
xMinorD_len = fast_expansion_sum_zeroelim(t50_len, t50, t47_len, t47, xMinorD);
t52_len = scale_expansion_zeroelim(bcSquareD3_len, bcSquareD3, acy, t52);
t53_len = scale_expansion_zeroelim(bcSquareB3_len, bcSquareB3, acyC, t53);
t51_len = fast_expansion_sum_zeroelim(t53_len, t53, t52_len, t52, t51);
t54_len = scale_expansion_zeroelim(bcSquareD3_len, bcSquareD3, acyC, t54);
xMinorD2_len = fast_expansion_sum_zeroelim(t54_len, t54, t51_len, t51, xMinorD2);
xMinorD3_len = fast_expansion_diff_zeroelim(xMinorD_len, xMinorD, xMinorD2_len, xMinorD2, xMinorD3);
t56_len = scale_expansion_zeroelim(bcSquareB3_len, bcSquareB3, acxC, t56);
t57_len = scale_expansion_zeroelim(bcSquareD3_len, bcSquareD3, acx, t57);
t55_len = fast_expansion_sum_zeroelim(t57_len, t57, t56_len, t56, t55);
t58_len = scale_expansion_zeroelim(bcSquareD3_len, bcSquareD3, acxC, t58);
yMinorD_len = fast_expansion_sum_zeroelim(t58_len, t58, t55_len, t55, yMinorD);
t60_len = scale_expansion_zeroelim(acSquareB3_len, acSquareB3, bcxC, t60);
t61_len = scale_expansion_zeroelim(acSquareD3_len, acSquareD3, bcx, t61);
t59_len = fast_expansion_sum_zeroelim(t61_len, t61, t60_len, t60, t59);
t62_len = scale_expansion_zeroelim(acSquareD3_len, acSquareD3, bcxC, t62);
yMinorD2_len = fast_expansion_sum_zeroelim(t62_len, t62, t59_len, t59, yMinorD2);
yMinorD3_len = fast_expansion_diff_zeroelim(yMinorD_len, yMinorD, yMinorD2_len, yMinorD2, yMinorD3);
Two_Product(bcx, acyC, t64[1], t64[0]);
Two_Product(bcxC, acy, t65[1], t65[0]);
Two_Two_Sum(t64[1], t64[0], t65[1], t65[0], t63[3], t63[2], t63[1], t63[0]); 
t63_len = 4;
Two_Product(bcxC, acyC, t66[1], t66[0]);
zMinorD_len = fast_expansion_sum_zeroelim(2, t66, t63_len, t63, zMinorD);
Two_Product(acx, bcyC, t68[1], t68[0]);
Two_Product(acxC, bcy, t69[1], t69[0]);
Two_Two_Sum(t68[1], t68[0], t69[1], t69[0], t67[3], t67[2], t67[1], t67[0]); 
t67_len = 4;
Two_Product(acxC, bcyC, t70[1], t70[0]);
zMinorD2_len = fast_expansion_sum_zeroelim(2, t70, t67_len, t67, zMinorD2);
zMinorD3_len = fast_expansion_diff_zeroelim(zMinorD_len, zMinorD, zMinorD2_len, zMinorD2, zMinorD3);
Two_Product(dcx, dcxC, t72[1], t72[0]);
t71[1] = 2.0 * t72[1]; t71[0] = 2.0 * t72[0];
Square(dcxC, t73[1], t73[0]);
Two_Two_Sum(t71[1], t71[0], t73[1], t73[0], dcSquareD[3], dcSquareD[2], dcSquareD[1], dcSquareD[0]); 
dcSquareD_len = 4;
Two_Product(dcy, dcyC, t75[1], t75[0]);
t74[1] = 2.0 * t75[1]; t74[0] = 2.0 * t75[0];
Square(dcyC, t76[1], t76[0]);
Two_Two_Sum(t74[1], t74[0], t76[1], t76[0], dcSquareD2[3], dcSquareD2[2], dcSquareD2[1], dcSquareD2[0]); 
dcSquareD2_len = 4;
dcSquareD3_len = fast_expansion_sum_zeroelim(dcSquareD2_len, dcSquareD2, dcSquareD_len, dcSquareD, dcSquareD3);
t78_len = scale_expansion_zeroelim(xMinorD3_len, xMinorD3, dcx, t78);
t79_len = scale_expansion_zeroelim(xMinorB3_len, xMinorB3, dcxC, t79);
t77_len = fast_expansion_sum_zeroelim(t79_len, t79, t78_len, t78, t77);
t80_len = scale_expansion_zeroelim(xMinorD3_len, xMinorD3, dcxC, t80);
dD_len = fast_expansion_sum_zeroelim(t80_len, t80, t77_len, t77, dD);
t82_len = scale_expansion_zeroelim(yMinorD3_len, yMinorD3, dcy, t82);
t83_len = scale_expansion_zeroelim(yMinorB3_len, yMinorB3, dcyC, t83);
t81_len = fast_expansion_sum_zeroelim(t83_len, t83, t82_len, t82, t81);
t84_len = scale_expansion_zeroelim(yMinorD3_len, yMinorD3, dcyC, t84);
dD2_len = fast_expansion_sum_zeroelim(t84_len, t84, t81_len, t81, dD2);
t86_len = mult_expansion_zeroelim(dcSquareB3_len, dcSquareB3, zMinorD3_len, zMinorD3, t86);
t87_len = mult_expansion_zeroelim(dcSquareD3_len, dcSquareD3, zMinorB3_len, zMinorB3, t87);
t85_len = fast_expansion_sum_zeroelim(t87_len, t87, t86_len, t86, t85);
t88_len = mult_expansion_zeroelim(dcSquareD3_len, dcSquareD3, zMinorD3_len, zMinorD3, t88);
dD3_len = fast_expansion_sum_zeroelim(t88_len, t88, t85_len, t85, dD3);
dD4_len = fast_expansion_sum_zeroelim(dD3_len, dD3, dD2_len, dD2, dD4);
dD5_len = fast_expansion_sum_zeroelim(dD4_len, dD4, dD_len, dD, dD5);
t89_len = fast_expansion_sum_zeroelim(dD5_len, dD5, dB5_len, dB5, t89);
if (t89[t89_len - 1] > 0.0) return 1.0; else
if (t89[t89_len - 1] < 0.0) return -1.0; else return 0.0;
}
};
}
};
}
};

}