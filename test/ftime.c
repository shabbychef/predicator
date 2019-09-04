#include <stdio.h>
#include "ftime.h"
#include "etime.h"
#include "delta.h"

double ftime(test_funct P, double E)
{
	int i, n;
	double T_aggregate = 0, T_threshold, Ts, Tf;
	double D = delta();
	T_threshold = D / E + D;

	for (n = 1; T_aggregate <= T_threshold; n <<= 1)
	{
		init_etime();
		Ts = get_etime();
		for (i = 0; i < n; i++) (*P)();
		Tf = get_etime();
		T_aggregate = Tf - Ts;
	}
	return(2 * T_aggregate / n);
}	
