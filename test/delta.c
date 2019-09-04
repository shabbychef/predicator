#include <stdio.h>
#include "etime.h"

double delta()
{
	double t, Ts;
	init_etime();
	Ts = get_etime();
	while ((t = get_etime()) == Ts);
 	return(t);
} 
