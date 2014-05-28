#include <math.h>
#include "wave.h"
// Forcing functions
const double pi=3.14159265358979;
double h(double z){ return sin(2*pi*z); }
double F(double x, double y) //{return 0;}
{ return 2*exp(x+y)+3*x*x+6*y*y+sin((x+y)/2)+cos((x+y)/2); }
double up(double x, double y) //{return 0;}
{ return exp(x+y)+x*x*x+2*y*y*y+sin((x+y)/2)-cos((x+y)/2); }
