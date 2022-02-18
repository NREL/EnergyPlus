#include "mgl2/define.h"

#if MGL_HAVE_C99_COMPLEX
MGL_EXPORT dual mdual2c(cmdual c)
{	return c.re+1.0i*c.im;	}
MGL_EXPORT cmdual c2mdual(dual c)
{	cmdual r;	r.re=creal(c);	r.im=cimag(c);	return r;	}
#endif
