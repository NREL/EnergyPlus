#include "mgl2/mgl.h"
#include "mgl2/base.h"
#include "mgl2/parser.h"
//-----------------------------------------------------------------------------
int test1(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,""))	{	gr->Box();	gr->Axis();	}
	else if(!strcmp(k,"s"))	{	gr->Box();	gr->Axis(a[0].s.s);	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
int test2(mglGraph *gr, long , mglArg *a, const char *k, const char *)
{
	int res=0;
	if(!strcmp(k,"nnnns"))
	{
		gr->FaceZ(mglPoint(a[0].v-a[2].v/2,a[1].v-a[3].v/2),a[2].v,a[3].v,"r");
		gr->Putsw(mglPoint(a[0].v,a[1].v),a[4].s.w);
	}
	else if(!strcmp(k,"nnnnss"))
	{
		gr->FaceZ(mglPoint(a[0].v-a[2].v/2,a[1].v-a[3].v/2),a[2].v,a[3].v,a[5].s.s);
		gr->Putsw(mglPoint(a[0].v,a[1].v),a[4].s.w);
	}
	else res = 1;
	return res;
}
//-----------------------------------------------------------------------------
mglCommand mgl_cmd_extra[] = {
	{"baxis","Box + axis","baxis ['stl'='']", test1, 12},
	{"trect","Box + text","trect x0 y0 dx dy 'text' ['stl'='']", test2, 13},
	{"",0,0,0,0}};
//-----------------------------------------------------------------------------
