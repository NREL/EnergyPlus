#include <iostream>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
//===================================================================
#define IM1 2147483563
#define IM2 2147483399
#define AM (1.0/IM1)
#define IMM1 (IM1-1)
#define IA1 40014
#define IA2 40692
#define IQ1 53668
#define IQ2 52774
#define IR1 12211
#define IR2 3791
#define NTAB 32
#define NDIV (1+IMM1/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)
#ifndef NULL
#define NULL 0L
#endif
//===================================================================
double Random()
// Long period (> 2 * 10^18 ) random number generator of L'Ecuyer with
// Bays-Durham shuffle and added safeguards. Returns a uniform random deviate
// between 0.0 and 1.0 (exclusive of the endpoint values). Call with idum a
// negative integer to initialize; thereafter, do not alter idum between
// successive deviates in a sequence. RNMX should approximate the largest
// floating value that is less than 1.
{
	static long idum=0;
	int j;
	long k;
	static long idum2=123456789;
	static long iy=0;
	static long iv[NTAB];
	double temp;
	if(idum==0)
		idum = -(long)(time(NULL));
	if (idum <= 0) { 				// Initialize.
		if (-(idum) < 1) idum=1;	// Be sure to prevent idum = 0.
		else idum = -(idum);
		idum2=(idum);
		for (j=NTAB+7;j>=0;j--) {	// Load the shuffle table (after 8 warm-ups).
			k=(idum)/IQ1;
			idum=IA1*(idum-k*IQ1)-k*IR1;
			if (idum < 0) idum += IM1;
			if (j < NTAB) iv[j] = idum;
		}
		iy=iv[0];
	}
	k=(idum)/IQ1; 					// Start here when not initializing.
	idum=IA1*(idum-k*IQ1)-k*IR1;	// Compute idum=(IA1*idum) % IM1 without
									// over ows by Schrage's method.
	if (idum < 0) idum += IM1;
	k=idum2/IQ2;
	idum2=IA2*(idum2-k*IQ2)-k*IR2;	// Compute idum2=(IA2*idum) % IM2 likewise.
	if (idum2 < 0) idum2 += IM2;
	j=iy/NDIV; 						// Will be in the range 0..NTAB-1.
	iy=iv[j]-idum2;					// Here idum is shued, idum and idum2 are
									// combined to generate output.
	iv[j] = idum;
	if (iy < 1) iy += IMM1;
	if ((temp=AM*iy) > RNMX)	// Because users don't expect endpoint values.
		return RNMX;
	else return temp;
}
//===================================================================
int strpos(char *str,char ch)
{
	char *p=strchr(str,ch);
	int res;
	if(p)	res = p-str;
	else	res = -1;
	return res;
}
//===================================================================
// multi_task empl_7b_tr.ini empl.ini 0:1:2 2:2:6
int main(int argc, char *argv[])
{
	bool e[10];
	if(argc<3)    // if incorrect number of arguments then print the help
	{
		printf("mgltask make output file with a set of copies of mask-file with repeatedly replaced $# by loop values. It useful for making set of initial conditions with a few parameters varied in specified range.\n");
		printf("Usage:\tmgltask maskfile outputfile [min1:step1:max1] [min2:step2:max2]\n\n");
		printf("\tmask file  -- mask file in which all '$#' will be replaced by counter # value\n");
		printf("\t\tHere # = 0 is random number in [0,1].\n");
		printf("\t\tHere # = 1,2...9 is counter number.\n");
		printf("\toutputfile -- file where result will be saved\n");
		printf("\tmin#:step#:max# -- is minimum, step increment and maximum values of counter #\n");
		printf("\t'e'min#:step#:max# -- the same but in exponential form 10^#\n");
//		system("PAUSE");
		return 0;
	}
	//char maskname[256],outname[256];
	char str[1024],*buf;
	double x1[10],x2[10],dx[10],x[10];
	int k,i,n=argc-3;//=(argc==4) ? 1:2;
	FILE *fm,*fo;
	
	// first place zeros
	for(i=0;i<10;i++)
	{
		x1[i] = x2[i] = 0;
		dx[i] = 1;
		e[i] = false;
	}
	printf("mask = %s, out = %s\n",argv[1],argv[2]);
	// read parameters of loops
	for(i=0;i<n;i++)
	{
		char *par = argv[i+3];
		if(par[0]=='e')	{	e[i] = true;	par++;	}
		sscanf(par,"%lg:%lg:%lg",&(x1[i]),&(dx[i]),&(x2[i]));
		printf("%g:%g:%g\n",x1[i],dx[i],x2[i]);
	}
	// for each variable
	fm = fopen(argv[1],"r");
	fo = fopen(argv[2],"w");
	for(x[0]=x1[0];x[0]<=x2[0];x[0]+=dx[0])
		for(x[1]=x1[1];x[1]<=x2[1];x[1]+=dx[1])
		{
			fseek(fm,0,0);
			while(!feof(fm))
			{
				fgets(str,1024,fm);		// for each string
				buf = str;
				while((i=strpos(buf,'$'))!=-1)    // find '$'
				{
					k=buf[i+1]-'1';
					buf[i]=0;
					if(k<0)				// random number
						fprintf(fo,"%s%g",buf,Random());
					if(k>=0 && k<n)		// if parameter is correct then change it
						fprintf(fo,"%s%g",buf,e[k] ? exp(M_LN10*x[k]):(fabs(x[k])<1e-10?0:x[k]));
					buf = &(buf[i+2]);	// handle the last part
				}
				fprintf(fo,"%s",buf);	// write it
			}
			fprintf(fo,"\n");
		}
	fclose(fm);
	fclose(fo);
	return 0;
}
//===================================================================
