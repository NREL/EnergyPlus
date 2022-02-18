/***************************************************************************
 * full_test.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <time.h>
#include <locale.h>
#include <time.h>
#include <getopt.h>
#ifdef WIN32
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#endif

#include "mgl2/base.h"
#include "mgl2/mgl.h"
#include "mgl2/font.h"
#include "mgl2/eval.h"
#include "mgl2/evalc.h"
//-----------------------------------------------------------------------------
void smgl_fexport(mglGraph *gr);	// test file export
void MGL_EXPORT mgl_create_cpp_font(HMGL gr, const wchar_t *how);
long MGL_EXPORT mgl_check_tex_table();
//-----------------------------------------------------------------------------
struct mglSample	/// Structure for list of samples
{
	const char *name;
	void (*func)(mglGraph*);
	const char *mgl;
	const char *info;
};
extern mglSample samp[];
extern const char *mmgl_dat_prepare;
//-----------------------------------------------------------------------------
int MGL_LOCAL_PURE mgl_cmd_smp(const void *a, const void *b)
{
	const mglSample *aa = (const mglSample *)a;
	const mglSample *bb = (const mglSample *)b;
	return strcmp(aa->name, bb->name);
}
//-----------------------------------------------------------------------------
int type = 0;
int dotest  = 0;
int width  = 800;
int height = 600;
int big  = 0;
int srnd = 0;
int use_mgl = 0;
int verbose = 0;
int quality  = MGL_DRAW_NORM;
//-----------------------------------------------------------------------------
void mgls_prepare1d(mglData *y, mglData *y1=0, mglData *y2=0, mglData *x1=0, mglData *x2=0);
void mgls_prepare2d(mglData *a, mglData *b=0, mglData *v=0);
void mgls_prepare3d(mglData *a, mglData *b=0);
void mgls_prepare2v(mglData *a, mglData *b);
void mgls_prepare3v(mglData *ex, mglData *ey, mglData *ez);
//-----------------------------------------------------------------------------
void save(mglGraph *gr,const char *name,const char *suf);
void test(mglGraph *gr)
{
	mglParse par;
	par.Execute(gr,"new f 100 'x^3':save f 'test.dat':axis:box:fplot ':test.dat:-1:1'");
	par.Execute(gr,"text 0 0 'ab' '@'");
//	par.Execute(gr,"new a 3 3 'x*y'\nsubplot 2 1 0 '':dens a:dens a 'k+/'\n"
//	"subplot 2 1 1 '':mask '+' 'ff00182424f800' 30:dens a '3+'");
	return;
	
	dual c(0,M_PI/2), r=mgl_ipowc(c,2);
	printf("(%g,%g)\n",r.real(),r.imag());
	return;
	
	mglData dat;	dat.Import("Equirectangular-projection.jpg","BbGYw",-1,1);
	dat.Save("1.dat");
//	gr->ShearPlot(3, 0, 0.2, 0.1);	gr->Box("r");
	return;
//	par.Execute(gr,"call 'test' -1\n func 'test' 1\nline $1 0 1 1 'b'\nreturn\n");
//	par.Execute(gr,"load '/home/balakin/mathgl-code/mathgl-2x/build/examples/libmgl_module.so':baxis\n");
//	par.Execute(gr,"subplot 1 1 0:#rotate 40 60\nperspective 1.22:box:axis\n");
//	return;
}
//-----------------------------------------------------------------------------
void mgl_generate_texi()
{
	FILE *fp = fopen("samples.texi","w");
	FILE *fq = fopen("samples_list.texi","w");
	fprintf(fp,"@c ------------------------------------------------------------------\n"
		"@chapter All samples\n@nav{}\n\n"
		"This chapter contain alphabetical list of MGL and C++ samples for most of MathGL graphics and features.\n\n@menu\n* initialization sample::\n");
	for(const mglSample *s = samp;s->name && s->name[0];s++)
		fprintf(fp,"* %s sample::\n",s->name);
	fprintf(fp,"@end menu\n@external{}\n");
	fprintf(fp,"@c ------------------------------------------------------------------\n"
		"@node initialization sample, %s sample, , All samples\n@section Functions for initialization\n@nav{}\n", samp[0].name);
	fprintf(fp,"\nThis section contain functions for input data for most of further samples.\n\n@strong{MGL code:}\n@verbatim\n%s\n@end verbatim\n", mmgl_dat_prepare);
	fprintf(fp,"\n@ifclear UDAV\n@strong{C++ code:}\n@verbatim\n");
	
	char buf[512], name[64];
	FILE *fs = fopen("wnd_samples.cpp","r");
	while(!feof(fs))
	{
		if(!fgets(buf,512,fs) || strstr(buf,"void mgls_prepare1d"))	break;
	}
	while(!feof(fs))
	{	fprintf(fp,"%s",buf);	if(!fgets(buf,512,fs))	break;	}
	fprintf(fp,"\n@end verbatim\n@end ifclear\n\n@external{}\n");
	fclose(fs);

	const char *prev = "initialization";
	fs = fopen("samples.cpp","r");
	for(size_t i=0;samp[i].name && samp[i].name[0];i++)
	{
		fprintf(fq, "@sfig{%s, %s sample}\n", samp[i].name, samp[i].name);
		if(samp[i+1].name && samp[i+1].name[0])
			fprintf(fp,"@c ------------------------------------------------------------------\n"
				"@node %s sample, %s sample, %s sample, All samples\n@section Sample @samp{%s}\n@nav{}\n",
				samp[i].name, samp[i+1].name, prev, samp[i].name);
		else
			fprintf(fp,"@c ------------------------------------------------------------------\n"
				"@node %s sample, , %s sample, All samples\n@section Sample '%s'\n@nav{}\n",
				samp[i].name, samp[i-1].name, samp[i].name);
		prev = samp[i].name;
		fprintf(fp,"\n%s\n\n@strong{MGL code:}\n@verbatim\n%s\n@end verbatim\n", samp[i].info, samp[i].mgl);
		fprintf(fp,"\n@ifclear UDAV\n@strong{C++ code:}\n@verbatim\n");

		fseek(fs,0,SEEK_SET);
		snprintf(name, 64, "void smgl_%s", samp[i].name);
		while(!feof(fs))
		{	if(!fgets(buf,512,fs) || strstr(buf,name))	break;	}
		while(!feof(fs))
		{
			fprintf(fp,"%s",buf);
			if(!fgets(buf,512,fs) || *buf=='}')	break;
		}
		fprintf(fp,"}\n@end verbatim\n@end ifclear\n@pfig{%s, Sample @samp{%s}}\n@external{}\n", samp[i].name, samp[i].name);
	}
	fclose(fs);	fclose(fp);	fclose(fq);
}

static struct option longopts[] =
{
	{ "big",	no_argument,	&big,		1 },
	{ "web",	no_argument,	&big,		2 },
	{ "mini",	no_argument,	&big,		3 },
	{ "help",	no_argument,	NULL,		'?' },
	{ "height",	required_argument,	NULL,	'h' },
	{ "kind",	required_argument,	NULL,	'k' },
	{ "list",	no_argument,	NULL,		'l' },
	{ "mgl",	no_argument,	&use_mgl,	1 },
	{ "srnd",	no_argument,	&srnd,		1 },

	{ "png",	no_argument,	&type,		0 },
	{ "eps",	no_argument,	&type,		1 },
	{ "svg",	no_argument,	&type,		2 },
	{ "solid",	no_argument,	&type,		3 },
	{ "jpeg",	no_argument,	&type,		4 },
	{ "prc",	no_argument,	&type,		5 },
	{ "gif",	no_argument,	&type,		6 },
	{ "none",	no_argument,	&type,		7 },
	{ "bps",	no_argument,	&type,		8 },
	{ "pdf",	no_argument,	&type,		9 },
	{ "obj_old",no_argument,	&type,		10 },
	{ "obj",	no_argument,	&type,		11 },
	{ "off",	no_argument,	&type,		12 },
	{ "stl",	no_argument,	&type,		13 },
	{ "tex",	no_argument,	&type,		14 },
	{ "json",	no_argument,	&type,		15 },
	{ "jsonz",	no_argument,	&type,		16 },
	{ "docs",	no_argument,	&type,		17 },

	{ "test",	no_argument,	&dotest,	1 },
	{ "font",	no_argument,	&dotest,	2 },
	{ "time",	no_argument,	&dotest,	3 },
	{ "fexport",no_argument,	&dotest,	4 },
	{ "textbl",	no_argument,	&dotest,	5 },
	{ "texi",	no_argument,	&dotest,	6 },

	{ "thread",	required_argument,	NULL,	't' },
	{ "verbose",no_argument,	&verbose,	1 },
	{ "width",	required_argument,	NULL,	'w' },
	{ "quality",required_argument,	NULL,	'q' },
	{ NULL,		0,				NULL,		0 }
};
//-----------------------------------------------------------------------------
void usage()
{
	puts (
//		"--png		- output png\n"
		"--width=num	- png picture width\n"
		"--height=num	- png picture height\n"
		"--mini		- png picture is 200x150\n"
		"--big		- png picture is 1920x1440\n"
		"--web		- png picture is 640x480\n"
		"--prc		- output prc\n"
		"--pdf		- output pdf\n"
		"--eps		- output EPS\n"
		"--tex		- output LaTeX\n"
		"--jpeg		- output JPEG\n"
		"--json		- output JSON\n"
		"--jsonz		- output JSONz\n"
		"--solid	- output solid PNG\n"
		"--svg		- output SVG\n"
		"--obj		- output obj/mtl\n"
		"--obj_old	- output obj/mtl in old way\n"
		"--off		- output off\n"
		"--stl		- output stl\n"
		"--docs		- output in png, prc/pdf and json\n"
		"--none		- none output\n"
		"--srnd		- use the same random numbers in any run\n"
		"--list		- print list of sample names\n"
		"--kind=name	- produce only this sample\n"
		"--thread=num	- number of threads used\n"
		"--mgl		- use MGL scripts for samples\n"
		"--test		- run in test mode\n"
		"--time		- measure execution time for all samples\n"
		"--texi		- make TeXi file with all samples\n"
		"--font		- write current font as C++ file\n"
		"--quality=val	- use specified quality for plot(s)\n"
		"--fexport	- test most of output formats\n"
	);
}
//-----------------------------------------------------------------------------
void save(mglGraph *gr,const char *name,const char *suf="")
{
	//	return;
	char buf[128];
	switch(type)
	{
		case 1:	// EPS
			snprintf(buf,128,"%s%s.eps",name,suf);
			gr->WriteEPS(buf);
			break;
		case 2:	// SVG
			snprintf(buf,128,"%s%s.svg",name,suf);
			gr->WriteSVG(buf);	break;
		case 3:	// PNG
			snprintf(buf,128,"%s%s.png",name,suf);
			gr->WritePNG(buf,0,true);	break;
		case 4:	// JPEG/* 		r1 = 1./(x*x+y*y+z*z+0.01);	r2=exp(-0.01/r1/r1)*r1;
			snprintf(buf,128,"%s%s.jpg",name,suf);
			gr->WriteJPEG(buf);	break;
		case 5:	// PRC
			snprintf(buf,128,"%s%s.prc",name,suf);
			gr->WritePRC(buf,"",false);	break;
		case 6:	// GIF
			snprintf(buf,128,"%s%s.gif",name,suf);
			gr->WriteGIF(buf);	break;
		case 7:	gr->Finish();	// none
			break;
		case 8:	// EPS to PNG
			snprintf(buf,128,"%s%s.png",name,suf);
			gr->WritePNG(buf,0,false);
			break;
 		case 9:	// PDF
			snprintf(buf,128,"%s%s.prc",name,suf);
			gr->WritePRC(buf);	remove(buf);	break;
		case 10:	// old OBJ
			snprintf(buf,128,"%s%s.obj",name,suf);
			gr->WriteOBJold(buf);	break;
		case 11:	// OBJ
			snprintf(buf,128,"%s%s.obj",name,suf);
			gr->WriteOBJ(buf);	break;
		case 12:	// OFF
			snprintf(buf,128,"%s%s.off",name,suf);
			gr->WriteOFF(buf);	break;
		case 13:	// STL
			snprintf(buf,128,"%s%s.stl",name,suf);
			gr->WriteSTL(buf);	break;
		case 14:	// TeX
			snprintf(buf,128,"%s%s.tex",name,suf);
			gr->WriteTEX(buf);	break;
		case 15:	// JSON
			snprintf(buf,128,"%s%s.json",name,suf);
			gr->WriteJSON(buf);	break;
		case 16:	// JSON
			snprintf(buf,128,"%s%s.jsonz",name,suf);
			gr->WriteJSON(buf,"",true);	break;
		case 17:	// PNG + JSON + PDF
			snprintf(buf,128,"%s%s.png",name,suf);
			gr->WritePNG(buf,0,true);
			snprintf(buf,128,"%s%s.json",name,suf);
			gr->WriteJSON(buf);
			gr->SetSize(height,height,false);
			snprintf(buf,128,"%s%s.prc",name,suf);
			gr->WritePRC(buf);	remove(buf);	break;
		default:// PNG (no alpha)
#if MGL_HAVE_PNG
			snprintf(buf,128,"%s%s.png",name,suf);
			gr->WritePNG(buf,0,false);	break;
#else
			snprintf(buf,128,"%s%s.bmp",name,suf);
			gr->WriteBMP(buf);	break;
#endif
	}
}
//-----------------------------------------------------------------------------
int main(int argc,char **argv)
{
// const char *f = strrchr(argv[0],'/');
// std::string p(argv[0],f-argv[0]);
// printf("getcwd = '%s', argv = '%s', path = '%s', inst = '%s'\n", getcwd(NULL,0), argv[0], p.c_str(), MGL_INSTALL_DIR);
// fflush(stdout);

	mgl_textdomain(argv?argv[0]:NULL,"");
	mgl_suppress_warn(true);
	const char *suf = "";
	char name[256]="", *tmp;
	int ch;
	time_t st,en;	time(&st);
	mglGraph *gr = NULL;
	mglSample *s=samp;
	while(( ch = getopt_long_only(argc, argv, "", longopts, NULL)) != -1)
		switch(ch)
		{
			case 0:		break;
			case 'w':	width =atoi(optarg);	break;
			case 'h':	height=atoi(optarg);	break;
			case 'q':	quality =atoi(optarg);	break;
			case 'k':	mgl_strncpy(name, optarg,256);
						tmp=strchr(name,'.');	if(tmp)	*tmp=0;
						tmp=strchr(name,'-');	if(tmp)	*tmp=0;
						break;
			case 't':	mgl_set_num_thr(atoi(optarg));	break;
			case 'l':
				while(s->name[0])	{	printf("%s ",s->name);	s++;	}
				printf("\n");	return 0;
			case '?':
			default:	usage();	return 0;
		}

	if(dotest==1)	printf("Global (before):%s\n",mglGlobalMess.c_str());
	gr = new mglGraph;
	if(	type==11|| type==12|| type==5 || type==9)	width=height;
	switch(big)
	{
	case 1:	gr->SetSize(1920,1440);	suf = "-lg";	break;
	case 2:	gr->SetSize(640,480);	break;
	case 3:	gr->SetSize(192,144);	suf = "-sm";	break;
	default:	gr->SetSize(width,height);
	}
	gr->SetQuality(quality);

	if(dotest==1)
	{
		mgl_set_test_mode(true);	test(gr);
		time(&en);	printf("time is %g sec\n",difftime(en,st));
#if MGL_HAVE_PNG
		gr->WritePNG("test.png","",false);
#else
		gr->WriteBMP("test.bmp");
#endif
		gr->WriteSVG("test.svg");
		gr->WriteEPS("test.eps");
		printf("Messages:%s\n",gr->Message());
		printf("Global:%s\n",mglGlobalMess.c_str());
		delete gr;	return 0;
	}
	else if(dotest==2)	// NOTE mgl_gen_fnt[###][6] have to be updated if new glyphs will be added to built-in font
	{	mgl_create_cpp_font(gr->Self(), L"!-~,¡-ÿ,̀-̏,Α-ω,ϑ,ϕ,ϖ,ϰ,ϱ,ϵ,А-я,ℏ,ℑ,ℓ,ℜ,←-↙,∀-∯,≠-≯,⟂");
		delete gr;	return 0;	}
	else if(dotest==3)
	{
		int qual[7]={0,1,2,4,5,6,8};
		size_t ll=strlen(mmgl_dat_prepare)+1;
		mglParse par;
		par.AllowSetSize(true);
		FILE *fp = fopen(big?"time_big.texi":"time.texi","w");
		FILE *fi = fopen("/proc/cpuinfo","r");
		if(fi)
		{
			char buf[128];
			while(!feof(fi))
			{	if(!fgets(buf,128,fi))	break;
				if(!strstr(buf,"model name"))
				{	fprintf(fp,"@c %s\n",buf);	break;	}	}
			fclose(fi);
		}
		fprintf(fp,"@multitable @columnfractions .16 .12 .12 .12 .12 .12 .12 .12\n");
		fprintf(fp,"@headitem Name");
		for(int i=0;i<7;i++)	fprintf(fp," @tab q=%d",qual[i]);
		clock_t beg,end;
		while(s->name[0])	// all samples
		{
			char *buf = new char[strlen(s->mgl)+ll];
			strcpy(buf,s->mgl);	strcat(buf,mmgl_dat_prepare);
			fprintf(fp,"\n@item %s",s->name);

			printf("%s",s->name);
			for(int i=0;i<7;i++)
			{
				gr->DefaultPlotParam();
				gr->SetQuality(qual[i]);	gr->Clf();
				beg = clock();
				if(!use_mgl)	s->func(gr);
				else 	par.Execute(gr,buf);
				gr->Finish();
				end = clock();
				fprintf(fp," @tab %.3g",double(end-beg)/CLOCKS_PER_SEC);
				printf("\t%d->%g",qual[i],double(end-beg)/CLOCKS_PER_SEC);
				fflush(fp);	fflush(stdout);
			}
			printf("\n");	delete []buf;	s++;
		}
		fprintf(fp,"\n@end multitable\n");	fclose(fp);
	}
	else if(dotest==4)
	{	smgl_fexport(gr);	delete gr;	return 0;	}
	else if(dotest==5)
	{
		mgl_check_tex_table();
		delete gr;	return 0;
	}
	else if(dotest==6)
	{
		mgl_generate_texi();
		delete gr;	return 0;
	}

	if(type==15 || type==16)	big=3;	// save mini version for json

	if(srnd)	mgl_srnd(1);
	gr->VertexColor(false);	gr->Compression(false);
	if(name[0]==0)
	{
		while(s->name[0])	// all samples
		{
			gr->DefaultPlotParam();	gr->Clf();
			if(use_mgl)
			{
				mglParse par;
				par.AllowSetSize(true);
				char *buf = new char[strlen(s->mgl)+strlen(mmgl_dat_prepare)+1];
				strcpy(buf,s->mgl);		strcat(buf,mmgl_dat_prepare);
				if(type!=7)	printf("\n-------\n%s\n-------\n",verbose?buf:s->mgl);
				par.Execute(gr,buf);	delete []buf;
				const char *mess = gr->Message();
				if(*mess)	printf("Warnings: %s\n-------\n",mess);
			}
			else	s->func(gr);
			save(gr, s->name, suf);
			printf("%s ",s->name);	fflush(stdout);	s++;
			gr->SetQuality(quality);
		}
		printf("\n");
	}
	else	// manual sample
	{
		mglSample tst;	tst.name=name;
		int i=0;
		for(i=0;samp[i].name[0];i++);	// determine the number of samples
		s = (mglSample *) bsearch(&tst, samp, i, sizeof(mglSample), mgl_cmd_smp);
		if(s)
		{
			gr->DefaultPlotParam();	gr->Clf();
			if(use_mgl)
			{
				mglParse par;
				par.AllowSetSize(true);
				char *buf = new char[strlen(s->mgl)+strlen(mmgl_dat_prepare)+1];
				strcpy(buf,s->mgl);		strcat(buf,mmgl_dat_prepare);
				if(type!=7)	printf("\n-------\n%s\n-------\n",verbose?buf:s->mgl);
				par.Execute(gr,buf);	delete []buf;
				const char *mess = gr->Message();
				if(*mess)	printf("Warnings: %s\n-------\n",mess);
			}
			else	s->func(gr);
			save(gr, s->name, suf);
		}
		else	printf("no sample %s\n",name);
	}
	delete gr;	return 0;
}
//-----------------------------------------------------------------------------
