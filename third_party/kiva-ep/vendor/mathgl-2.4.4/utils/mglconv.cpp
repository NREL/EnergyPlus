/***************************************************************************
 * mglconv.cpp is part of Math Graphic Library
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
#include <locale.h>
#include <getopt.h>
#include "mgl2/mgl.h"
#include "mgl2/wnd.h"

#ifdef _MSC_VER
#define mnpos (std::basic_string<wchar_t>::size_type)-1
#else
#define mnpos std::wstring::npos
#endif
//-----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	mgl_suppress_warn(true);
	mglGraph gr;
	mglParse p(true);
	char buf[2048], iname[256]="", oname[256]="";
	std::vector<std::wstring> var;	// animation variants
	std::wstring str;	// script
	bool none=false;

	while(1)
	{
		int ch = getopt(argc, argv, "1:2:3:4:5:6:7:8:9:hno:L:C:A:s:S:q:v:g:");
		if(ch>='1' && ch<='9')	p.AddParam(ch-'0', optarg);
		else if(ch=='s')
		{
			FILE *fp = fopen(optarg,"r");
			if(fp)
			{
				wchar_t ch;
				while(!feof(fp) && size_t(ch=fgetwc(fp))!=WEOF)	str.push_back(ch);
				fclose(fp);	str += L"\n";
			}
		}
		else if(ch=='n')	none = true;
		else if(ch=='L')
		{	setlocale(LC_ALL, optarg);	setlocale(LC_NUMERIC, "C");	}
		else if(ch=='S')	mgl_set_size_scl(atof(optarg));
		else if(ch=='q')	gr.SetQuality(atoi(optarg));
		else if(ch=='v')	p.SetVariant(atoi(optarg));
		else if(ch=='g')	gr.Gray(atoi(optarg));
		else if(ch=='A')
		{
			std::wstring s;
			for(size_t i=0;optarg[i];i++)	s.push_back(optarg[i]);
			var.push_back(s);
		}
		else if(ch=='C')
		{
			double v1,v2,dv=1,v;
			int res=sscanf(optarg,"%lg:%lg:%lg",&v1,&v2,&dv);
			if(res<3)	dv=1;
			wchar_t num[64];
			for(v=v1;v<=v2;v+=dv)
			{
				mglprintf(num,64,L"%g",v);
				var.push_back(num);
			}
		}
		else if(ch=='h' || (ch==-1 && optind>=argc))
		{
			printf(_("mglconv convert mgl script to image file (default PNG).\nCurrent version is 2.%g\n"),MGL_VER2);
			printf(_("Usage:\tmglconv [parameter(s)] scriptfile\n"));
			printf(
				_("\t-1 str       set str as argument $1 for script\n"
				"\t...          ...\n"
				"\t-9 str       set str as argument $9 for script\n"
				"\t-L loc       set locale to loc\n"
				"\t-s fname     set MGL script for setting up the plot\n"
				"\t-S val       set scaling factor for images\n"
				"\t-q val       set quality for output (val=0...9)\n"
				"\t-g val       set gray-scale mode (val=0|1)\n"
				"\t-v val       set variant of arguments\n"
				"\t-o name      set output file name\n"
				"\t-n           no default output (script should save results by itself)\n"
				"\t-A val       add animation value val\n"
				"\t-C n1:n2:dn  add animation value in range [n1,n2] with step dn\n"
				"\t-C n1:n2     add animation value in range [n1,n2] with step 1\n"
				"\t-            get script from standard input\n"
				"\t-h           print this message\n") );
			return 0;
		}
		else if(ch=='o')	mgl_strncpy(oname, optarg,256);
		else if(ch==-1 && optind<argc)
		{	mgl_strncpy(iname, argv[optind][0]=='-'?"":argv[optind],256);	break;	}
	}
	if(*oname==0)	{	mgl_strncpy(oname,*iname?iname:"out",250);	strcat(oname,".png");	}
	else	none = false;

	mgl_ask_func = mgl_ask_gets;
	// prepare for animation
	FILE *fp = *iname?fopen(iname,"r"):stdin;
	if(!fp)	{	printf("No file for MGL script\n");	return 0;	}
	wchar_t cw;
	while(!feof(fp) && size_t(cw=fgetwc(fp))!=WEOF)	str.push_back(cw);
	if(*iname)	fclose(fp);

/*	size_t n;
	for(size_t i=0;;)	// collect exact values
	{
		n = str.find(L"##a ",i);
		if (n == mnpos)	break;
		i = n+4;	var.push_back(str.substr(i,str.find('\n',i)));
	}
	n = str.find(L"##c ");
	if (n != mnpos)
		{
		double v1,v2,dv,v;
		wscanf(str.c_str()+n+4,L"%lg%lg%lg",&v1,&v2,&dv);
		wchar_t ss[64];
		for(v=v1;v<=v2;v+=dv)
		{	mglprintf(ss,64,L"%g",v);	var.push_back(ss);	}
	}*/
	mgl_parse_animation(str.c_str(), var);
	bool gif = !strcmp(oname+strlen(oname)-4,".gif");
	gr.SetSize(600,400);	// specially call for "S" option
	if(var.size()>1)	// there is animation
	{
		if(gif)	gr.StartGIF(oname);
		for(size_t i=0;i<var.size();i++)
		{
			gr.NewFrame();
			printf("frame %zu for $0 = \"%ls\"\n",i,var[i].c_str());
			p.AddParam(0,var[i].c_str());
			p.Execute(&gr,str.c_str());
			if(gr.Message()[0])	printf("%s\n",gr.Message());
			gr.EndFrame();
			snprintf(buf,2048,"%s-%zu",oname,i);	buf[2047]=0;
			if(!gif)	gr.WriteFrame(buf);
		}
		if(gif)	gr.CloseGIF();
	}
	else
	{
		p.Execute(&gr,str.c_str());
		if(gr.Message()[0])	printf("%s\n",gr.Message());
		if(!none)	gr.WriteFrame(oname);
	}
	if(!mglGlobalMess.empty())	printf("%s",mglGlobalMess.c_str());
	if(!none || gif)	printf("Write output to %s\n",oname);
	return 0;
}
//-----------------------------------------------------------------------------
