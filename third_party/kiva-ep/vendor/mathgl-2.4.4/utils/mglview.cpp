/***************************************************************************
 * mglview.cpp is part of Math Graphic Library
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
#if USE_FLTK
	#include "mgl2/fltk.h"
	#include <FL/Fl.H>
	#include <FL/Fl_Preferences.H>
#else
	#include "mgl2/qt.h"
#endif
//-----------------------------------------------------------------------------
std::wstring str, opt;
std::vector<std::string> anim;
mglParse p(true);
void prop_func(char id, const char *val, void *)
{	p.AddParam(id<='9' ? id-'0' : id-'a'+10, val);	}
//-----------------------------------------------------------------------------
int show(mglGraph *gr)
{
	if(anim.size()>0)
	{
		for(size_t i=0;i<anim.size();i++)
		{
			gr->NewFrame();
			p.AddParam(0,anim[i].c_str());
			p.Execute(gr,str.c_str());
			gr->EndFrame();
		}
		return gr->GetNumFrame();
	}
	p.Execute(gr,str.c_str());
	printf("%s\n",gr->Message());
	return 0;
}
//-----------------------------------------------------------------------------
int main(int argc, char **argv)
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	char iname[256]="";
	mgl_suppress_warn(true);
	bool gray = false;
	while(1)
	{
		int ch = getopt(argc, argv, "1:2:3:4:5:6:7:8:9:hL:s:g:v:");
		if(ch>='1' && ch<='9')	p.AddParam(ch-'0', optarg);
		else if(ch=='s')
		{
			FILE *fp = fopen(optarg,"r");
			if(fp)
			{
				wchar_t ch;
				while(!feof(fp) && size_t(ch=fgetwc(fp))!=WEOF)	opt.push_back(ch);
				fclose(fp);
			}
		}
		else if(ch=='v')	p.SetVariant(atoi(optarg));
		else if(ch=='g')	gray= atoi(optarg);
		else if(ch=='L')
		{	setlocale(LC_ALL, optarg);	setlocale(LC_NUMERIC, "C");	}
		else if(ch=='h' || (ch==-1 && optind>=argc))
		{
			printf(_("mglview show plot from MGL script or MGLD file.\nCurrent version is 2.%g\n"),MGL_VER2);
			printf(_("Usage:\tmglview [parameter(s)] scriptfile\n"));
			printf(
				_("\t-1 str       set str as argument $1 for script\n"
				"\t...          ...\n"
				"\t-9 str       set str as argument $9 for script\n"
				"\t-g val       set gray-scale mode (val=0|1)\n"
				"\t-v val       set variant of arguments\n"
				"\t-s opt       set MGL script for setting up the plot\n"
				"\t-L loc       set locale to loc\n"
				"\t-            get script from standard input\n"
				"\t-h           print this message\n") );
			return 0;
		}
		else if(ch==-1 && optind<argc)
		{	mgl_strncpy(iname, argv[optind][0]=='-'?"":argv[optind],256);	break;	}
	}

	std::string ids;
	std::vector<std::string> par;
	bool mgld=(*iname && iname[strlen(iname)-1]=='d');
	if(!mgld)
	{
		str = opt + L"\n";
		FILE *fp = *iname?fopen(iname,"r"):stdin;
		if(fp)
		{
			wchar_t ch;
			std::string text;
			while(!feof(fp) && size_t(ch=fgetwc(fp))!=WEOF)
			{	str.push_back(ch);	text.push_back(ch);	}
			fclose(fp);

			double a1, a2, da;
			mgl_parse_comments(text.c_str(), a1, a2, da, anim, ids, par);
//			if(!ids.empty())	dr->gr->dialog(ids,par);
		}
		else	{	printf("No file for MGL script\n");	return 0;	}
	}

#if USE_FLTK
	mgl_ask_func = mgl_ask_fltk;
	mgl_progress_func = mgl_progress_fltk;
	mglFLTK gr(mgld?NULL:show, *iname?iname:"mglview");
#else
	mgl_ask_func = mgl_ask_qt;
	mglQT gr(mgld?NULL:show, *iname?iname:"mglview");
#endif
	gr.SetPropFunc(prop_func,NULL);
	gr.MakeDialog(ids, par);
	if(gray)	gr.Gray(gray);

	if(mgld)
	{
		gr.Setup(false);
		gr.NewFrame();
		const std::string loc = setlocale(LC_NUMERIC, "C");
		if(!opt.empty())
		{
			p.Execute(&gr,opt.c_str());
			printf("Setup script: %s\n",gr.Message());
			gr.ImportMGLD(iname,true);
		}
		else	gr.ImportMGLD(iname);
		setlocale(LC_NUMERIC, loc.c_str());
		gr.EndFrame();	gr.Update();
	}
	if(!mglGlobalMess.empty())	printf("%s",mglGlobalMess.c_str());
	return gr.Run();
}
//-----------------------------------------------------------------------------
