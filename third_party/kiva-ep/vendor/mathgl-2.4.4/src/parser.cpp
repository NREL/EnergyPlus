/***************************************************************************
 * parse.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 3 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <ctype.h>
#include "mgl2/parser.h"
#include "mgl2/canvas_cf.h"
#include "mgl2/base.h"
//-----------------------------------------------------------------------------
int MGL_LOCAL_PURE mgl_cmd_cmp(const void *a, const void *b)
{
	const mglCommand *aa = (const mglCommand *)a;
	const mglCommand *bb = (const mglCommand *)b;
	return strcmp(aa->name, bb->name);
}
//-----------------------------------------------------------------------------
mglCommand *mglParser::BaseCmd=NULL;	///< Base table of MGL commands. It MUST be sorted by 'name'!!!
void mglParser::FillBaseCmd()
{
	if(BaseCmd)	return;
	size_t na=0, nd=0, ng=0, np=0, ns=0, nsum=0;
	while(mgls_prg_cmd[na].name[0])	na++;
	while(mgls_dat_cmd[nd].name[0])	nd++;
	while(mgls_grf_cmd[ng].name[0])	ng++;
	while(mgls_prm_cmd[np].name[0])	np++;
	while(mgls_set_cmd[ns].name[0])	ns++;
	BaseCmd = new mglCommand[na+nd+ng+np+ns+1];
	memcpy(BaseCmd, 	mgls_prg_cmd, na*sizeof(mglCommand));	nsum+=na;
	memcpy(BaseCmd+nsum,mgls_dat_cmd, nd*sizeof(mglCommand));	nsum+=nd;
	memcpy(BaseCmd+nsum,mgls_grf_cmd, ng*sizeof(mglCommand));	nsum+=ng;
	memcpy(BaseCmd+nsum,mgls_prm_cmd, np*sizeof(mglCommand));	nsum+=np;
	memcpy(BaseCmd+nsum,mgls_set_cmd, (ns+1)*sizeof(mglCommand));	nsum+=ns;
	qsort(BaseCmd, nsum, sizeof(mglCommand), mgl_cmd_cmp);
#if DEBUG
	long stat[17];	memset(stat,0,17*sizeof(long));
	const char *name[17] = { _("0 - special plot"), _("1 - other plot"), _("2 - setup"), _("3 - data handle"), _("4 - data create"), _("5 - subplot"), _("6 - program flow"), _("7 - 1d plot"), _("8 - 2d plot"), _("9 - 3d plot"), _("10 - dd plot"), _("11 - vector plot"), _("12 - axis"), _("13 - primitives"), _("14 - axis setup"), _("15 - text/legend"), _("16 - data transform") };
	for(size_t i=0;BaseCmd[i].name[0];i++)	stat[BaseCmd[i].type]+=1;
	for(size_t i=0;i<17;i++)	printf("%s: %ld\n",name[i],stat[i]);
	printf("\n");	fflush(stdout);
#endif
}
//-----------------------------------------------------------------------------
HMDT MGL_NO_EXPORT mglFormulaCalc(std::wstring string, mglParser *arg, const std::vector<mglDataA*> &head);
HADT MGL_NO_EXPORT mglFormulaCalcC(std::wstring string, mglParser *arg, const std::vector<mglDataA*> &head);
//-----------------------------------------------------------------------------
MGL_EXPORT void (*mgl_ask_func)(const wchar_t *, wchar_t *)=0;
void MGL_EXPORT mgl_ask_gets(const wchar_t *quest, wchar_t *res)
{	printf("%ls\n",quest);	if(!fgetws(res,1024,stdin))	*res=0;	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_progress_txt(int value, int maximal, HMGL)
{
	static int prev = 0;
	if(value<=0 || value>=maximal)	{	printf("\n");	value=0;	}
	else	for(int i=prev;i<value;i++)	printf("#");
	prev = value;	fflush(stdout);
}
MGL_EXPORT void (*mgl_progress_func)(int value, int maximal, HMGL)=mgl_progress_txt;
void MGL_EXPORT mgl_progress(int value, int maximal, HMGL gr)
{	mgl_progress_func(value, maximal, gr);	}
//-----------------------------------------------------------------------------
mglFunc::mglFunc(long p, const wchar_t *f)
{
	pos = p;	func = f;
	size_t i;
	for(i=0;(isalnum(f[i]) || f[i]=='_');i++);
	narg = wcstol(f+i+1,0,0);	func.crop(0,i);
	if(narg<0 || narg>9)	narg=0;
}
//-----------------------------------------------------------------------------
long mglParser::IsFunc(const wchar_t *name, int *na)
{
	for(size_t i=0;i<func.size();i++)
	{
		const mglFunc &f = func[i];
		if(f.func==name)
		{	if(na)	*na=f.narg;	return f.pos+1;	}
	}
	return 0;
}
//-----------------------------------------------------------------------------
void mglParser::ScanFunc(const wchar_t *line)
{
	static long num=0;
	if(!line)
	{	func.clear();	num=0;	return;	}
	num++;
	while(*line<=' ' && *line!=0)	line++;
	if(wcsncmp(line,L"func",4) || line[4]>' ')	return;
	long i;
	for(i=4;line[i]<=' ' || line[i]=='\'';i++);
	func.push_back(mglFunc(num-1, line+i));
}
//-----------------------------------------------------------------------------
MGL_NO_EXPORT wchar_t *mgl_str_copy(const char *s)
{
	size_t i,l=strlen(s);
	wchar_t *str = new wchar_t[l+1];
	for(i=0;i<l;i++)	str[i] = s[i];
	str[i] = 0;	return str;
}
//-----------------------------------------------------------------------------
bool mglParser::CheckForName(const std::wstring &s)
{
	return !isalpha(s[0]) || s.find_first_of(L"!@#$%%^&*/()-+|,.<>:")!=std::wstring::npos || s==L"rnd" || FindNum(s.c_str());
//	return !isalpha(s[0])||s.find_first_of(L".:()")!=std::wstring::npos;
}
//-----------------------------------------------------------------------------
const mglCommand *mglParser::FindCommand(const char *com) const
{
	if(!AllowFileIO && ( !strncmp(com,"read",4) || !strncmp(com,"save",4) || !strcmp(com,"fgets") || !strcmp(com,"import") || !strcmp(com,"export") ))
		return 0;
	mglCommand tst, *rts, *cmd = Cmd;
	long i;
	for(i=0;cmd[i].name[0];i++);	// determine the number of symbols
	tst.name = com;
	rts = (mglCommand *) bsearch(&tst, cmd, i, sizeof(mglCommand), mgl_cmd_cmp);
	return rts;
}
//-----------------------------------------------------------------------------
const mglCommand *mglParser::FindCommand(const wchar_t *com) const
{
	char cmd[33]="";
	size_t s = wcstombs(0,com,0);	// NOTE: command name should be less than 32
	if(s<32)	{	wcstombs(cmd,com,s+1);	cmd[s]=0;	}
	return FindCommand(cmd);
}
//-----------------------------------------------------------------------------
// return values : 0 -- OK, 1 -- wrong arguments, 2 -- wrong command, 3 -- unclosed string
int mglParser::Exec(mglGraph *gr, const wchar_t *com, long n, mglArg *a, const std::wstring &/*var*/, const wchar_t *opt)
{
	const char *id="dsn";
	std::string k;
	for(long i=0;i<n;i++)	k += id[a[i].type];
	const mglCommand *rts=FindCommand(com);
	if(!rts || !rts->exec)	return 2;
/*	if(rts->type == 4)
	{
		if(n<1 || CheckForName(var))	return 2;
		a[0].type = 0;	a[0].d = AddVar(var.c_str());
		a[0].w = var;	k[0] = 'd';
	}*/
	std::string vopt;
	if(opt && *opt)	// TODO: parse arguments of options
	{
		size_t len = mgl_wcslen(opt);
		std::vector<std::wstring> ss;
		std::wstring s, o;
		bool st = true, name = true;
		for(size_t i=0;i<len+1;i++)
		{
			if(st && opt[i]<=' ')	continue;	// skip spaces at beginning
			st = false;
			if(i<len && opt[i]!=';')
			{
				if(name && opt[i]<=' ')	{	name = false;	o = s;	}
				s.push_back(opt[i]);
			}
			else
			{
				size_t j = o.size(),k;
				wchar_t buf[64];
				if(o==L"xrange" || o==L"yrange" || o==L"zrange" || o==L"crange")	// 2 arguments
				{
					bool alph=false;
					for(k=j;k<s.length();k++)
					{
						if(s[k]>' ')	alph=true;
						if(alph && s[k]<=' ')	break;
					}
					HMDT d1 = mglFormulaCalc(s.substr(j+1,k-j),this, DataList);
					HMDT d2 = mglFormulaCalc(s.substr(k+1),this, DataList);
					mglprintf(buf,64,L" %g %g",d1->a[0],d2->a[0]);
					s = o+buf;	delete d1;	delete d2;
				}
				else if(o!=L"legend")	// 1 argument
				{
					HMDT dd = mglFormulaCalc(s.substr(j+1),this, DataList);
					mglprintf(buf,64,L" %g",dd->a[0]);
					s = o+buf;	delete dd;
				}
				st = name = true;	ss.push_back(s);
				o.clear();	s.clear();
			}
		}
		for(size_t i=0;i<ss.size();i++)
		{
			for(size_t j=0;j<ss[i].length();j++)	vopt += ss[i][j];
			vopt += ';';
		}
	}
	int res=rts->exec(gr, n, a, k.c_str(), vopt.c_str());
	return res;
}
//-----------------------------------------------------------------------------
mglParser::mglParser(bool setsize)
{
	InUse = 1;	curGr = 0;	Variant = 0;
	Skip=Stop=false;	StarObhID = 0;
	for(long i=0;i<40;i++)	par[i]=L"";

	FillBaseCmd();	Cmd = BaseCmd;
	AllowSetSize=setsize;	AllowFileIO=true;	AllowDllCall=true;
	Once = true;
	mglNum *v;
	v = new mglNum(0);	v->s = L"off";	NumList.push_back(v);
	v = new mglNum(1);	v->s = L"on";	NumList.push_back(v);
	v = new mglNum(-1);	v->s = L"all";	NumList.push_back(v);
	v = new mglNum(NAN);	v->s = L"nan";	NumList.push_back(v);
	v = new mglNum(M_PI);	v->s = L"pi";	NumList.push_back(v);
	v = new mglNum(INFINITY);	v->s = L"inf";	NumList.push_back(v);
#if MGL_HAVE_LTDL
	lt_dlinit();
#endif
}
//-----------------------------------------------------------------------------
mglParser::~mglParser()
{
	DeleteAll();
	for(size_t i=0;i<NumList.size();i++)	// force delete built-in variables
		if(NumList[i])	delete NumList[i];
	NumList.clear();
#if MGL_HAVE_LTDL
	lt_dlexit();
#endif
}
//-----------------------------------------------------------------------------
void mglParser::DeleteAll()
{
	for(size_t i=0;i<DataList.size();i++)
		if(DataList[i])	delete DataList[i];
	DataList.clear();
	for(size_t i=0;i<NumList.size();i++)
		if(NumList[i])	delete NumList[i];
	NumList.clear();
	mglNum *v;
	v = new mglNum(0);	v->s = L"off";	NumList.push_back(v);
	v = new mglNum(1);	v->s = L"on";	NumList.push_back(v);
	v = new mglNum(-1);	v->s = L"all";	NumList.push_back(v);
	v = new mglNum(NAN);	v->s = L"nan";	NumList.push_back(v);
	v = new mglNum(M_PI);	v->s = L"pi";	NumList.push_back(v);
	v = new mglNum(INFINITY);	v->s = L"inf";	NumList.push_back(v);
	if(Cmd && Cmd!=BaseCmd)	{	delete []Cmd;	Cmd = BaseCmd;	}
#if MGL_HAVE_LTDL
	for(size_t i=0;i<DllOpened.size();i++)
		lt_dlclose(DllOpened[i]);
	DllOpened.clear();
#endif
}
//-----------------------------------------------------------------------------
void mglParser::AddParam(int n, const char *str)
{
	MGL_TO_WCS(str,AddParam(n,wcs));
}
//-----------------------------------------------------------------------------
int mglParser::Parse(mglGraph *gr, const char *str, long pos)
{
	int r=0;
	MGL_TO_WCS(str,r = Parse(gr,wcs,pos));
	return r;
}
//-----------------------------------------------------------------------------
mglDataA *mglParser::AddVar(const char *str)
{
	mglDataA *v=0;
	MGL_TO_WCS(str,v = AddVar(wcs));
	return v;
}
//-----------------------------------------------------------------------------
mglDataA *mglParser::FindVar(const char *str)
{
	mglDataA *v=0;
	MGL_TO_WCS(str,v = FindVar(wcs));
	return v;
}
//-----------------------------------------------------------------------------
mglNum *mglParser::AddNum(const char *str)
{
	mglNum *v=0;
	MGL_TO_WCS(str,v = AddNum(wcs));
	return v;
}
//-----------------------------------------------------------------------------
mglNum *mglParser::FindNum(const char *str)
{
	mglNum *v=0;
	MGL_TO_WCS(str,v = FindNum(wcs));
	return v;
}
//-----------------------------------------------------------------------------
void mglParser::AddParam(int n, const wchar_t *str)
{
//	if(str && n>=0 && n<40 && !wcschr(str,'$'))	par[n] = str;
	if(str && n>=0 && n<40)	par[n] = str;
}
//-----------------------------------------------------------------------------
mglDataA *mglParser::FindVar(const wchar_t *name)
{
	if(name[0]=='!')	name = name+1;	// ignore complex prefix
 	for(size_t i=0;i<DataList.size();i++)
 		if(DataList[i] && !wcscmp(DataList[i]->Name(),name))
			return DataList[i];
	return 0;
}
//-----------------------------------------------------------------------------
mglDataA *mglParser::AddVar(const wchar_t *name)
{	// TODO add list of forbidden names (like function names)
	mglDataA *d=FindVar(name);
	if(name[0]=='!' && dynamic_cast<mglDataC*>(d)==0)
	{	d = new mglDataC;	d->Name(name+1);	DataList.push_back(d);	}
	else if(!d)
	{	d = new mglData;	d->Name(name);	DataList.push_back(d);	}
	return d;
}
//-----------------------------------------------------------------------------
mglNum *mglParser::FindNum(const wchar_t *name)
{
	for(size_t i=0;i<NumList.size();i++)
		if(NumList[i] && NumList[i]->s==name)	return NumList[i];
	return 0;
}
//-----------------------------------------------------------------------------
mglNum *mglParser::AddNum(const wchar_t *name)
{
	mglNum *v = FindNum(name);
	if(!v)	{	v=new mglNum;	v->s = name;	NumList.push_back(v);	}
	return v;
}
//-----------------------------------------------------------------------------
int MGL_LOCAL_PURE mglFindArg(const std::wstring &str)
{
	long l=0,k=0;
	const size_t s=str.length();
	for(size_t i=0;i<s;i++)
	{
		if(str[i]=='\'') l++;
		if(str[i]=='{') k++;
		if(str[i]=='}') k--;
		if(l%2==0 && k==0)
		{
			if(str[i]=='#' || str[i]==';')	return -long(i);
			if(str[i]<=' ')	return long(i);
		}
	}
	return 0;
}
//-----------------------------------------------------------------------------
// convert substrings to arguments
void mglParser::FillArg(mglGraph *gr, int k, std::wstring *arg, mglArg *a)
{
	for(long n=1;n<k;n++)
	{
		mglDataA *v;	mglNum *f;
		a[n-1].type = -1;
		std::wstring str = arg[n];
		size_t i1=0, i2=str.length(), j=0;
		bool s=true;
		for(size_t i=0;i<i2;i++)
		{
			if(str[i]=='\'')	s = !s;
			if(s && str[i]=='?')
			{
				if(j<Variant)	i1=i+1;
				else	i2=i;
				j++;
			}
		}
		str = str.substr(i1,i2-i1);

		if(str.empty())	a[n-1].type = -2;
		else if(str[0]=='|')	a[n-1].type = -1;
		else if(str[0]=='\'')	// this is string (simplest case)
		{
			a[n-1].type = 1;
			long na=1, ns=0, np=0, ll=str.length(), ii=1, op=0;
			std::vector<std::wstring> s;
			std::vector<int> id;	// 0 - string, 1 - cval, 2 - rval, 3 - plus, 4 - index
			for(long i=1;i<ll;i++)
			{
				if(str[i]=='\'')
				{
					if(na%2==1)
					{	id.push_back(0);	s.push_back(str.substr(ii,i-ii));	}
					else if(op && i>ii)
					{	id.push_back(op);	s.push_back(str.substr(ii,i-ii));	}
					na++;	ii=i+1;	op=0;
				}
				else if(na%2==0 && ns==0 && np==0)
				{
					if(str[i]=='+' && str[i-1]=='\'')
					{	op=3;	ii=i+1;	}
					else if(str[i]==',' && str[i+1]=='!')
					{
						if(op && i>ii)
						{	id.push_back(op);	s.push_back(str.substr(ii,i-ii));	}
						op=1;	ii=i+2;
					}
					else if(str[i]==',')
					{
						if(op && i>ii)
						{	id.push_back(op);	s.push_back(str.substr(ii,i-ii));	}
						op=2;	ii=i+1;
					}
					else if(str[i]=='[' && str[i-1]=='\'')
					{	ii=i+1;	ns++;	}
					else if(str[i]=='(')	np++;
				}
				else if(na%2==0 && np==0 && str[i]==']' && ns==1)
				{
					id.push_back(4);	s.push_back(str.substr(ii,i-ii));
					op=0;	ii=i+1;	ns--;
				}
				else if(na%2==0 && np==1 && str[i]==')' && ns==0)	np--;
			}
			if(op && ll>ii)
			{	id.push_back(op);	s.push_back(str.substr(ii,ll-ii));	}
			wchar_t buf[32];
			for(size_t i=0;i<id.size();i++)
			{
				if(id[i]==0)	a[n-1].s += s[i].c_str();
				else if(id[i]==1)
				{
					HADT d = mglFormulaCalcC(s[i], this, DataList);
					mreal di = imag(d->a[0]), dr = real(d->a[0]);
					if(di>0)	mglprintf(buf,32,L"%g+%gi",dr,di);
					else if(di<0)	mglprintf(buf,32,L"%g-%gi",dr,-di);	// TODO use \u2212 ???
					else	mglprintf(buf,32,L"%g",dr);
					a[n-1].s += buf;	delete d;
				}
				else if(id[i]==2)
				{
					HMDT d = mglFormulaCalc(s[i], this, DataList);
					mglprintf(buf,32,L"%g",d->a[0]);	a[n-1].s += buf;	delete d;
				}
				else if(id[i]==3)
				{
					HMDT d = mglFormulaCalc(s[i], this, DataList);
					a[n-1].s[a[n-1].s.length()-1] += d->a[0];	delete d;
				}
				else if(id[i]==4)
				{
					HMDT d = mglFormulaCalc(s[i], this, DataList);
					long v = long(d->a[0]+0.5);	delete d;
					if(v>=0 && v<long(a[n-1].s.length()))	a[n-1].s = a[n-1].s[v];
					else	a[n-1].s = L"";
				}
			}
		}
		else if(str[0]=='{')
		{	// this is temp data
			mglData *u=new mglData;
			std::wstring s = str.substr(1,str.length()-2);
			a[n-1].s = L"/*"+s+L"*/";
			a[n-1].type = 0;	u->Name(a[n-1].s.w);
			ParseDat(gr, s, *u);	a[n-1].d = u;
			u->temp=true;	DataList.push_back(u);
		}
		else if((v = FindVar(str.c_str()))!=0)	// try to find normal variables (for data creation)
		{	a[n-1].type=0;	a[n-1].d=v;	a[n-1].s=v->Name();	}
		else if((f = FindNum(str.c_str()))!=0)	// try to find normal number (for data creation)
		{	a[n-1].type=2;	a[n-1].d=0;	a[n-1].v=f->d;	a[n-1].c=f->c;	a[n-1].s = f->s;	}
		else if(str[0]=='!')	// complex array is asked
		{	// parse all numbers and formulas by unified way
			HADT d = mglFormulaCalcC(str.substr(1), this, DataList);
			if(d->GetNN()==1)
			{
				if(CheckForName(str.substr(1)))
				{	a[n-1].type = 2;	a[n-1].v = d->v(0);	a[n-1].c = d->a[0];	}
				else
				{	a[n-1].type = 0;	a[n-1].d = AddVar(str.c_str());	}
				delete d;
			}
			else
			{
				a[n-1].s = L"/*"+str+L"*/";
				d->temp=true;	DataList.push_back(d);
				a[n-1].type = 0;	a[n-1].d = d;
			}
		}
		else
		{	// parse all numbers and formulas by unified way
			HMDT d = mglFormulaCalc(str, this, DataList);
			if(d->GetNN()==1)
			{
				if(CheckForName(str))
				{	a[n-1].type = 2;	a[n-1].c = a[n-1].v = d->v(0);	}
				else
				{	a[n-1].type = 0;	a[n-1].d = AddVar(str.c_str());	}
				delete d;
			}
			else
			{
				a[n-1].s = L"/*"+str+L"*/";
				d->temp=true;	DataList.push_back(d);
				a[n-1].type = 0;	a[n-1].d = d;
			}
		}
	}
}
//-----------------------------------------------------------------------------
// return values: 0 - not found, 1 - OK, 2 - wrong arguments, 3 - wrong command, 4 - string too long
int mglParser::PreExec(mglGraph *, long k, std::wstring *arg, mglArg *a)
{
	long n=0;
	if(!arg[0].compare(L"list"))	// parse command "list"
	{
		if(k<3 || CheckForName(arg[1]))	return 2;
		long nx=0, ny=1,j=0,i,t=0;
		for(i=2;i<k;i++)
		{
			char ch = arg[i][0];
			if(a[i-1].type==1)	return 2;
			if(a[i-1].type==0)
			{
				if(t==1)	return 2;
				t=2;	nx++;
			}
			if(a[i-1].type==2)
			{
				if(t==2)	return 2;
				j++;	t=1;
			}
			if(ch=='|' && t==1)		{	nx = j>nx ? j:nx;	j=0;	ny++;	}
		}
		mglDataA *vv = AddVar(arg[1].c_str());
		mglData *v = dynamic_cast<mglData*>(vv);
		mglDataC *vc = dynamic_cast<mglDataC*>(vv);
		if(v)
		{
			if(t==1)	nx = j>nx ? j:nx;
			if(t==1)	// list of numeric values
			{
				v->Create(nx,ny);
				j=t=0;
				for(i=2;i<k;i++)
				{
					if(arg[i][0]=='|')	{	t++;	j=0;	}
					else
					{	v->a[j+nx*t] = a[i-1].v;	j++;	}
				}
			}
			if(t==2)	// list of data
			{
				v->Set(a[1].d);
				for(long i=2;i<k;i++)	v->Join(*(a[i].d));
			}
			n=1;
		}
		if(vc)
		{
			if(t==1)	nx = j>nx ? j:nx;
			if(t==1)	// list of numeric values
			{
				vc->Create(nx,ny);
				j=t=0;
				for(i=2;i<k;i++)
				{
					if(arg[i][0]=='|')	{	t++;	j=0;	}
					else
					{	vc->a[j+nx*t] = a[i-1].c;	j++;	}
				}
			}
			if(t==2)	// list of data
			{
				vc->Set(a[1].d);
				for(long i=2;i<k;i++)	vc->Join(*(a[i].d));
			}
			n=1;
		}
	}
	return n;
}
//-----------------------------------------------------------------------------
void mglParser::PutArg(std::wstring &str, bool def)
{
	size_t pos = str.find('$',def?10:0);
	while(pos<str.length())
	{
		wchar_t ch = str[pos+1];
		if(ch>='0' && ch<='9')	str.replace(pos,2,par[ch-'0'].w);
		else if(ch>='a' && ch<='z')	str.replace(pos,2,par[ch-'a'+10].w);
		else if(ch=='$')	str.replace(pos,2,L"\uffff");
		else str.replace(pos,1,L"\uffff");
		pos = str.find('$',def?10:0);
	}
	while((pos = str.find(L'\uffff'))<str.length())	str[pos]='$';
}
//-----------------------------------------------------------------------------
std::wstring mgl_trim_ws(const std::wstring &str)
{
	size_t n=str.length(), k, i;
	for(k=0;k<n;k++)	if(str[k]>' ')	break;
	for(i=n;i>k;i--)	if(str[i-1]>' ')	break;
	return str.substr(k,i-k);
}
//-----------------------------------------------------------------------------
int mglParser::ParseDef(std::wstring &str)
{
	if(!skip() && !str.compare(0,3,L"def") && (str[6]==' ' || str[6]=='\t'))
	{
		int res = 1;	mreal d;
		PutArg(str,true);
		size_t end;	bool ss=false;
		for(end=7;str[end] && (str[end]!='#' || ss);end++)
		{	if(str[end]=='\'')	ss=!ss;	}
		const std::wstring s = mgl_trim_ws(str.substr(7,end-7));
		if(!str.compare(3,3,L"ine"))
		{
			int nn = s[1]<='9' ? s[1]-'0' : (s[1]>='a' ? s[1]-'a'+10:-1);
			if(s[0]=='$' && nn>=0 && nn<='z'-'a'+10)
			{
				AddParam(nn, mgl_trim_ws(s.substr(2)).c_str());	return 1;
			}
		}
		if(!str.compare(3,3,L"num"))
		{
			int nn = s[1]<='9' ? s[1]-'0' : (s[1]>='a' ? s[1]-'a'+10:-1);
			if(s[0]=='$' && nn>=0 && nn<='z'-'a'+10)
			{
				res = 0;
				HMDT dd = mglFormulaCalc(mgl_trim_ws(s.substr(2)), this, DataList);
				d = dd->a[0];	delete dd;
				AddParam(nn, mgl_str_num(d).c_str());
			}
			return res+1;
		}
		if(!str.compare(3,3,L"chr"))
		{
			int nn = s[1]<='9' ? s[1]-'0' : (s[1]>='a' ? s[1]-'a'+10:-1);
			if(s[0]=='$' && nn>=0 && nn<='z'-'a'+10)
			{
				res = 0;
				HMDT dd = mglFormulaCalc(mgl_trim_ws(s.substr(2)), this, DataList);
				d=dd->a[0];	delete dd;
				wchar_t buf[2]={0,0};	buf[0] = wchar_t(d);	AddParam(nn, buf);
			}
			return res+1;
		}
	}
	if(!skip() && !str.compare(0,3,L"ask") && (str[3]==' ' || str[3]=='\t'))
	{
		PutArg(str,true);
		std::wstring s = mgl_trim_ws(str.substr(4));
		int nn = s[1]<='9' ? s[1]-'0' : (s[1]>='a' ? s[1]-'a'+10:-1);
		if(s[0]=='$' && nn>=0 && nn<='z'-'a'+10)
		{
			s = mgl_trim_ws(s.substr(2));
			if(s[0]=='\'')	s=s.substr(1,s.length()-2);
			if(mgl_ask_func)
			{
				static wchar_t res[1024];
				mgl_ask_func(s.c_str(),res);
				if(*res)	AddParam(nn, res);
			}
			return mgl_ask_func?1:2;
		}
		else	return 2;
	}
	if(!skip() && !str.compare(0,3,L"for") && (str[3]==' ' || str[3]=='\t'))
	{
		size_t i;	for(i=4;str[i]<=' ';i++);
		// if command have format 'for $N ...' then change it to 'for N ...'
		if(str[i]=='$' && str[i+1]>='0' && str[i+1]<='9')	str[i] = ' ';
		if(str[i]=='$' && str[i+1]>='a' && str[i+1]<='z')	str[i] = ' ';
	}
	return 0;
}
//-----------------------------------------------------------------------------
// return values: 0 - OK, 1 - wrong arguments, 2 - wrong command, 3 - string too long, 4 -- unclosed string
int mglParser::Parse(mglGraph *gr, std::wstring str, long pos)
{
	if(Stop || gr->NeedStop())	return 0;
	curGr = gr->Self();	gr->pr = this;
	str=mgl_trim_ws(str);
	long n,k=0,m=0,mm=0,res;
	// try parse ':' -- several commands in line
	for(n=0;n<long(str.length());n++)
	{
		if(str[n]=='\'' && (n==0 || str[n-1]!='\\'))	k++;
		if(k%2)	continue;
		if(str[n]=='(')	m++;
		if(str[n]==')')	m--;
		if(str[n]=='{')	mm++;
		if(str[n]=='}')	mm--;
		if(str[n]=='#')	break;
		if((str[n]==':' || str[n]=='\n') && k%2==0 && m==0 && mm==0)
		{
			res=Parse(gr,str.substr(0,n),pos);
			if(!res)	res=Parse(gr,str.substr(n+1),pos);
			return res;
		}
	}
	if(k%2 || m || mm)	return 4;	// strings is not closed
	// define parameters or start cycle
	res = ParseDef(str);	if(res)	return res-1;
	// parse arguments (parameters $1, ..., $9)
	PutArg(str,false);	str=mgl_trim_ws(str);

	std::wstring opt;
	std::vector<std::wstring> arg;
	while(!str.empty())	// parse string to substrings (by spaces)
	{
		n = mglFindArg(str);
		if(n<1)	// this is option
		{
			if(str[-n]==';')	opt = str.substr(-n+1);
			if(n<0)	str = str.substr(0,-n);
			break;
		}
		arg.push_back(str.substr(0,n));
		str = mgl_trim_ws(str.substr(n+1));
	}
	// try to find last argument
	if(str[0]!=0 && str[0]!='#' && str[0]!=';')	arg.push_back(str);
	k = arg.size();
	if(k<1) n = 0;
	else
	{
		// fill arguments by its values
		mglArg *a = new mglArg[k];
		FillArg(gr, k, &(arg[0]), a);
		// execute first special (program-flow-control) commands
		if(!skip() && !arg[0].compare(L"stop"))
		{	Stop = true;	delete []a;	return 0;	}
		if(!arg[0].compare(L"func"))
		{	Stop = true;	delete []a;	return 0;	}
		n = FlowExec(gr, arg[0].c_str(),k-1,a);
		if(n)		{	delete []a;	return n-1;	}
		if(skip())	{	delete []a;	return 0;	}
		if(!arg[0].compare(L"define"))
		{
			if(k==3)
			{
				DeleteVar(arg[1].c_str());	// force to delete variable with the same name
				mglNum *v=AddNum(arg[1].c_str());
				if(arg[2][0]=='!')	// complex number is added
				{	HADT dd = mglFormulaCalcC(arg[2].substr(1),this, DataList);
					v->d=NAN;	v->c = dd->a[0];	delete dd;	}
				else
				{	HMDT dd = mglFormulaCalc(arg[2],this, DataList);
					v->c = v->d = dd->a[0];	delete dd;	}
			}
			delete []a;	return k==3?0:1;
		}
		if(!arg[0].compare(L"call"))
		{
			n = 1;
			if(a[0].type==1)
			{
				int na=0;
				n=-IsFunc(a[0].s.w,&na);
				if(n && k!=na+2)
				{
					char buf[64];
					snprintf(buf,64,_("Bad arguments for %ls: %ld instead of %d\n"), a[0].s.w,k-2,na);
					buf[63]=0;	gr->SetWarn(-1,buf);	n = 1;
				}
				else if(n)
				{
					mglFnStack fn;			fn.pos = pos;	fn.stk = stack.size();
					for(int i=0;i<10;i++)	{	fn.par[i] = par[i];	par[i]=L"";	}
					for(int i=1;i<k-1;i++)	AddParam(i,arg[i+1].c_str());
					fn_stack.push_back(fn);	n--;
				}
				else if(AllowFileIO)	// disable external scripts if AllowFileIO=false
				{
					FILE *fp = fopen(a[0].s.s,"rt");
					if(fp)
					{
						mglParser *prs = new mglParser(AllowSetSize);
						prs->DataList.swap(DataList);	prs->NumList.swap(NumList);	prs->Cmd=Cmd;
						for(int i=10;i<30;i++)	prs->AddParam(i,par[i].w);
						prs->Execute(gr,fp);
						for(int i=10;i<30;i++)	AddParam(i,prs->par[i].w);
						DataList.swap(prs->DataList);	NumList.swap(prs->NumList);
						prs->Cmd=0;	delete prs;	fclose(fp);
					}
					else	n=1;
				}
			}
			delete []a;	return n;
		}
		if(!arg[0].compare(L"if") && k>3 && !arg[2].compare(L"then"))
		{
			bool cond=false;	n=0;
			if(a[0].type==2)	cond = (a[0].v!=0);
			else if(a[0].type==0)
			{	cond = a[0].d->FindAny((m>1 && a[1].type==1) ? a[1].s.s:"u");	}
			if(cond)
			{	// alocate new arrays and execute the command itself
				n = FlowExec(gr, arg[3].c_str(),k-4,a+3);
				if(!n && !skip())
				{
					n = PreExec(gr, k-4, &(arg[3]), a+3);
					if(n>0)	n--;
					else if(!arg[3].compare(L"setsize") && !AllowSetSize)	n = 2;
					else	n = Exec(gr, arg[3].c_str(),k-4,a+3, k>3?arg[4]:L"", opt.c_str());
				}
				else	n = skip()?0:n-1;
			}
			delete []a;	DeleteTemp();	return n;
		}
		if(!arg[0].compare(L"do"))
		{
			mglPosStack st(MGL_ST_LOOP);
			st.pos = pos;	st.par = -1;	st.ind = -1;
			stack.push_back(st);	delete []a;	return n;
		}
		if(!arg[0].compare(L"for"))
		{
			if(k<2)	{	delete []a;	return 1;	}
			n = 1;
			char ch = arg[1][0];
			int r = ch-'0';
			if(ch>='a' && ch<='z')	r = 10+ch-'a';
//			int r = int(a[0].v);
			mglPosStack st(MGL_ST_LOOP);
			if(arg[1][1]==0 && (r>=0 && r<40))
			{
				if(a[1].type==0)	{	st.v = *(a[1].d);	n=0;	}
				else if(a[1].type==2 && a[2].type==2 && a[2].v>=a[1].v)
				{
					mreal step = a[3].type==2?a[3].v:1;
					mm = int(step>0 ? (a[2].v-a[1].v)/step : 0);
					if(mm>=0)
					{
						n=0;	st.v.Create(mm+1);
						for(int ii=0;ii<mm+1;ii++)	st.v.a[ii] = a[1].v + step*ii;
					}
				}
				if(n==0)
				{
					st.pos = pos;	st.par = r;	st.ind = 1;
					st.v.nx *= st.v.ny*st.v.nz;	st.v.ny=st.v.nz=1;
					wchar_t buf[32];	mglprintf(buf,32,L"%g",st.v.a[0]);
					AddParam(r, buf);
				}
			}
			if(n)	st.state = MGL_ST_BREAK;
			stack.push_back(st);	delete []a;	return n;
		}
		// alocate new arrays and execute the command itself
		n = PreExec(gr, k, &(arg[0]), a);
		if(n>0)	n--;
		else if(!arg[0].compare(L"setsize") && !AllowSetSize)	n = 2;
		else	n = Exec(gr, arg[0].c_str(),k-1,a, k>1?arg[1]:L"", opt.c_str());
		delete []a;
	}
	DeleteTemp();	return n;
}
//-----------------------------------------------------------------------------
// return values: 0 - OK, 1 - wrong arguments, 2 - wrong command, 3 - string too long, 4 -- unclosed string
int mglParser::ParseDat(mglGraph *gr, std::wstring str, mglData &res)
{
	std::wstring arg[32];
	str = mgl_trim_ws(str);
	long n,k=0;
	for(k=0;k<32;k++)	// parse string to substrings (by spaces)
	{
		n = mglFindArg(str);
		if(n<1)	{	if(n<0)	str=str.substr(0,-n);	break;	}
		arg[k] = str.substr(0,n);//	k++;
		str = str.substr(n+1);	str = mgl_trim_ws(str);
	}
	// try to find last argument
	if(!str.empty() && k<32)	{	arg[k] = str;	k++;	}
	if(k<1) n = 0;
	else
	{	// fill arguments by its values
		mglArg *a = new mglArg[k+1];
		FillArg(gr, k, arg, a+1);	a[0].type=0;	a[0].d=&res;
		// alocate new arrays and execute the command itself
		int i;
		std::string kk;
		const char *id="dsn";
		for(i=0;i<k;i++)	kk += id[a[i].type];
		const mglCommand *rts=FindCommand(arg[0].c_str());
		if(!rts || rts->type!=4)	n = 2;
		else n = rts->exec(gr, k, a, kk.c_str(), 0);
		delete []a;
	}
	return n;
}
//-----------------------------------------------------------------------------
int mglParser::FlowExec(mglGraph *, const std::wstring &com, long m, mglArg *a)
{
	int n=-1;
	if(!ifskip() && !com.compare(L"once"))
	{
		if(a[0].type==2)
		{
			n = 0;
			if(a[0].v)	Skip = !Once;
			else	Skip = Once = false;
		}
		else n = 1;
	}
	else if(!Skip && !com.compare(L"if"))
	{
		bool cond=0;	n=1;
		if(m>2 && a[1].type==0 && !wcscmp(a[1].d->Name(),L"then"))
		{	n = -1;	a[1].d->temp=true;	}	// NOTE: ugly hack :(
		else if(a[0].type==2)
		{	n = 0;	cond = (a[0].v!=0);	}
		else	n = TestCond(m, a[0], a[1], cond);
		if(n==0)
		{	mglPosStack st(cond?MGL_ST_TRUE:MGL_ST_FALSE);	stack.push_back(st);	}
	}
	else if(!Skip && !com.compare(L"while"))
	{
		n=1;	if(stack.size())
		{
			mglPosStack &st = stack.back();
			if(st.state==MGL_ST_LOOP)
			{
				bool cond = false;
				n = TestCond(m, a[0], a[1], cond);
				if(cond)
				{
					if(st.ind<0)	n = -st.pos-1;	// do-while loop
					else if(st.ind<st.v.GetNN())	// next iteration
					{
						wchar_t buf[32];	mglprintf(buf,32,L"%g",st.v.a[st.ind]);
						AddParam(st.par, buf);	st.ind++;	n = -st.pos-1;
					}
					else	stack.pop_back();	// finish
				}
				else	stack.pop_back();
			}
			else if(st.state==MGL_ST_BREAK)
			{	stack.pop_back();	n=0;	}
		}
	}
	else if(!Skip && !com.compare(L"endif"))
	{
		if(stack.size() && stack.back().state<MGL_ST_LOOP)
		{	stack.pop_back();	n=0;	}
		else	n = 1;
	}
	else if(!Skip && !com.compare(L"else"))
	{
		n=1;	if(stack.size())
		{
			mglPosStack &st = stack.back();
			if(st.state<MGL_ST_LOOP)	n=0;
			if(st.state==MGL_ST_TRUE)	st.state = MGL_ST_DONE;
			if(st.state==MGL_ST_FALSE)	st.state = MGL_ST_TRUE;
		}
	}
	else if(!Skip && !com.compare(L"elseif"))
	{
		n=1;	if(stack.size())
		{
			mglPosStack &st = stack.back();
			if(st.state<MGL_ST_LOOP)	n=0;
			if(st.state==MGL_ST_TRUE)	st.state = MGL_ST_DONE;
			if(st.state==MGL_ST_FALSE)
			{
				bool cond=false;
				n = TestCond(m, a[0], a[1], cond);
				if(cond)	st.state = MGL_ST_TRUE;
			}
		}
	}
	else if(!ifskip() && !Skip && !com.compare(L"break"))
	{
		bool nf=true;
		size_t nn = stack.size();
		if(nn)	for(size_t i=nn;i>0;i--)
			if(stack[i-1].state==MGL_ST_LOOP)
			{	nf=false;	stack[i-1].state=MGL_ST_BREAK;	break;	}
		n = nf?1:0;
	}
	else if(!ifskip() && !Skip && !com.compare(L"next"))
	{
		n=1;
		if(stack.size())
		{
			mglPosStack &st = stack.back();
			if(st.state==MGL_ST_LOOP)
			{
				if(st.ind<0)	n = -st.pos-1;	// do-while loop
				else if(st.ind<st.v.GetNN())	// next iteration
				{
					wchar_t buf[32];	mglprintf(buf,32,L"%g",st.v.a[st.ind]);
					AddParam(st.par, buf);	st.ind++;	n = -st.pos-1;
				}
				else	{	stack.pop_back();	n=0;	}	// finish
			}
			else if(st.state==MGL_ST_BREAK)
			{	stack.pop_back();	n=0;	}
		}
	}
	else if(!ifskip() && !Skip && !com.compare(L"continue"))
	{
		bool nf=true;
		size_t nn = stack.size();
		if(nn)	for(size_t i=nn;i>0;i--)
		{
			mglPosStack &st = stack[i-1];
			if(st.state==MGL_ST_LOOP)
			{
				if(st.ind<0)	n = -st.pos-1;	// do-while loop
				else if(st.ind<st.v.GetNN())	// next iteration
				{
					wchar_t buf[32];	mglprintf(buf,32,L"%g",st.v.a[st.ind]);
					AddParam(st.par, buf);	st.ind++;	n = -st.pos-1;
				}
				else	{	st.state = MGL_ST_BREAK;	n=0;	}	// finish
				nf=false;	stack.resize(i);	break;
			}
		}
		if(nf)	n=1;
	}
	else if(!skip() && !com.compare(L"return"))
	{
		if(fn_stack.size()<1)	return 2;
		const mglFnStack &fn=fn_stack.back();
		for(int i=0;i<10;i++)	par[i]=fn.par[i];
		n = -fn.pos-1;	fn_stack.pop_back();
	}
	return n+1;
}
//-----------------------------------------------------------------------------
void mglParser::Execute(mglGraph *gr, FILE *fp, bool print)
{
	if(gr==0 || fp==0)	return;
	std::wstring str;
	wchar_t ch;
	while(!feof(fp) && size_t(ch=fgetwc(fp))!=WEOF)	str.push_back(ch);
	Execute(gr,str.c_str());
	if(print)	printf("%s\n",gr->Message());
}
//-----------------------------------------------------------------------------
void mglParser::Execute(mglGraph *gr, int n, const wchar_t **text)
{
	if(n<1 || text==0)	return;
	long res=0;
	char buf[64];
	Skip=false;	ScanFunc(0);	fn_stack.clear();	stack.clear();
	for(long i=0;i<n;i++)	ScanFunc(text[i]);
	for(long i=0;i<n;i++)
	{
		gr->SetWarn(-1, "");
		gr->SetObjId(i+1+StarObhID);
		long r = Parse(gr,text[i],i+1);
		if(r<0)	{	i = -r-2;	continue;	}
		if(r==1)		snprintf(buf,64,_("\nWrong argument(s) in line %ld"), i+1);
		else if(r==2)	snprintf(buf,64,_("\nWrong command in line %ld"), i+1);
		else if(r==3)	snprintf(buf,64,_("\nString too long in line %ld"), i+1);
		else if(r==4)	snprintf(buf,64,_("\nUnbalanced ' in line %ld"), i+1);
		else if(r==5)	snprintf(buf,64,_("\nChange temporary data in line %ld"), i+1);
		else if(gr->GetWarn()>0)	snprintf(buf,64,_("in line %ld"), i+1);
		else *buf=0;
		buf[63] = 0;
		if(*buf)	gr->SetWarn(-2,buf);
		if(r>0 && r<5)	res=r;
	}
	int code[]={mglScrArg,	mglScrCmd,	mglScrLong,	mglScrStr, mglScrTemp};
	if(res>0)	gr->SetWarn(code[res-1],_("MGL Parser"));
}
//-----------------------------------------------------------------------------
void mglParser::Execute(mglGraph *gr, const wchar_t *text)
{
	size_t s = mgl_wcslen(text)+1, n=1;
	wchar_t *wcs = new wchar_t[s];
	const wchar_t **str;
	for(size_t i=0;i<s;i++)	if(text[i]=='\n')	n++;
	str = (const wchar_t **)malloc(n*sizeof(wchar_t *));
	memcpy(wcs, text, s*sizeof(wchar_t));
	str[0] = wcs;	n=1;
	long next=0;
	Stop = false;
	for(size_t i=0;i<s;i++)
	{
		if(text[i]=='\\')	next = i;
		else if(text[i]>' ')next = 0;
		if(text[i]=='\n')
		{	// if string need to be continued then I but ' ' instead of 0x0 and
			// pointer next string to 0x0. Last one for keeping number of strings.
			if(next)
			{	for(size_t ii=next;ii<=i;ii++)	wcs[ii]='\b';	str[n] = wcs+s-1;	next=0;	}
			else
			{	wcs[i]=0;	str[n] = wcs+i+1;	}
			n++;
		}
	}
	Execute(gr, n, str);
	delete []wcs;	free(str);
}
//-----------------------------------------------------------------------------
void mglParser::Execute(mglGraph *gr, const char *text)
{
	MGL_TO_WCS(text, Execute(gr, wcs));
}
//-----------------------------------------------------------------------------
void mglParser::DeleteVar(const char *name)
{
	MGL_TO_WCS(name,DeleteVar(wcs));
}
//-----------------------------------------------------------------------------
void mglParser::DeleteVar(const wchar_t *name)
{
	for(size_t i=0;i<DataList.size();i++)
		if(DataList[i] && !wcscmp(DataList[i]->Name(),name))
		{	mglDataA *u=DataList[i];	DataList[i]=0;	delete u;	}
}
//-----------------------------------------------------------------------------
void mglParser::AddCommand(const mglCommand *cmd)
{
	// determine the number of symbols
	size_t mp=0;	while(Cmd[mp].name[0])	mp++;
	size_t mc=0;	while(cmd[mc].name[0])	mc++;
	// copy all together
	mglCommand *buf = new mglCommand[mp+mc+1];
	memcpy(buf, cmd, mc*sizeof(mglCommand));
	memcpy(buf+mc, Cmd, (mp+1)*sizeof(mglCommand));
	qsort(buf, mp+mc, sizeof(mglCommand), mgl_cmd_cmp);	// sort it
#pragma omp critical(cmd_parser)
	{	if(Cmd!=BaseCmd)   delete []Cmd;	Cmd = buf;	}
}
//-----------------------------------------------------------------------------
HMPR MGL_EXPORT mgl_create_parser()		{	return new mglParser;	}
void MGL_EXPORT mgl_delete_parser(HMPR p)	{	delete p;	}
void MGL_EXPORT mgl_parser_add_param(HMPR p, int id, const char *str)			{	p->AddParam(id,str);	}
void MGL_EXPORT mgl_parser_add_paramw(HMPR p, int id, const wchar_t *str)		{	p->AddParam(id,str);	}
MGL_EXPORT mglDataA *mgl_parser_add_var(HMPR p, const char *name)	{	return p->AddVar(name);	}
MGL_EXPORT_PURE mglDataA *mgl_parser_find_var(HMPR p, const char *name)	{	return p->FindVar(name);}
void MGL_EXPORT mgl_parser_del_var(HMPR p, const char *name)	{	p->DeleteVar(name);	}
MGL_EXPORT mglDataA *mgl_parser_add_varw(HMPR p, const wchar_t *name)	{	return p->AddVar(name);	}
MGL_EXPORT_PURE mglDataA *mgl_parser_find_varw(HMPR p, const wchar_t *name)	{	return p->FindVar(name);}
void MGL_EXPORT mgl_parser_del_varw(HMPR p, const wchar_t *name)	{	p->DeleteVar(name);	}
int MGL_EXPORT mgl_parse_line(HMGL gr, HMPR p, const char *str, int pos)
{	return p->Parse(gr, str, pos);	}
int MGL_EXPORT mgl_parse_linew(HMGL gr, HMPR p, const wchar_t *str, int pos)
{	return p->Parse(gr, str, pos);	}
void MGL_EXPORT mgl_parse_text(HMGL gr, HMPR p, const char *str)
{	p->Execute(gr, str);	}
void MGL_EXPORT mgl_parse_textw(HMGL gr, HMPR p, const wchar_t *str)
{	p->Execute(gr, str);	}
void MGL_EXPORT mgl_parse_file(HMGL gr, HMPR p, FILE *fp, int print)
{	p->Execute(gr,fp,print);	}
void MGL_EXPORT mgl_parser_restore_once(HMPR p)	{	p->RestoreOnce();	}
void MGL_EXPORT mgl_parser_stop(HMPR p)	{	p->Stop = true;		}
void MGL_EXPORT mgl_parser_allow_setsize(HMPR p, int a)	{	p->AllowSetSize= a;	}
void MGL_EXPORT mgl_parser_allow_file_io(HMPR p, int a)	{	p->AllowFileIO = a;	}
void MGL_EXPORT mgl_parser_allow_dll_call(HMPR p, int a){	p->AllowDllCall = a;	}
//-----------------------------------------------------------------------------
#define _PR_	((mglParser *)(*p))
uintptr_t MGL_EXPORT mgl_create_parser_()	{	return uintptr_t(new mglParser);	}
void MGL_EXPORT mgl_delete_parser_(uintptr_t* p)	{	delete _PR_;	}
void MGL_EXPORT mgl_parser_add_param_(uintptr_t* p, int *id, const char *str, int l)
{	char *s=new char[l+1];		memcpy(s,str,l);	s[l]=0;
	_PR_->AddParam(*id, s);		delete []s;	}
/*===!!! NOTE !!! You must not delete obtained data arrays !!!===============*/
uintptr_t MGL_EXPORT mgl_parser_add_var_(uintptr_t* p, const char *name, int l)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	mglDataA *v=_PR_->AddVar(s);	delete []s;	return uintptr_t(v);	}
/*===!!! NOTE !!! You must not delete obtained data arrays !!!===============*/
uintptr_t MGL_EXPORT mgl_parser_find_var_(uintptr_t* p, const char *name, int l)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	mglDataA *v=_PR_->FindVar(s);	delete []s;	return uintptr_t(v);	}
void MGL_EXPORT mgl_parser_del_var_(uintptr_t* p, const char *name, int l)
{	char *s=new char[l+1];		memcpy(s,name,l);	s[l]=0;
	_PR_->DeleteVar(s);	delete []s;	}
int MGL_EXPORT mgl_parse_line_(uintptr_t* gr, uintptr_t* p, const char *str, int *pos, int l)
{	char *s=new char[l+1];		memcpy(s,str,l);	s[l]=0;
	int r = _PR_->Parse(_GR_, s, *pos);	delete []s;	return r;	}
void MGL_EXPORT mgl_parse_text_(uintptr_t* gr, uintptr_t* p, const char *str, int l)
{	char *s=new char[l+1];		memcpy(s,str,l);	s[l]=0;
	_PR_->Execute(_GR_, s);	delete []s;	}
void MGL_EXPORT mgl_parser_restore_once_(uintptr_t* p)	{	_PR_->RestoreOnce();	}
void MGL_EXPORT mgl_parser_allow_setsize_(uintptr_t* p, int *a)	{	_PR_->AllowSetSize= *a;	}
void MGL_EXPORT mgl_parser_allow_file_io_(uintptr_t* p, int *a)	{	_PR_->AllowFileIO = *a;	}
void MGL_EXPORT mgl_parser_allow_dll_call_(uintptr_t* p, int *a){	_PR_->AllowDllCall= *a;	}
void MGL_EXPORT mgl_parser_stop_(uintptr_t* p)	{	_PR_->Stop = true;	}
//-----------------------------------------------------------------------------
long MGL_EXPORT mgl_use_parser(HMPR pr, int inc)
{	pr->InUse+=inc;	return pr->InUse;	}
long MGL_EXPORT mgl_use_parser_(uintptr_t *p, int *inc)
{	_PR_->InUse+=*inc;	return _PR_->InUse;	}
//---------------------------------------------------------------------------
MGL_EXPORT_PURE mglDataA *mgl_parser_get_var(HMPR p, unsigned long id)
{	return id<p->DataList.size()?p->DataList[id]:0;	}
uintptr_t MGL_EXPORT_PURE mgl_parser_get_var_(uintptr_t* p, unsigned long *id)
{	return uintptr_t(mgl_parser_get_var(_PR_,*id));	}
long MGL_EXPORT_PURE mgl_parser_num_var(HMPR p)
{	return p->DataList.size();	}
long MGL_EXPORT_PURE mgl_parser_num_var_(uintptr_t* p)
{	return mgl_parser_num_var(_PR_);	}
long MGL_EXPORT_PURE mgl_parser_num_const(HMPR p)
{	return p->NumList.size();	}
long MGL_EXPORT_PURE mgl_parser_num_const_(uintptr_t* p)
{	return mgl_parser_num_const(_PR_);	}
MGL_EXPORT_PURE mglNum *mgl_parser_get_const(HMPR p, unsigned long id)
{	return id<p->NumList.size()?p->NumList[id]:0;	}
uintptr_t MGL_EXPORT_PURE mgl_parser_get_const_(uintptr_t* p, unsigned long *id)
{	return uintptr_t(mgl_parser_get_const(_PR_,*id));	}
//---------------------------------------------------------------------------
int MGL_EXPORT_PURE mgl_parser_cmd_type(HMPR pr, const char *name)
{
	const mglCommand *cmd = pr->FindCommand(name);
	return cmd ? cmd->type + 1 : 0;
}
int MGL_EXPORT_PURE mgl_parser_cmd_type_(uintptr_t* p, const char *str, int l)
{	char *s=new char[l+1];	memcpy(s,str,l);	s[l]=0;
	l = mgl_parser_cmd_type(_PR_, s);	delete []s;	return l;	}
//---------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mgl_parser_cmd_desc(HMPR pr, const char *name)
{
	const mglCommand *cmd = pr->FindCommand(name);
	return cmd ? cmd->desc : 0;
}
MGL_EXPORT_PURE const char *mgl_parser_cmd_frmt(HMPR pr, const char *name)
{
	const mglCommand *cmd = pr->FindCommand(name);
	return cmd ? cmd->form : 0;
}
//---------------------------------------------------------------------------
MGL_EXPORT_PURE const char *mgl_parser_cmd_name(HMPR pr, long id)
{	return (id<mgl_parser_cmd_num(pr) && id>=0) ? pr->Cmd[id].name:"";	}
long MGL_EXPORT_PURE mgl_parser_cmd_num(HMPR pr)
{	long i=0;	while(pr->Cmd[i].name[0])	i++; 	return i;	}
//---------------------------------------------------------------------------
HMDT MGL_EXPORT mgl_parser_calc(HMPR pr, const char *formula)
{	HMDT d=0;	MGL_TO_WCS(formula,d = mgl_parser_calcw(pr,wcs));	return d;	}
HMDT MGL_EXPORT mgl_parser_calcw(HMPR pr, const wchar_t *formula)
{	return mglFormulaCalc(formula,pr, pr->DataList);	}
uintptr_t MGL_EXPORT mgl_parser_calc_(uintptr_t *p, const char *str,int l)
{	char *s=new char[l+1];	memcpy(s,str,l);	s[l]=0;
	uintptr_t d = (uintptr_t)mgl_parser_calc(_PR_, s);	delete []s;	return d;	}
//---------------------------------------------------------------------------
HADT MGL_EXPORT mgl_parser_calc_complex(HMPR pr, const char *formula)
{	HADT d=0;	MGL_TO_WCS(formula,d = mgl_parser_calc_complexw(pr,wcs));	return d;	}
HADT MGL_EXPORT mgl_parser_calc_complexw(HMPR pr, const wchar_t *formula)
{	return mglFormulaCalcC(formula,pr, pr->DataList);	}
uintptr_t MGL_EXPORT mgl_parser_calc_complex_(uintptr_t *p, const char *str,int l)
{	char *s=new char[l+1];	memcpy(s,str,l);	s[l]=0;
	uintptr_t d = (uintptr_t)mgl_parser_calc_complex(_PR_, s);	delete []s;	return d;	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_parser_del_all(HMPR p)	{	p->DeleteAll();	}
void MGL_EXPORT mgl_parser_del_all_(uintptr_t *p)	{	_PR_->DeleteAll();	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_parser_load(HMPR pr, const char *so_name)
{
	if(!pr->AllowDllCall)	return;
#if MGL_HAVE_LTDL
	lt_dlhandle so = lt_dlopen(so_name);
	if(!so)	return;
	const mglCommand *cmd = (const mglCommand *)lt_dlsym(so,"mgl_cmd_extra");
	bool exist = true;
	if(cmd)	for(size_t i=0;cmd[i].name[0];i++)
		if(!pr->FindCommand(cmd[i].name))	exist=false;
	if(exist)	{	lt_dlclose(so);	return;	}	// all commands already presents
	else	pr->DllOpened.push_back(so);
	pr->AddCommand(cmd);
#endif
}
void MGL_EXPORT mgl_parser_load_(uintptr_t *p, const char *dll_name,int l)
{	char *s=new char[l+1];	memcpy(s,dll_name,l);	s[l]=0;
	mgl_parser_load(_PR_, s);	delete []s;	}
//---------------------------------------------------------------------------
struct mglRKdat
{
	mglDataA *v;
	std::wstring e;
	bool cmplx;
	mglDataC cin,c1,c2,c3,c4, *cc;
	mglData  din,d1,d2,d3,d4, *dd;
	mglRKdat(mglDataA *var, std::wstring &eq):v(var), e(eq)
	{	cmplx = dynamic_cast<mglDataC*>(var);	cc=0;	dd=0;	}
	void allocate()
	{
		if(cmplx)
		{	cc = dynamic_cast<mglDataC*>(v);	cin.Set(v);	}
		else
		{	dd = dynamic_cast<mglData*>(v);		din.Set(v);	}
	}
};
void MGL_EXPORT mgl_rk_step_w(HMPR pr, const wchar_t *Eqs, const wchar_t *Vars, mreal dt)
{
	const std::wstring eqs(Eqs);
	const std::wstring vars(Vars);
	std::vector<mglRKdat> rkv;
	size_t iv=0,jv=0,ie=0,je=0;
	while(1)
	{
		iv = vars.find(';',jv);	ie = eqs.find(';',je);
		mglDataA *vv=mgl_parser_find_varw(pr,vars.substr(jv,iv-jv).c_str());
		std::wstring eq = eqs.substr(je,ie-je).c_str();
		if(vv)	rkv.push_back(mglRKdat(vv, eq ));
		jv = iv+1;	je = ie+1;
		if(iv==std::wstring::npos || ie==std::wstring::npos)	break;
	}
	for(size_t i=0;i<rkv.size();i++)	rkv[i].allocate();
	mreal hh = dt/2;
	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cmplx)	rk.c1.Move(mglFormulaCalcC(rk.e, pr, pr->DataList));
		else	rk.d1.Move(mglFormulaCalc(rk.e, pr, pr->DataList));
	}
	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cc)
		{
			long n = rk.cc->GetNN();	dual a = hh*rk.c1.a[0];
			if(rk.c1.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + hh*rk.c1.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + a;
		}
		if(rk.dd)
		{
			long n = rk.dd->GetNN();	mreal a = hh*rk.d1.a[0];
			if(rk.d1.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + hh*rk.d1.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + a;
		}
	}

	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cmplx)	rk.c2.Move(mglFormulaCalcC(rk.e, pr, pr->DataList));
		else	rk.d2.Move(mglFormulaCalc(rk.e, pr, pr->DataList));
	}
	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cc)
		{
			long n = rk.cc->GetNN();	dual a = hh*rk.c2.a[0];
			if(rk.c2.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + hh*rk.c2.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + a;
		}
		if(rk.dd)
		{
			long n = rk.dd->GetNN();	mreal a = hh*rk.d2.a[0];
			if(rk.d2.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + hh*rk.d2.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + a;
		}
	}

	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cmplx)	rk.c3.Move(mglFormulaCalcC(rk.e, pr, pr->DataList));
		else	rk.d3.Move(mglFormulaCalc(rk.e, pr, pr->DataList));
	}
	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cc)
		{
			long n = rk.cc->GetNN();	dual a = dt*rk.c3.a[0];
			if(rk.c3.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + dt*rk.c3.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + a;
		}
		if(rk.dd)
		{
			long n = rk.dd->GetNN();	mreal a = dt*rk.d3.a[0];
			if(rk.d3.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + dt*rk.d3.a[j];
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + a;
		}
	}

	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cmplx)	rk.c4.Move(mglFormulaCalcC(rk.e, pr, pr->DataList));
		else	rk.d4.Move(mglFormulaCalc(rk.e, pr, pr->DataList));
	}
	for(size_t i=0;i<rkv.size();i++)
	{
		mglRKdat &rk = rkv[i];
		if(rk.cc)
		{
			long n = rk.cc->GetNN();
			dual a = (rk.c1.a[0]+rk.c2.a[0]+mreal(2)*(rk.c3.a[0]+rk.c4.a[0]))*(dt/6);
			if(rk.c1.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + (rk.c1.a[j]+rk.c2.a[j]+mreal(2)*(rk.c3.a[j]+rk.c4.a[j]))*(dt/6);
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.cc->a[j] = rk.cin.a[j] + a;
		}
		if(rk.dd)
		{
			long n = rk.dd->GetNN();
			mreal a = (rk.d1.a[0]+rk.d2.a[0]+2*(rk.d3.a[0]+rk.d4.a[0]))*(dt/6);
			if(rk.d1.GetNN()==n)
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + (rk.d1.a[j]+rk.d2.a[j]+2*(rk.d3.a[j]+rk.d4.a[j]))*(dt/6);
			else
#pragma omp parallel for
				for(long j=0;j<n;j++)	rk.dd->a[j] = rk.din.a[j] + a;
		}
	}
}
void MGL_EXPORT mgl_rk_step(HMPR pr, const char *Eqs, const char *Vars, mreal dt)
{
	if(Eqs && *Eqs && Vars && *Vars)
	{
		size_t s=mbstowcs(0,Eqs,0), w=mbstowcs(0,Vars,0);
		wchar_t *eqs=new wchar_t[s+1];	mbstowcs(eqs,Eqs ,s);	eqs[s]=0;
		wchar_t *wcs=new wchar_t[s+1];	mbstowcs(wcs,Vars,s);	wcs[w]=0;
		mgl_rk_step_w(pr,eqs,wcs,dt);	delete []wcs;	delete []eqs;
	}
}
void MGL_EXPORT mgl_rk_step_(uintptr_t *p, const char *eqs, const char *vars, double *dt,int l,int m)
{	char *e=new char[l+1];	memcpy(e,eqs,l);	e[l]=0;
	char *s=new char[m+1];	memcpy(s,vars,m);	s[m]=0;
	mgl_rk_step(_PR_,e,s,*dt);	delete []e;	delete []s;	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_parser_variant(HMPR p, int var)	{	p->SetVariant(var);	}
void MGL_EXPORT mgl_parser_variant_(uintptr_t *p, int *var)	{	mgl_parser_variant(_PR_,*var);	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_parser_openhdf(HMPR p, const char *fname)
{
	const char * const *res = mgl_datas_hdf_str(fname);
	if(!res)	return;
	for(size_t n=0;res[n][0];n++)
	{
		mglDataA *d = p->AddVar(res[n]);
		mglData *dr = dynamic_cast<mglData*>(d);
		mglDataC *dc = dynamic_cast<mglDataC*>(d);
		if(dr)	dr->ReadHDF(fname,res[n]);
		if(dc)	dc->ReadHDF(fname,res[n]);
	}
}
void MGL_EXPORT mgl_parser_openhdf_(uintptr_t *p, const char *fname,int l)
{	char *s=new char[l+1];	memcpy(s,fname,l);	s[l]=0;
	mgl_parser_openhdf(_PR_,s);	delete []s;	}
//---------------------------------------------------------------------------
void MGL_EXPORT mgl_parser_start_id(HMPR pr, int id)
{	pr->StarObhID = id;	}
void MGL_EXPORT mgl_parser_start_id_(uintptr_t* p, int *id)
{	mgl_parser_start_id(_PR_, *id);	}

//---------------------------------------------------------------------------
mglCommand mgls_prg_cmd[] = {
	{"ask",_("Define parameter from user input"),"ask $N 'question'", 0, 6},
	{"break",_("Break for-loop"),"break", 0, 6},
	{"call",_("Execute script in external file"),"call 'name' [args]", 0, 6},
	{"continue",_("Skip commands and iterate for-loop again"),"continue", 0, 6},
	{"do",_("Begin of do-while loop"),"do", 0, 6},
	{"defchr",_("Define parameter as character"),"defchr $N val", 0, 6},
	{"define",_("Define constant or parameter"),"define $N sth | Var val", 0, 6},
	{"defnum",_("Define parameter as numerical value"),"defnum $N val", 0, 6},
//	{"defpal",_("Define parameter as palette color"),"defpal $N val", 0, 6},
	{"else",_("Execute if condition is false"),"else", 0, 6},
	{"elseif",_("Conditional operator"),"elseif val|Dat ['cond']", 0, 6},
	{"endif",_("Finish if/else block"),"endif", 0, 6},
	{"for",_("For loop"),"for $N v1 v2 [dv] | $N Dat", 0, 6},
	{"func",_("Start function definition and stop execution of main script"),"func 'name' [narg]", 0, 6},
	{"if",_("Conditional operator"),"if val|Dat ['cond']", 0, 6},
	{"list",_("Creates new variable from list of numbers or data"),"list Var v1 ...|Var D1 ...", 0, 4},
	{"next",_("Start next for-loop iteration"),"next", 0, 6},
	{"once",_("Start/close commands which should executed only once"),"once val", 0, 6},
	{"return",_("Return from function"),"return", 0, 6},
	{"stop",_("Stop execution"),"stop", 0, 6},
	{"while",_("Condition of do-while loop"),"while val|Dat ['cond']", 0, 6},
{"","","",NULL,0}};
//-----------------------------------------------------------------------------
