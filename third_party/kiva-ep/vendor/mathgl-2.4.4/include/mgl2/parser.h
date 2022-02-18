/***************************************************************************
 * parser.h is part of Math Graphic Library
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
#ifndef _MGL_PARSER_H_
#define _MGL_PARSER_H_

#ifdef __cplusplus
#include "mgl2/mgl.h"
#if MGL_HAVE_LTDL
#include <ltdl.h>
#endif
//-----------------------------------------------------------------------------
/// Structure for the command argument.
struct mglArg
{
	int type;		///< Type of argument {0-data,1-string,2-number}
	mglDataA *d;	///< Pointer to data (used if type==0)
	mglString s;	///< String with parameters
	mreal v;		///< Numerical value (used if type==2)
	dual c;			///< Numerical complex value (used if type==2)
	mglArg():type(-1),d(0),v(0),c(0.)	{}
};
//-----------------------------------------------------------------------------
/// Structure for MGL command
struct mglCommand
{
	const char *name;	///< Name of command
	const char *desc;	///< Short command description (can be NULL)
	const char *form;	///< Format of command arguments (can be NULL)
	/// Function for executing (plotting)
	int (*exec)(mglGraph *gr, long n, mglArg *a, const char *k, const char *opt);
	/// Type of command: 0 - special plot, 1 - other plot,
	///	2 - setup, 3 - data handle, 4 - data create, 5 - subplot, 6 - program
	///	7 - 1d plot, 8 - 2d plot, 9 - 3d plot, 10 - dd plot, 11 - vector plot
	///	12 - axis, 13 - primitives, 14 - axis setup, 15 - text/legend, 16 - data transform
	int type;
};
extern mglCommand mgls_prg_cmd[], mgls_dat_cmd[], mgls_grf_cmd[], mgls_set_cmd[], mgls_prm_cmd[];
//-----------------------------------------------------------------------------
/// Structure for function name and position.
struct mglFunc
{
	long pos;
	int narg;
	mglString func;
	mglFunc(long p, const wchar_t *f);
	mglFunc(const mglFunc &f):pos(f.pos),narg(f.narg),func(f.func)	{}
	mglFunc():pos(-1),narg(-1)	{}
	const mglFunc &operator=(const mglFunc &f)
	{	pos=f.pos;	narg=f.narg;	func=f.func;	return f;	}
};
//-----------------------------------------------------------------------------
/// Structure for stack of functions and its arguments.
struct mglFnStack
{
	long pos;	///< position to return
	size_t stk;	///< stack at 'call'
	mglString par[10];	///< input parameters
	mglFnStack():pos(0),stk(0)	{}
};
//-----------------------------------------------------------------------------
/// Structure for stack of if|for|while.
#define MGL_ST_TRUE		0	// condition true
#define MGL_ST_FALSE	1	// condition false
#define MGL_ST_DONE		2	// condition done
#define MGL_ST_LOOP		4	// normal loop
#define MGL_ST_BREAK	8	// loop break
#define MGL_ST_STOP		(MGL_ST_FALSE|MGL_ST_DONE|MGL_ST_BREAK)
#define MGL_ST_SKIP		(MGL_ST_FALSE|MGL_ST_DONE)
struct mglPosStack
{
	int pos;	///< position to return
	mglData v;	///< data to iterate
	long ind;	///< index in data array
	int par;	///< for-parameter
	unsigned state;	///< state of stack item
	mglPosStack(int st=MGL_ST_LOOP):pos(-1),ind(0),par(-1),state(st)	{}
};
//-----------------------------------------------------------------------------
/// Function for asking question in console mode
void MGL_EXPORT mgl_ask_gets(const wchar_t *quest, wchar_t *res);
//-----------------------------------------------------------------------------
/// Structure for the command argument (see mglGraph::Exec()).
class mglParser
{
friend void mgl_export(wchar_t *out, const wchar_t *in, int type);
public:
#if MGL_HAVE_LTDL
	std::vector<lt_dlhandle> DllOpened;	///< Opened external DLL (keep )
#endif
	std::vector<mglDataA*> DataList;	///< List with data and its names
	std::vector<mglNum*> NumList;	///< List with numbers and its names
	bool AllowDllCall;	///< Allow calls from external dynamic libraries
	bool AllowSetSize;	///< Allow using setsize command
	bool AllowFileIO;	///< Allow reading/saving files
	volatile bool Stop;	///< Stop command was. Flag prevent further execution
	mglCommand *Cmd;	///< Table of MGL commands (can be changed by user). It MUST be sorted by 'name'!!!
	long InUse;			///< Smart pointer (number of users)
	HMGL curGr;			///< Current grapher
	int StarObhID;		///< staring object id

	mglParser(bool setsize=false);
	virtual ~mglParser();
	/// Find the command by the keyword name
	const mglCommand *FindCommand(const char *name) const MGL_FUNC_PURE;
	const mglCommand *FindCommand(const wchar_t *name) const MGL_FUNC_PURE;
	/// Parse and execute the string of MGL script
	inline int Parse(HMGL gr, const char *str, long pos=0)
	{	mglGraph GR(gr);	return Parse(&GR,str,pos);	}
	int Parse(mglGraph *gr, const char *str, long pos=0);
	/// Parse and execute the unicode string of MGL script
	inline int Parse(HMGL gr, const wchar_t *str, long pos=0)
	{	mglGraph GR(gr);	return Parse(&GR,str,pos);	}
	int Parse(mglGraph *gr, std::wstring str, long pos=0);
	/// Execute MGL script file fname
	inline void Execute(HMGL gr, FILE *fp, bool print=false)
	{	mglGraph GR(gr);	Execute(&GR,fp,print);	}
	void Execute(mglGraph *gr, FILE *fp, bool print=false);
	/// Execute MGL script from array of lines
	inline void Execute(HMGL gr, int num, const wchar_t **text)
	{	mglGraph GR(gr);	Execute(&GR,num,text);	}
	void Execute(mglGraph *gr, int num, const wchar_t **text);
	/// Execute MGL script text with '\n' separated lines
	inline void Execute(HMGL gr, const wchar_t *text)
	{	mglGraph GR(gr);	Execute(&GR,text);	}
	void Execute(mglGraph *gr, const wchar_t *text);
	/// Execute MGL script text with '\n' separated lines
	inline void Execute(HMGL gr, const char *text)
	{	mglGraph GR(gr);	Execute(&GR,text);	}
	void Execute(mglGraph *gr, const char *text);
	/// Scan for functions (use NULL for reset)
	void ScanFunc(const wchar_t *line);
	/// Check if name is function and return its address (or 0 if no)
	long IsFunc(const wchar_t *name, int *narg=0);
	/// Find variable or return 0 if absent
	mglDataA *FindVar(const char *name) MGL_FUNC_PURE;
	mglDataA *FindVar(const wchar_t *name) MGL_FUNC_PURE;
	/// Find variable or create it if absent
	mglDataA *AddVar(const char *name);
	mglDataA *AddVar(const wchar_t *name);
	/// Find number or return 0 if absent
	mglNum *FindNum(const char *name) MGL_FUNC_PURE;
	mglNum *FindNum(const wchar_t *name) MGL_FUNC_PURE;
	/// Find number or create it if absent
	mglNum *AddNum(const char *name);
	mglNum *AddNum(const wchar_t *name);
	/// Add string for parameter $1, ..., $9
	void AddParam(int n, const char *str);
	void AddParam(int n, const wchar_t *str);
	/// Add new MGL command(s) (last command MUST HAVE name[0]=0 !!!)
	void AddCommand(const mglCommand *cmd);
	/// Restore Once flag
	inline void RestoreOnce()	{	Once = true;	}
	/// Delete variable by its name
	void DeleteVar(const char *name);
	void DeleteVar(const wchar_t *name);
	/// Delete all data variables
	void DeleteAll();
	/// Delete temporary data arrays
	inline void DeleteTemp()
	{	for(size_t i=0;i<DataList.size();i++)	if(DataList[i] && DataList[i]->temp)
		{	mglDataA *u=DataList[i];	DataList[i]=0;	delete u;	}	}
	/// Set variant of argument(s) separated by '?' to be used
	inline void SetVariant(int var=0)	{	Variant = var<=0?0:var;	}
protected:
	static mglCommand *BaseCmd;	///< Base table of MGL commands. It MUST be sorted by 'name'!!!
	static void FillBaseCmd();	///< Fill BaseCmd at initialization stage

	///< Test if condition is not-valid (n=1) or false (0) or true (1)
	int TestCond(long m, const mglArg &a0, mglArg &a1, bool &cond)
	{
		int n = 1;
		if(a0.type==2)	{	cond = a0.v!=0;	n=0;	}
		else if(a0.type==0)
		{	n=0;	cond = a0.d->FindAny((m>1 && a1.type==1) ? a1.s.s:"u");	}
		return n;
	}
private:
//	long parlen;		///< Length of parameter strings
	mglString par[40];	///< Parameter for substituting instead of $1, ..., $9
	bool Once;			///< Flag for command which should be executed only once
	bool Skip;			///< Flag that commands should be skiped (inside 'once' block)
	std::vector<mglPosStack> stack;	///< Stack of if|for|while commands
	std::vector<mglFunc> func;	///< function names and position
	std::vector<mglFnStack> fn_stack;	///< function calls stack
	unsigned Variant;	///< Select variant of argument(s) separated by '?'

	/// Parse command
	int Exec(mglGraph *gr, const wchar_t *com, long n, mglArg *a, const std::wstring &var, const wchar_t *opt);
	/// Fill arguments a from strings
	void FillArg(mglGraph *gr, int n, std::wstring *arg, mglArg *a);
	/// PreExecute stage -- parse some commands and create variables
	int PreExec(mglGraph *gr, long n, std::wstring *arg, mglArg *a);
	/// Execute program-flow control commands
	int FlowExec(mglGraph *gr, const std::wstring &com, long n, mglArg *a);
	/// Parse and execute the unicode string of MGL script
	int ParseDat(mglGraph *gr, std::wstring str, mglData &res);
	/// Define '$' parameters or start for loop
	int ParseDef(std::wstring &str);
	/// Parse $N arguments
	void PutArg(std::wstring &str, bool def);
	/// In skip mode
	bool inline ifskip()
	{	return ( stack.size() && (stack.back().state & MGL_ST_SKIP) );	}
	bool inline skip()
	{	return (Skip || (stack.size() && (stack.back().state & MGL_ST_STOP) ));	}
	bool CheckForName(const std::wstring &s);	// check if name is valid for new data
};
//-----------------------------------------------------------------------------
#endif
#endif
