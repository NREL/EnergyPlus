/***************************************************************************
 * window.cpp is part of Math Graphic Library
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
#include "mgl2/canvas_wnd.h"
//-----------------------------------------------------------------------------
mglCanvasWnd::mglCanvasWnd() : mglCanvas()
{
	Setup();	LoadFunc=0;	FuncPar=0;	DrawFunc=0;	ClickFunc=0;
	GG = 0;		NumFig = 0;	CurFig=-1;	PropFunc=0;	PropPar=0;
#if MGL_HAVE_PTHR_WIDGET
	mutex=0;
#endif
}
//-----------------------------------------------------------------------------
mglCanvasWnd::~mglCanvasWnd()	{	if(GG) free(GG);	}
//-----------------------------------------------------------------------------
void mglCanvasWnd::SetCurFig(int c)
{
	CurFig=c;
	if(get(MGL_VECT_FRAME) && c>=0 && c<(long)DrwDat.size() && DrawFunc)
		GetFrame(c);
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::ResetFrames()
{
	if(GG)	free(GG);
	GG = 0;	NumFig = CurFig = 0;
	mglCanvas::ResetFrames();
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::SetSize(int w,int h,bool)
{
	if(DrawFunc)	ResetFrames();
	mglCanvas::SetSize(w,h,false);
//	if(Wnd)	Wnd->size(w,h);
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::EndFrame()
{
	CurFig = CurFrameId-1;
	if(!GG)
	{
		GG = (unsigned char *)malloc(3*Width*Height);
		NumFig = 1;		CurFig = 0;
	}
	else if(CurFig>NumFig-1)
	{
		GG = (unsigned char *)realloc(GG,3*(CurFig+1)*Width*Height);
		NumFig = CurFig+1;
	}
	mglCanvas::EndFrame();
	memcpy(GG + CurFig*Width*Height*3,G,3*Width*Height);
	CurFig++;
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::SetFrame(long i)
{
	mglCanvas::SetFrame(i);
	if(i>=0 && i<NumFig)	memcpy(GG + i*Width*Height*3,G,3*Width*Height);
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::DelFrame(long i)
{
	if(i<0 || i>=CurFrameId)	return;
	if(CurFig>=i)	CurFig--;
	long n = Width*Height*3;
	if(CurFrameId-i>1)	memmove(GG+i*n, GG+i*n+n, n*(CurFrameId-i-1));
	mglCanvas::DelFrame(i);
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::SetDrawFunc(int (*draw)(mglBase *gr, void *p), void *par, void (*reload)(void *p))
{
	if(draw)
	{
		ResetFrames();
		if(get(MGL_CLF_ON_UPD))	DefaultPlotParam();
		const std::string loc = setlocale(LC_NUMERIC, "C");
		// use frames for quickly redrawing while adding/changing primitives
		if(mgl_is_frames(this))	NewFrame();

		int n = draw(this,par);
		if(n<NumFig && n>=0)	NumFig = n;
		DrawFunc = draw;		FuncPar = par;
		LoadFunc = reload;

		if(mgl_is_frames(this))	EndFrame();
		if(n>=0)	SetCurFig(0);
		setlocale(LC_NUMERIC, loc.c_str());
	}
	else	LoadFunc = 0;
}
//-----------------------------------------------------------------------------
const unsigned char *mglCanvasWnd::GetBits()
{
	const unsigned char *g = mglCanvas::GetBits();
	if(GG && NumFig>0 && CurFig<NumFig && CurFig>=0 && !get(MGL_VECT_FRAME))
		g = GG + CurFig*Width*Height*3;
	return g;
}
//-----------------------------------------------------------------------------
void mglCanvasWnd::ReLoad()
{
	if(LoadFunc)
	{
		LoadFunc(FuncPar);
		// update number of slides
		ResetFrames();
		const std::string loc = setlocale(LC_NUMERIC, "C");
		// use frames for quickly redrawing while adding/changing primitives
		if(mgl_is_frames(this))	NewFrame();

		int n = DrawFunc ? DrawFunc(this,FuncPar) : 0;
		if(n<NumFig && n>=0)	NumFig = n;

		if(mgl_is_frames(this))	EndFrame();
		setlocale(LC_NUMERIC, loc.c_str());
		Update();
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_prop_func(char id, const char *val, void *p)
{	mglCanvasWnd *g = (mglCanvasWnd *)(p);	if(g)	g->SetParam(id, val);	}
void MGL_EXPORT mgl_wnd_make_dialog(HMGL gr, const char *ids, char const * const *args, const char *title)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->MakeDialog(ids, args, title);	}
void MGL_EXPORT mgl_wnd_set_func(HMGL gr, int (*draw)(HMGL gr, void *p), void *par, void (*reload)(void *p))
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->SetDrawFunc(draw, par, reload);	}
void MGL_EXPORT mgl_wnd_set_prop(HMGL gr, void (*prop)(char id, const char *val, void *p), void *par)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->SetPropFunc(prop, par);	}
void MGL_EXPORT mgl_wnd_toggle_alpha(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ToggleAlpha();	}
void MGL_EXPORT mgl_wnd_toggle_light(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ToggleLight();	}
void MGL_EXPORT mgl_wnd_toggle_zoom(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ToggleZoom();	}
void MGL_EXPORT mgl_wnd_toggle_rotate(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ToggleRotate();	}
void MGL_EXPORT mgl_wnd_toggle_no(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ToggleNo();	}
void MGL_EXPORT mgl_wnd_update(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->Update();	}
void MGL_EXPORT mgl_wnd_reload(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ReLoad();	}
void MGL_EXPORT mgl_wnd_adjust(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->Adjust();	}
void MGL_EXPORT mgl_wnd_next_frame(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->NextFrame();	}
void MGL_EXPORT mgl_wnd_prev_frame(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->PrevFrame();	}
void MGL_EXPORT mgl_wnd_animation(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->Animation();	}
void MGL_EXPORT mgl_setup_window(HMGL gr, int clf_upd, int showpos)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->Setup(clf_upd, showpos);	}
void MGL_EXPORT mgl_set_click_func(HMGL gr, void (*func)(void *p))
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->ClickFunc = func;	}
void MGL_EXPORT mgl_get_last_mouse_pos(HMGL gr, mreal *x, mreal *y, mreal *z)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	mglPoint p;	if(g)	p=g->GetMousePos();
	*x=p.x;	*y=p.y;	*z=p.z;	}
MGL_EXPORT void *mgl_wnd_window(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	return g?g->Window():NULL;	}
MGL_EXPORT void *mgl_wnd_widget(HMGL gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	return g?g->Widget():NULL;	}
/// Move window to given position
void MGL_EXPORT mgl_wnd_move(HMGL gr, int x, int y)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->WndMove(x,y);	}
void MGL_EXPORT mgl_wnd_move_(uintptr_t *gr, int *x, int *y)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));	if(g)	g->WndMove(*x,*y);	}
/// Change window sizes
void MGL_EXPORT mgl_wnd_size(HMGL gr, int w, int h)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);	if(g)	g->WndSize(w,h);	}
void MGL_EXPORT mgl_wnd_size_(uintptr_t *gr, int *w, int *h)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));	if(g)	g->WndSize(*w,*h);	}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_wnd_toggle_alpha_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ToggleAlpha();	}
void MGL_EXPORT mgl_wnd_toggle_light_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ToggleLight();	}
void MGL_EXPORT mgl_wnd_toggle_zoom_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ToggleZoom();	}
void MGL_EXPORT mgl_wnd_toggle_rotate_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ToggleRotate();	}
void MGL_EXPORT mgl_wnd_toggle_no_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ToggleNo();	}
void MGL_EXPORT mgl_wnd_update_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->Update();	}
void MGL_EXPORT mgl_wnd_reload_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->ReLoad();	}
void MGL_EXPORT mgl_wnd_adjust_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->Adjust();	}
void MGL_EXPORT mgl_wnd_next_frame_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->NextFrame();	}
void MGL_EXPORT mgl_wnd_prev_frame_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->PrevFrame();	}
void MGL_EXPORT mgl_wnd_animation_(uintptr_t *gr)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->Animation();	}
void MGL_EXPORT mgl_setup_window_(uintptr_t *gr, int *clf_upd, int *showpos)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	if(g)	g->Setup(*clf_upd, *showpos);	}
void MGL_EXPORT mgl_get_last_mouse_pos_(uintptr_t *gr, mreal *x, mreal *y, mreal *z)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>((HMGL)(*gr));
	mglPoint p;	if(g)	p=g->GetMousePos();
	*x=p.x;	*y=p.y;	*z=p.z;	}
//-----------------------------------------------------------------------------
#if MGL_HAVE_PTHR_WIDGET
void MGL_EXPORT mgl_wnd_set_mutex(HMGL gr, pthread_mutex_t *mutex)
{	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	if(g)	g->mutex = mutex;	}
#endif
//-----------------------------------------------------------------------------
//
//	mglDraw class handling
//
//-----------------------------------------------------------------------------
/*int mgl_draw_class(HMGL gr, void *p)	// so stupid way to save mglDraw class inheritance :(
{
	mglGraph g(gr);	mglWindow *w = (mglWindow *)p;
	return (w && w->dr) ? w->dr->Draw(&g) : 0;
}
void MGL_EXPORT mgl_reload_class(void *p)	// so stupid way to save mglDraw class inheritance :(
{	mglWindow *w = (mglWindow *)p;	if(w && w->dr)	w->dr->Reload();}
void MGL_EXPORT mgl_click_class(void *p)	// so stupid way to save mglDraw class inheritance :(
{	mglWindow *w = (mglWindow *)p;	if(w && w->dr)	w->dr->Click();	}*/
int MGL_EXPORT mgl_draw_class(HMGL gr, void *p)
{	mglGraph g(gr);	mglDraw *dr = (mglDraw *)p;	return dr?dr->Draw(&g):0;	}
void MGL_EXPORT mgl_reload_class(void *p)
{	mglDraw *dr = (mglDraw *)p;	if(dr)	dr->Reload();	}
void MGL_EXPORT mgl_click_class(void *p)
{	mglDraw *dr = (mglDraw *)p;	if(dr)	dr->Click();		}
void MGL_EXPORT mgl_prop_class(char id, const char *val, void *p)
{	mglDraw *dr = (mglDraw *)p;	if(dr)	dr->Param(id,val);	}
//-----------------------------------------------------------------------------
typedef int (*draw_func)(mglGraph *gr);
int MGL_EXPORT mgl_draw_graph(HMGL gr, void *p)
{
	mglGraph g(gr);
	draw_func func = (draw_func)(p);
	return func ? func(&g) : 0;
}
//-----------------------------------------------------------------------------
MGL_EXPORT void *mgl_draw_calc(void *p)
{
#if MGL_HAVE_PTHR_WIDGET
	mglDraw *d = (mglDraw *)p;
	d->Calc();	d->running = false;
#endif
	return 0;
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_parse_comments(const wchar_t *text, double &a1, double &a2, double &da, std::vector<std::wstring> &anim, std::string &ids, std::vector<std::wstring> &par)
{
	a1=0;	a2=0;	da=1;
	const wchar_t *str = wcsstr(text, L"##c");
	if(str)	// this is animation loop
	{
		int res=wscanf(str+3, "%lg%lg%lg", &a1, &a2, &da);
		da = res<3?1:da;
		if(res>2 && da*(a2-a1)>0)
		{
			for(double a=a1;da*(a2-a)>=0;a+=da)
			{
				wchar_t buf[128];	swprintf(buf,128,L"%g",a);
				anim.push_back(buf);
			}
			return;
		}
	}
	str = wcsstr(text, L"##a");
	while(str)
	{
		str += 3;
		while(*str>0 && *str<=' ' && *str!='\n')	str++;
		if(*str>' ')
		{
			size_t j=0;	while(str[j]>' ')	j++;
			std::wstring val(str,j);
			anim.push_back(val);
		}
		str = wcsstr(str, L"##a");
	}
	str = wcsstr(text, L"##d");	// custom dialog
	while(str)
	{
		str = wcschr(str,'$');
		if(str)
		{
			char id = str[1];	str += 2;
			while(*str>0 && *str<=' ' && *str!='\n')	str++;
			if(*str>' ')
			{
				long j=0;	while(str[j]!='\n')	j++;
				while(str[j-1]<=' ')	j--;
				
				ids.push_back(id);
				std::wstring val(str,j);
				par.push_back(val);
			}
		}
		str = wcsstr(str, L"##d");
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_parse_comments(const char *text, double &a1, double &a2, double &da, std::vector<std::string> &anim, std::string &ids, std::vector<std::string> &par)
{
	a1=0;	a2=0;	da=1;
	const char *str = strstr(text, "##c");
	if(str)	// this is animation loop
	{
		int res=sscanf(str+3, "%lg%lg%lg", &a1, &a2, &da);
		da = res<3?1:da;
		if(res>2 && da*(a2-a1)>0)
		{
			for(double a=a1;da*(a2-a)>=0;a+=da)
			{
				char buf[128];	snprintf(buf,128,"%g",a);
				anim.push_back(buf);
			}
			return;
		}
	}
	str = strstr(text, "##a");
	while(str)
	{
		str += 3;
		while(*str>0 && *str<=' ' && *str!='\n')	str++;
		if(*str>' ')
		{
			size_t j=0;	while(str[j]>' ')	j++;
			std::string val(str,j);
			anim.push_back(val);
		}
		str = strstr(str, "##a");
	}
	str = strstr(text, "##d");	// custom dialog
	while(str)
	{
		str = strchr(str,'$');
		if(str)
		{
			char id = str[1];	str += 2;
			while(*str>0 && *str<=' ' && *str!='\n')	str++;
			if(*str>' ')
			{
				long j=0;	while(str[j]!='\n')	j++;
				while(str[j-1]<=' ')	j--;
				
				ids.push_back(id);
				std::string val(str,j);
				par.push_back(val);
			}
		}
		str = strstr(str, "##d");
	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_parse_animation(const char *text, std::vector<std::string> &anim)
{
	std::string ids;
	std::vector<std::string> par;
	double a1, a2, da;
	mgl_parse_comments(text, a1, a2, da, anim, ids, par);
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_parse_animation(const wchar_t *text, std::vector<std::wstring> &anim)
{
	std::string ids;
	std::vector<std::wstring> par;
	double a1, a2, da;
	mgl_parse_comments(text, a1, a2, da, anim, ids, par);
}
//-----------------------------------------------------------------------------
#undef _GR_
#define _GR_	((mglCanvas *)(*gr))
void MGL_EXPORT mgl_wnd_set_delay(HMGL gr, double dt)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	if(g)	g->SetDelay(dt);	}
double MGL_EXPORT_PURE mgl_wnd_get_delay(HMGL gr)
{	mglCanvas *g = dynamic_cast<mglCanvas *>(gr);	return g?g->GetDelay():0;	}
void MGL_EXPORT mgl_wnd_set_delay_(uintptr_t *gr, mreal *dt)	{	_GR_->SetDelay(*dt);	}
double MGL_EXPORT_PURE mgl_wnd_get_delay_(uintptr_t *gr)	{	return _GR_->GetDelay();	}
//-----------------------------------------------------------------------------
MGL_EXPORT const char *mgl_hints[] = {
	_("You can shift axis range by pressing middle button and moving mouse. Also, you can zoom in/out axis range by using mouse wheel."),
	_("You can rotate/shift/zoom whole plot by mouse. Just press 'Rotate' toolbutton, click image and hold a mouse button: left button for rotation, right button for zoom/perspective, middle button for shift."),
	_("You may quickly draw the data from file. Just use: mgllab 'filename.dat' in command line."),
	_("You can copy the current image to clipboard by pressing Ctrl-Shift-C. Later you can paste it directly into yours document or presentation."),
	_("You can export image into a set of format (EPS, SVG, PNG, JPEG) by pressing right mouse button inside image and selecting 'Export as ...'."),
	_("You can setup colors for script highlighting in Property dialog. Just select menu item 'Settings/Properties'."),
	_("You can save the parameter of animation inside MGL script by using comment started from '##a ' or '##c ' for loops."),
	_("New drawing never clears things drawn already. For example, you can make a surface with contour lines by calling commands 'surf' and 'cont' one after another (in any order). "),
	_("You can put several plots in the same image by help of commands 'subplot' or 'inplot'."),
	_("All indexes (of data arrays, subplots and so on) are always start from 0."),
	_("You can edit MGL file in any text editor. Also you can run it in console by help of commands: mglconv, mglview."),
	_("You can use command 'once on|off' for marking the block which should be executed only once. For example, this can be the block of large data reading/creating/handling. Press F9 (or menu item 'Graphics/Reload') to re-execute this block."),
	_("You can use command 'stop' for terminating script parsing. It is useful if you don't want to execute a part of script."),
	_("You can type arbitrary expression as input argument for data or number. In last case (for numbers), the first value of data array is used."),
	_("There is powerful calculator with a lot of special functions. You can use buttons or keyboard to type the expression. Also you can use existed variables in the expression."),
	_("The calculator can help you to put complex expression in the script. Just type the expression (which may depend on coordinates x,y,z and so on) and put it into the script."),
	_("You can easily insert file or folder names, last fitted formula or numerical value of selection by using menu Edit|Insert."),
	_("The special dialog (Edit|Insert|New Command) help you select the command, fill its arguments and put it into the script."),
	_("You can put several plotting commands in the same line or in separate function, for highlighting all of them simultaneously."),
	_("You can concatenation of strings and numbers using `,` with out spaces (for example, `'max(u)=',u.max,' a.u.'` or `'u=',!(1+i2)` for complex numbers). Also you can get n-th symbol of the string using `[]` (for example, `'abc'[1]` will give 'b'), or add a value to the last character of the string using `+` (for example, `'abc'+3` will give 'abf'), or use it all together."),
	NULL
};
//-----------------------------------------------------------------------------
