/***************************************************************************
 * fltk.cpp is part of Math Graphic Library
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
#include <FL/Fl_Pixmap.H>
#include <FL/fl_ask.H>
#include <FL/Fl_Double_Window.H>
#include <FL/fl_draw.H>
#include <FL/Fl_Native_File_Chooser.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Choice.H>
#include <FL/Fl_Check_Button.H>
//-----------------------------------------------------------------------------
#include "mgl2/canvas_wnd.h"
#include "mgl2/Fl_MathGL.h"
#include <limits.h>
//-----------------------------------------------------------------------------
#define MGL_MAX_LINES	(INT_MAX-1000)
//-----------------------------------------------------------------------------
/// Class allows the window creation for displaying plot bitmap with the help of FLTK library
/** NOTE!!! All frames are saved in memory. So animation with many frames require a lot memory and CPU time (for example, for mouse rotation).*/
class mglCanvasFL : public mglCanvasWnd
{
public:
using mglCanvasWnd::Window;
	Fl_Window *Wnd;		///< Pointer to window
	Fl_MGLView *mgl;	///< Pointer to MGL widget with buttons

	mglCanvasFL();
	virtual ~mglCanvasFL();

	/// Create a window for plotting. Now implemeted only for GLUT.
	void Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p), const char *title,
						void *par=NULL, void (*reload)(void *p)=NULL, bool maximize=false);
	/// Switch on/off transparency (do not overwrite switches in user drawing function)
	void ToggleAlpha();
	/// Switch on/off lighting (do not overwrite switches in user drawing function)
	void ToggleLight();
	void ToggleRotate();	///< Switch on/off rotation by mouse
	void ToggleZoom();		///< Switch on/off zooming by mouse
	void ToggleNo();		///< Switch off all zooming and rotation
	void Update();			///< Update picture by calling user drawing function
	void Adjust();			///< Adjust size of bitmap to window size
	void GotoFrame(int d);	///< Show arbitrary frame (use relative step)
	void Animation();	///< Run animation (I'm too lasy to change it)
	void MakeDialog(const char *ids, char const * const *args, const char *title="")
	{	if(GetNumFig()==1)	mgl->dialog(ids,args,title);	}
	void *Window()	{return Wnd;}	///< Return pointer to widget (Fl_Window*) used for plotting
	void *Widget()	{return mgl;}	///< Return pointer to widget (Fl_MGLView*) used for plotting
	void WndSize(int w, int h)	{	Wnd->size(w,h);	}	///< Resize window
	void WndMove(int x, int y)	{	Wnd->position(x,y);	}	///< Move window
};
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ask_fltk(const wchar_t *quest, wchar_t *res)
{
	static char buf[1024];	*res=0;	// TODO
#if FL_MINOR_VERSION>=3
	fl_utf8fromwc(buf, 1024, quest, mgl_wcslen(quest)+1);
	const char *str = fl_input("%s",buf,"");
	if(str)	fl_utf8towc(str, strlen(str)+1, res, 1024);
#else
	wcstombs(buf,quest,mgl_wcslen(quest)+1);
	const char *str = fl_input("%s",buf,"");
	MGL_TO_WCS(str,wcscpy(res,str));
#endif
}
//-----------------------------------------------------------------------------
//
//		class Fl_MathGL
//
//-----------------------------------------------------------------------------
MGL_EXPORT const char *mgl_file_chooser(const char *mess, const char *filter, bool save)
{
	static Fl_Native_File_Chooser fnfc;
	fnfc.title(mess);
	fnfc.type(save ? Fl_Native_File_Chooser::BROWSE_SAVE_FILE : Fl_Native_File_Chooser::BROWSE_FILE);
	fnfc.filter(filter);
//	fnfc.directory("/var/tmp");           // default directory to use
	fnfc.show();
	return fnfc.filename();
}
MGL_EXPORT const char *mgl_dir_chooser(const char *mess, const char *path)
{
	static Fl_Native_File_Chooser fnfc;
	fnfc.title(mess);
	fnfc.type(Fl_Native_File_Chooser::BROWSE_DIRECTORY);
	fnfc.directory(path);           // default directory to use
	fnfc.show();
	return fnfc.filename();
}
//-----------------------------------------------------------------------------
Fl_MathGL::Fl_MathGL(int xx, int yy, int ww, int hh, const char *lbl) : Fl_Widget(xx,yy,ww,hh,lbl)
{
	gr = new mglCanvas;	use_pthr = true;
	tet=phi=x1=y1=0;	x2=y2=1;	img = 0;
	zoom = rotate = handle_keys = grid = false;
	flag=x0=y0=xe=ye=0;	show_warn=true;
	tet_val = phi_val = 0;
	draw_par = 0;	draw_func = 0;	draw_cl = 0;
	last_id = -1;	run = false;
	popup=0;	vpar=0;	wpar=0;
#if (MGL_HAVE_PTHREAD|MGL_HAVE_PTHR_WIDGET)
	thr=0;
#endif
}
//-----------------------------------------------------------------------------
Fl_MathGL::~Fl_MathGL()	{	if(mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);	}
//-----------------------------------------------------------------------------
void Fl_MathGL::stop(bool stop)	{	gr->AskStop(stop);	}
//-----------------------------------------------------------------------------
void mgl_fltk_event_func(void *)	{	Fl::awake();	}
//-----------------------------------------------------------------------------
void Fl_MathGL::set_graph(HMGL GR)
{
	mglCanvas *gg = dynamic_cast<mglCanvas *>(GR);
	if(!gg)	return;
	if(gr && mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);
	gr=gg;	mgl_use_graph(gg,1);
	gr->SetEventFunc(mgl_fltk_event_func, NULL);
}
//-----------------------------------------------------------------------------
void Fl_MathGL::refresh()	{	img = mgl_get_rgb(gr);	redraw();	}
//-----------------------------------------------------------------------------
void Fl_MathGL::draw()
{
	if(!img)	img = mgl_get_rgb(gr);
	int ww=mgl_get_width(gr), hh=mgl_get_height(gr);
	if(img)	fl_draw_image(img, x(), y(), ww, hh, 3);
	if(grid)
	{
		char str[5]="0.0";
		fl_color(192,192,192);
		for(int i=1;i<10;i++)
		{
			str[2] = '0'+10-i;	fl_draw(str, x(), y()+i*hh/10);
			fl_line(x(), y()+i*hh/10, x()+ww, y()+i*hh/10);
			str[2] = '0'+i;	fl_draw(str, x()+i*ww/10, y()+hh);
			fl_line(x()+i*ww/10, y(), x()+i*ww/10, y()+hh);
		}
		int d = (hh>ww?ww:hh)/100;
		for(size_t i=0;i<gr->Act.size();i++)
		{
			const mglActivePos &p=gr->Act[i];
			fl_rectf(x()+p.x-d/2, y()+p.y-d/2-1, d,d, fl_rgb_color(127,255,63));
			fl_rect(x()+p.x-d/2, y()+p.y-d/2-1, d,d, FL_BLACK);
		}
		fl_color(FL_BLACK);		fl_draw(mouse_pos,40,70);
		mgl_set_flag(gr,1,MGL_SHOW_POS);
	}
	else	mgl_set_flag(gr,0,MGL_SHOW_POS);
}
//-----------------------------------------------------------------------------
inline void Fl_MathGL::draw_plot()	// drawing itself
{
	if(draw_func || draw_cl)
	{
		mgl_set_def_param(gr);		mgl_reset_frames(gr);
		if(mgl_get_flag(gr,MGL_CLF_ON_UPD))	mgl_set_def_param(gr);
		mgl_set_alpha(gr,flag&1);	mgl_set_light(gr,flag&2);
		if(tet_val)	tet = tet_val->value();
		if(phi_val)	phi = phi_val->value();
		mgl_zoom(gr,x1,y1,x2,y2);	mgl_view(gr,-phi,-tet,0);
		const std::string loc = setlocale(LC_NUMERIC, "C");
		// use frames for quickly redrawing while adding/changing primitives
		if(mgl_is_frames(gr))	mgl_new_frame(gr);

		if(draw_func)	draw_func(gr, draw_par);
		else	if(draw_cl)	{	mglGraph g(gr);	draw_cl->Draw(&g);	}

		if(mgl_is_frames(gr))	mgl_end_frame(gr);
		setlocale(LC_NUMERIC, loc.c_str());
		const char *buf = mgl_get_mess(gr);
		if(show_warn && *buf)	fl_message("%s",buf);
		if(!prim.empty())	// manual primitives
		{
			const std::string loc = setlocale(LC_NUMERIC, "C");
			mgl_subplot(gr,1,1,0,"#");
			mgl_set_ranges(gr, -1,1, -1,1, -1,1);
			mglParse pr;	pr.StartID(MGL_MAX_LINES);
			mgl_parse_text(gr,pr.Self(),prim.c_str());
			setlocale(LC_NUMERIC, loc.c_str());
		}
	}
	else if(mgl_get_num_frame(gr)>0)
	{
		mgl_set_alpha(gr,flag&1);	mgl_set_light(gr,flag&2);
		if(tet_val)	tet = tet_val->value();
		if(phi_val)	phi = phi_val->value();
		mgl_zoom(gr,x1,y1,x2,y2);	mgl_view(gr,-phi,-tet,0);
		mgl_get_frame(gr,0);
	}
	mgl_finish(gr);	img = mgl_get_rgb(gr);
	run = false;	Fl::awake();
}
//-----------------------------------------------------------------------------
static void *draw_plot_thr(void *v)
{	((Fl_MathGL*)v)->draw_plot();	return 0;	}
//-----------------------------------------------------------------------------
void Fl_MathGL::update()
{
	if(run)	return;
//	Fl::lock();
	run = true;
#if MGL_HAVE_FL_COPY
	top_window()->cursor(FL_CURSOR_WAIT);
#endif
#if MGL_HAVE_PTHR_WIDGET
	if(use_pthr)
	{
		pthread_create(&thr,0,draw_plot_thr,this);
		while(run)	Fl::wait();
		pthread_join(thr,0);
	}
	else
#endif
		draw_plot();
//	Fl::unlock();

	if(mgl_get_width(gr)!=w() || mgl_get_height(gr)!=h())
		size(mgl_get_width(gr), mgl_get_height(gr));
	gr->AskStop(false);	redraw();
#if MGL_HAVE_FL_COPY
	top_window()->cursor(FL_CURSOR_DEFAULT);
#endif
	Fl::flush();
}
//-----------------------------------------------------------------------------
void Fl_MathGL::resize(int xx, int yy, int ww, int hh)
{	Fl_Widget::resize(xx,yy,ww,hh);	}
//-----------------------------------------------------------------------------
int Fl_MathGL::handle(int code)
{
	static bool busy=false;
	static int last_pos=-1;
	if(handle_keys && code==FL_KEYUP && Fl::event_button()!=FL_LEFT_MOUSE)
	{
		int key=Fl::event_key();
		if(!strchr(" .,wasdrfx",key))	return 0;
		if(key==' ')	{	update();	return 1;	}
		if(key=='w')
		{
			tet += 10;
			if(tet_val)	tet_val->value(tet);
			update();	return 1;
		}
		if(key=='s')
		{
			tet -= 10;
			if(tet_val)	tet_val->value(tet);
			update();	return 1;
		}
		if(key=='a')
		{
			phi += 10;
			if(phi_val)	phi_val->value(phi);
			update();	return 1;
		}
		if(key=='d')
		{
			phi -= 10;
			if(phi_val)	phi_val->value(phi);
			update();	return 1;
		}
		if(key=='x')
		{
			mglCanvasFL *g=dynamic_cast<mglCanvasFL *>(gr);
			if(g && g->mgl->FMGL==this)
			{	g->Wnd->hide();	return 1;	}
			else	return 0;
			//				exit(0);
		}
		if(key==',')
		{
			mglCanvasFL *g=dynamic_cast<mglCanvasFL *>(gr);
			if(g && g->mgl->FMGL==this)
			{	g->PrevFrame();	return 1;	}
			else	return 0;
		}
		if(key=='.')
		{
			mglCanvasFL *g=dynamic_cast<mglCanvasFL *>(gr);
			if(g && g->mgl->FMGL==this)
			{	g->NextFrame();	return 1;	}
			else	return 0;
		}
		if(key=='r')
		{	flag = (flag&2) + ((~(flag&1))&1);	update();	return 1;	}
		if(key=='f')
		{	flag = (flag&1) + ((~(flag&2))&2);	update();	return 1;	}
	}
	else if(code==FL_PUSH)
	{
		last_pos=-1;	xe=x0=Fl::event_x();	ye=y0=Fl::event_y();
		if(popup && Fl::event_button()==FL_RIGHT_MOUSE)
		{
			const Fl_Menu_Item *m = popup->popup(Fl::event_x(), Fl::event_y(), 0, 0, 0);
			if(m)	m->do_callback(wpar, vpar);
		}
		else if(!zoom && !rotate && Fl::event_button()==FL_LEFT_MOUSE)
		{
			int xx = x0-x(), yy = y0-y();
			last_id = mgl_get_obj_id(gr,xx, yy);
			if(last_id>=MGL_MAX_LINES)	last_id=-1;
			mglCanvasWnd *g=dynamic_cast<mglCanvasWnd *>(gr);
			if(g && g->ClickFunc)	g->ClickFunc(draw_par);
			if(mgl_get_flag(gr,MGL_SHOW_POS))
			{
				mglPoint p = gr->CalcXYZ(xx, yy);
				if(g)	g->LastMousePos = p;
				snprintf(mouse_pos,128,"x=%g, y=%g, z=%g",p.x,p.y,p.z);
				mouse_pos[127]=0;	draw();
			}
			if(Fl::event_clicks())
			{
				int id = mgl_get_obj_id(gr,x0-x(), y0-y()) - MGL_MAX_LINES-1;
				if(grid && id>=0)	// delete manual primitive
				{
					prim = (id>0?mgl_str_arg(prim, '\n', 0,id-1)+'\n':"") + mgl_str_arg(prim, '\n', id+1,INT_MAX);
					update();
				}
				else if(draw_cl)	draw_cl->Click();
				else	update();
			}
		}
		return 1;
	}
	else if(code==FL_DRAG)
	{
		if(busy)	return 1;	// remove possible conflicts of too often events
		busy = true;	xe=Fl::event_x();	ye=Fl::event_y();
		mreal ff = 240./sqrt(mreal(w()*h()));
		// handle primitives
		int id = mgl_get_obj_id(gr,x0-x(), y0-y()) - MGL_MAX_LINES-1;
		int ww=mgl_get_width(gr), hh=mgl_get_height(gr), d=(hh>ww?ww:hh)/100;
		long pos = mgl_is_active(gr,x0-x(),y0-y(),d);
		if(grid && pos<0)	pos=last_pos;
		if(grid && pos>=0)
		{
//			Fl::lock();
			last_pos=pos;
			const mglActivePos &p = gr->Act[pos];
			id = long(p.id)-MGL_MAX_LINES-1;
			if(id<0)	return 0;
			std::string line = mgl_str_arg(prim, '\n', id), res;
			if(line.empty())	return 0;	// NOTE stupid check (never should be here)
			std::vector<std::string> arg = mgl_str_args(line,' ');
			// try "attract" mouse
			for(size_t i=0;i<=10;i++)
			{
				int tt = i*(w()/10);	if(abs(xe-tt)<2*d)	xe = tt;
				tt = i*(h()/10);	if(abs(ye-tt)<2*d)	ye = tt;
			}
			for(size_t i=0;i<gr->Act.size();i++)
			{
				const mglActivePos &q = gr->Act[i];
				if(abs(xe-q.x)<2*d && abs(ye-q.y)<2*d)	{	xe=q.x;	ye=q.y;	}
			}
			// now move point
			float dx = 2*(xe-x0)/float(w()), dy = 2*(y0-ye)/float(h());
			float xx=atof(arg[1].c_str()), yy=atof(arg[2].c_str());
			if(p.n==0)	{	arg[1]=mgl_str_num(xx+dx);	arg[2]=mgl_str_num(yy+dy);	}
			else if(arg[0]=="rect")
			{
				float x_=atof(arg[3].c_str()), y_=atof(arg[4].c_str());
				if(p.n==1)	{	xx+=dx;	y_+=dy;	}
				if(p.n==2)	{	x_+=dx;	yy+=dy;	}
				if(p.n==3)	{	x_+=dx;	y_+=dy;	}
				arg[1]=mgl_str_num(xx);	arg[2]=mgl_str_num(yy);
				arg[3]=mgl_str_num(x_);	arg[4]=mgl_str_num(y_);
			}
			else if(p.n==1)
			{
				float xx=atof(arg[3].c_str()), yy=atof(arg[4].c_str());
				arg[3]=mgl_str_num(xx+dx);	arg[4]=mgl_str_num(yy+dy);
			}
			else if(arg[0]=="rhomb" || arg[0]=="ellipse")
			{
				float x_=atof(arg[3].c_str())-xx, y_=atof(arg[4].c_str())-yy;
				float r_=atof(arg[5].c_str()), dr=0;
				if(x_*x_+y_*y_>0)
				{	dr = (dx*x_+dy*y_)/(x_*x_+y_*y_);	dr = hypot(dx-dr*x_,dy-dr*y_);	}
				else	dr = hypot(dx,dy);
				arg[5]=mgl_str_num(r_+dr);
			}
			else if(arg[0]=="arc")
			{
				float x_=atof(arg[3].c_str())-xx, y_=atof(arg[4].c_str())-yy;
				float a_=atof(arg[5].c_str());
				double c=cos(M_PI*a_/180), s=sin(M_PI*a_/180);
				double a = atan2(x_,y_) - atan2(x_*c-y_*s+dx,x_*s+y_*c+dy);
				arg[5]=mgl_str_num(a*180/M_PI);
			}
			else if(p.n==2)
			{
				float xx=atof(arg[5].c_str()), yy=atof(arg[6].c_str());
				arg[5]=mgl_str_num(xx+dx);	arg[6]=mgl_str_num(yy+dy);
			}
			else if(p.n==3)
			{
				float xx=atof(arg[7].c_str()), yy=atof(arg[8].c_str());
				if(arg[0]=="curve")	{	dx*=-1;	dy*=-1;	}
				arg[7]=mgl_str_num(xx+dx);	arg[8]=mgl_str_num(yy+dy);
			}
			res = arg[0];	for(size_t i=1;i<arg.size();i++)	res += ' '+arg[i];
			prim = (id>0?mgl_str_arg(prim, '\n', 0,id-1)+'\n':"") + res+'\n' + mgl_str_arg(prim, '\n', id+1,INT_MAX);
//			Fl::unlock();
			x0 = xe;	y0 = ye;	update();
		}
		else if(grid && id>=0)
		{
//			Fl::lock();
			std::string line = mgl_str_arg(prim, '\n', id), res;
			if(line.empty())	return 0;	// NOTE stupid check (never should be here)
			std::vector<std::string> arg = mgl_str_args(line,' ');

			float dx = 2*(xe-x0)/float(w()), dy = 2*(y0-ye)/float(h());
			float x1=atof(arg[1].c_str()), y1=atof(arg[2].c_str());
			arg[1] = mgl_str_num(x1+dx);	arg[2] = mgl_str_num(y1+dy);
			if(arg[0]=="curve")
			{
				float x2=atof(arg[5].c_str()), y2=atof(arg[6].c_str());
				arg[5]=mgl_str_num(x2+dx);	arg[6]=mgl_str_num(y2+dy);
			}
			else if(arg[0]!="ball")
			{
				float x2=atof(mgl_str_arg(line,' ',3).c_str()), y2=atof(mgl_str_arg(line,' ',4).c_str());
				arg[3]=mgl_str_num(x2+dx);	arg[4]=mgl_str_num(y2+dy);
			}
			res = arg[0];	for(size_t i=1;i<arg.size();i++)	res += ' '+arg[i];
			prim = (id>0?mgl_str_arg(prim, '\n', 0,id-1)+'\n':"") + res+'\n' + mgl_str_arg(prim, '\n', id+1,INT_MAX);
//			Fl::unlock();
			x0 = xe;	y0 = ye;	update();
		}
		else if(rotate)
		{
			phi += (x0-xe)*ff;
			tet += (y0-ye)*ff;
			if(phi>180)		phi-=360;
			if(phi<-180)	phi+=360;
			if(tet>180)		tet-=360;
			if(tet<-180)	tet+=360;
			if(tet_val)	tet_val->value(tet);
			if(phi_val)	phi_val->value(phi);
			x0 = xe;	y0 = ye;	update();
		}
		busy = false;	redraw();	return 1;
	}
	else if(code==FL_RELEASE)
	{
		last_pos=-1;
		if(zoom)
		{
			int w1=w(),h1=h();
			mreal _x1,_x2,_y1,_y2;
			_x1 = x1+(x2-x1)*(x0-x())/mreal(w1);
			_y1 = y2-(y2-y1)*(ye-y())/mreal(h1);
			_x2 = x1+(x2-x1)*(xe-x())/mreal(w1);
			_y2 = y2-(y2-y1)*(y0-y())/mreal(h1);
			x1=_x1;		x2=_x2;		y1=_y1;		y2=_y2;
			if(x1>x2)	{	_x1=x1;	x1=x2;	x2=_x1;	}
			if(y1>y2)	{	_x1=y1;	y1=y2;	y2=_x1;	}
			update();
		}
		else if(rotate)
		{
			if(tet_val)	tet_val->value(tet);
			if(phi_val)	phi_val->value(phi);
		}
		redraw();	return 1;
	}
/*	else if(code==FL_MOVE && mgl_get_flag(gr,MGL_SHOW_POS))
	{
		mglPoint p = gr->CalcXYZ(x0-x(), y0-y());
		snprintf(mouse_pos,128,"x=%g, y=%g, z=%g",p.x,p.y,p.z);
		mouse_pos[127]=0;	draw();
		return 1;
	}*/
	return 0;
}
//-----------------------------------------------------------------------------
//
//		class Fl_MGLView
//
//-----------------------------------------------------------------------------
void Fl_MGLView::toggle(int &val, Fl_Button *b, const char *txt)
{
	val = 1-val;	b->value(val);
	if(menu && txt && *txt)
	{
		Fl_Menu_Item *m = (Fl_Menu_Item *)menu->find_item(_(txt));
		if(m && val)	m->set();
		if(m && !val)	m->clear();
	}
	update();
}
//-----------------------------------------------------------------------------
void Fl_MGLView::setoff(int &val, Fl_Button *b, const char *txt)
{
	val = 0;	b->value(val);
	if(menu && txt && *txt)
	{
		Fl_Menu_Item *m = (Fl_Menu_Item *)menu->find_item(_(txt));
		if(m && val)	m->set();
		if(m && !val)	m->clear();
	}
	//update();
}
//-----------------------------------------------------------------------------
void Fl_MGLView::exec_pause()
{
#if MGL_HAVE_PTHR_WIDGET
	pthread_mutex_t *mutex=0;
	mglCanvasWnd *g=dynamic_cast<mglCanvasWnd *>(FMGL->gr);
	if(g && g->mutex)	mutex = g->mutex;
	else
	{
		mglDraw *d=FMGL->get_class();
		if(d)	mutex = &(d->mutex);
	}
	if(mutex)
	{
		pthread_mutex_trylock(mutex);
		if(!pauseC)	pthread_mutex_unlock(mutex);
	}
#endif
}
//-----------------------------------------------------------------------------
void static mgl_pause_cb(Fl_Widget*, void* v)
{	if(v)	((Fl_MGLView*)v)->toggle_pause();	}
//-----------------------------------------------------------------------------
void static mgl_grid_cb(Fl_Widget*, void* v)
{	if(v)	((Fl_MGLView*)v)->toggle_grid();	}
//-----------------------------------------------------------------------------
void static mgl_alpha_cb(Fl_Widget*, void* v)
{	if(v)	((Fl_MGLView*)v)->toggle_alpha();	}
void mglCanvasFL::ToggleAlpha()	{	mgl->toggle_alpha();	}
//-----------------------------------------------------------------------------
void static mgl_light_cb(Fl_Widget*, void* v)
{	if(v)	((Fl_MGLView*)v)->toggle_light();	}
void mglCanvasFL::ToggleLight()	{	mgl->toggle_light();	}
//-----------------------------------------------------------------------------
void static mgl_norm_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	e->setoff_rotate();			e->setoff_zoom();
	e->FMGL->tet_val->value(0);	e->FMGL->phi_val->value(0);
	e->FMGL->set_zoom(0,0,1,1);
	e->update();
}
void mglCanvasFL::ToggleNo()	{	mgl_norm_cb(0,mgl);	}
//-----------------------------------------------------------------------------
void static mgl_zoom_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	e->setoff_rotate();	e->toggle_zoom();
}
void mglCanvasFL::ToggleZoom()	{	mgl_zoom_cb(0,mgl);	}
//-----------------------------------------------------------------------------
void static mgl_rotate_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	e->setoff_zoom();	e->toggle_rotate();
}
void mglCanvasFL::ToggleRotate()	{	mgl_rotate_cb(0,mgl);	}
//-----------------------------------------------------------------------------
void Fl_MGLView::update()
{
	FMGL->set_state(zoom_bt->value(), rotate_bt->value(), grid_bt->value());
	FMGL->set_flag(alpha + 2*light);
	FMGL->update();
}
void static mgl_draw_cb(Fl_Widget*, void* v)
{	if(v)	((Fl_MGLView*)v)->update();	}
void mglCanvasFL::Update()		{	mgl->update();	Wnd->show();	}
//-----------------------------------------------------------------------------
static const char *mgl_save_name(const char *ext)
{
	static std::string fname;
	fname = mgl_file_chooser(_("Save File As?"), ext, true);
	if(fname.empty())	return NULL;
	if(fname.find(ext+1)==std::string::npos)	fname += ext+1;
	return fname.c_str();
}
#define _FGR_	((Fl_MGLView*)v)->FMGL->get_graph()
//-----------------------------------------------------------------------------
void static mgl_export_png_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.png");
	if(fname)	mgl_write_png(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_bps_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.eps");
	if(fname)	mgl_write_bps(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_pngn_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.png");
	if(fname)	mgl_write_png_solid(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_jpeg_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.jpg");
	if(fname)	mgl_write_jpg(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_svg_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.svg");
	if(fname)	mgl_write_svg(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_eps_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.eps");
	if(fname)	mgl_write_eps(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_gif_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.gif");
	if(fname)	mgl_write_gif(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_bmp_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.bmp");
	if(fname)	mgl_write_bmp(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_prc_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.prc");
	if(fname)	mgl_write_prc(_FGR_,fname,0,1);
}
//-----------------------------------------------------------------------------
void static mgl_export_tex_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.tex");
	if(fname)	mgl_write_tex(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_obj_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.obj");
	if(fname)	mgl_write_obj(_FGR_,fname,0,1);
}
//-----------------------------------------------------------------------------
void static mgl_export_off_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.off");
	if(fname)	mgl_write_off(_FGR_,fname,0,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_stl_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.stl");
	if(fname)	mgl_write_stl(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_export_xyz_cb(Fl_Widget*, void* v)
{
	const char *fname = mgl_save_name("*.xyz");
	if(fname)	mgl_write_xyz(_FGR_,fname,0);
}
//-----------------------------------------------------------------------------
void static mgl_su_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (y2-y1)/3;	y1 -= d;	y2 -= d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_sd_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (y2-y1)/3;	y1 += d;	y2 += d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_sr_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (x2-x1)/3;	x1 -= d;	x2 -= d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_sl_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (x2-x1)/3;	x1 += d;	x2 += d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_sz_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (y2-y1)/4;	y1 += d;	y2 -= d;
	d = (x2-x1)/4;	x1 += d;	x2 -= d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_so_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;	if(!e)	return;
	double x1,x2,y1,y2,d;
	e->FMGL->get_zoom(&x1,&y1,&x2,&y2);
	d = (y2-y1)/2;	y1 -= d;	y2 += d;
	d = (x2-x1)/2;	x1 -= d;	x2 += d;
	e->FMGL->set_zoom(x1,y1,x2,y2);
}
//-----------------------------------------------------------------------------
void static mgl_dialog_cb(Fl_Widget*, void*v)
{	Fl_MGLView *e = (Fl_MGLView*)v;	if(e)	e->dlg_show();	}
//-----------------------------------------------------------------------------
void static mgl_adjust_cb(Fl_Widget*, void*v)
{	Fl_MGLView *e = (Fl_MGLView*)v;	if(e)	e->adjust();	}
void mglCanvasFL::Adjust()	{	mgl_adjust_cb(0,mgl);	}
//-----------------------------------------------------------------------------
void static mgl_oncemore_cb(Fl_Widget*, void*v)
{	Fl_MGLView *e = (Fl_MGLView*)v;	if(e && e->reload)	e->reload(e->par);	}
//-----------------------------------------------------------------------------
//void static mgl_quit_cb(Fl_Widget*, void*)	{	Fl::first_window()->hide();	}
//-----------------------------------------------------------------------------
void static mgl_snext_cb(Fl_Widget*, void* v)
{	Fl_MGLView *e = (Fl_MGLView*)v;	if(e && e->next)	e->next(e->par);	}
//-----------------------------------------------------------------------------
void static mgl_sprev_cb(Fl_Widget*, void* v)
{	Fl_MGLView *e = (Fl_MGLView*)v;	if(e && e->prev)	e->prev(e->par);	}
//-----------------------------------------------------------------------------
void static mgl_time_cb(void *v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;
	if(!e || !e->is_sshow() || !e->next || !e->delay)	return;
	e->next(e->par);
	Fl::repeat_timeout(e->delay(e->par), mgl_time_cb, v);
}
//-----------------------------------------------------------------------------
void static mgl_sshow_cb(Fl_Widget *, void *v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;
	if(!e || !e->delay || !e->next)	return;
	e->toggle_sshow();
	if(e->is_sshow())	Fl::add_timeout(e->delay(e->par), mgl_time_cb, v);
}
void mglCanvasFL::Animation()	{	mgl_sshow_cb(0,mgl);	}
void MGL_NO_EXPORT mgl_no_cb(Fl_Widget *, void *)	{}
//-----------------------------------------------------------------------------
void static mgl_stop_cb(Fl_Widget*, void* v)
{
	Fl_MGLView *e = (Fl_MGLView*)v;
	if(e)	e->FMGL->stop();
}
//-----------------------------------------------------------------------------
void static mgl_fl_next(void *v)	{	((mglCanvasWnd*)v)->NextFrame();	}	///< Callback function for next frame
void static mgl_fl_prev(void *v)	{	((mglCanvasWnd*)v)->PrevFrame();	}	///< Callback function for prev frame
void static mgl_fl_reload(void *v)	{	((mglCanvasWnd*)v)->ReLoad();	}		///< Callback function for reloading
mreal MGL_LOCAL_PURE mgl_fl_delay(void *v)	{	return ((mglCanvasWnd*)v)->GetDelay();	}	///< Callback function for delay
//-----------------------------------------------------------------------------
void copy_coor_cb(Fl_Widget *,void *v)
{
	HMGL gr = ((Fl_MGLView*)v)->get_graph();
	mreal x,y,z;	mgl_get_last_mouse_pos(gr,&x,&y,&z);
	static char buf[256];
	snprintf(buf,255,_("click at %g, %g, %g"),x,y,z);
	Fl::copy(buf,strlen(buf),1);
}
//-----------------------------------------------------------------------------
#if MGL_HAVE_FL_COPY
#include <FL/Fl_Copy_Surface.H>
void mgl_copyimg_cb(Fl_Widget *,void *v)
{
	Fl_MathGL *g = ((Fl_MGLView*)v)->FMGL;
	Fl_Copy_Surface *copy_surf = new Fl_Copy_Surface(g->w(), g->h());	// create an Fl_Copy_Surface object
	copy_surf->set_current();							// direct graphics requests to the clipboard
	fl_color(FL_WHITE);	fl_rectf(0, 0, g->w(), g->h());	// draw a white background
	copy_surf->draw(g);									// draw the g widget in the clipboard
	delete copy_surf;									// after this, the clipboard is loaded
	Fl_Display_Device::display_device()->set_current();	// direct graphics requests back to the display
}
#else
void mgl_copyimg_cb(Fl_Widget *,void *v)	{}
#endif
//-----------------------------------------------------------------------------
Fl_Menu_Item pop_graph[] = {
	{ _("Export as ..."), 0, mgl_no_cb, 0, FL_SUBMENU},
#if MGL_HAVE_PNG
		{ "PNG",	0, mgl_export_png_cb},
		{ "PNG (solid)",	0, mgl_export_pngn_cb},
#endif
#if MGL_HAVE_JPEG
		{ "JPEG",	0, mgl_export_jpeg_cb},
#endif
#if MGL_HAVE_GIF
		{ "GIF",	0, mgl_export_gif_cb},
#endif
		{ "BMP",	0, mgl_export_bmp_cb},
		{ "SVG",	0, mgl_export_svg_cb},
		{ "vector EPS",	0, mgl_export_eps_cb},
		{ "bitmap EPS",	0, mgl_export_bps_cb},
		{ "TeX",	0, mgl_export_tex_cb,0, FL_MENU_DIVIDER},
		{ "OBJ",	0, mgl_export_obj_cb},
		{ "PRC",	0, mgl_export_prc_cb},
		{ "OFF",	0, mgl_export_off_cb},
		{ "STL",	0, mgl_export_stl_cb},
		{ "XYZ",	0, mgl_export_xyz_cb},
		{0},
	{ _("Copy graphics"),	0, mgl_copyimg_cb, 0, FL_MENU_INACTIVE|FL_MENU_DIVIDER},
	{ _("Normal view"),	0, mgl_norm_cb},
	{ _("Redraw plot"),	0, mgl_draw_cb},
	{ _("Adjust size"),	0, mgl_adjust_cb},
	{ _("Reload data"),	0, mgl_oncemore_cb},
	{0}
};
//-----------------------------------------------------------------------------
#include "image.h"
#include "xpm/wire.xpm"
Fl_MGLView::Fl_MGLView(int xx, int yy, int ww, int hh, const char *lbl) : Fl_Window(xx,yy,ww,hh,lbl)
{
	Fl_Button *o;
	grid = alpha = light = sshow = pauseC = rotate = zoom = 0;
	menu = NULL;	next = prev = reload = NULL;	delay = NULL;

	Fl_Group *g = new Fl_Group(0,0,ww,30);
	alpha_bt = new Fl_Button(0, 1, 25, 25);	alpha_bt->type(FL_TOGGLE_BUTTON);
	alpha_bt->image(img_alpha);	alpha_bt->callback(mgl_alpha_cb,this);
	alpha_bt->tooltip(_("Switch on/off transparency in the picture"));
	light_bt = new Fl_Button(25, 1, 25, 25);	light_bt->type(FL_TOGGLE_BUTTON);
	light_bt->image(img_light);	light_bt->callback(mgl_light_cb,this);
	light_bt->tooltip(_("Switch on/off lightning in the picture"));
	grid_bt = new Fl_Button(50, 1, 25, 25);	grid_bt->type(FL_TOGGLE_BUTTON);
	grid_bt->image(new Fl_Pixmap(wire_xpm));	grid_bt->callback(mgl_grid_cb,this);
	grid_bt->tooltip(_("Switch on/off grid drawing"));

	rotate_bt = new Fl_Button(80, 1, 25, 25);rotate_bt->type(FL_TOGGLE_BUTTON);
	rotate_bt->image(img_move);	rotate_bt->callback(mgl_rotate_cb,this);
	rotate_bt->tooltip(_("Rotate picture by holding left mouse button"));
	zoom_bt = new Fl_Button(105, 1, 25, 25);	zoom_bt->type(FL_TOGGLE_BUTTON);
	zoom_bt->image(img_zoomIn);	zoom_bt->callback(mgl_zoom_cb,this);
	zoom_bt->tooltip(_("Zoom in selected region of the picture"));
	o = new Fl_Button(130, 1, 25, 25);		o->tooltip(_("Restore default graphics rotation, zoom and perspective"));
	o->image(img_orig);	o->callback(mgl_norm_cb,this);

	o = new Fl_Button(160, 1, 25, 25);	o->tooltip(_("Refresh the picture"));
	o->image(img_update);	o->callback(mgl_draw_cb,this);
	o = new Fl_Button(185, 1, 25, 25);	o->tooltip(_("Stop drawing"));
	o->image(img_stop);	o->callback(mgl_stop_cb,this);
	o = new Fl_Button(210, 1, 25, 25);	o->tooltip(_("Adjust picture size to fill drawing area"));
	o->image(img_adjust);	o->callback(mgl_adjust_cb,this);
	o = new Fl_Button(235, 1, 25, 25);	o->tooltip(_("Reload data and refresh the picture"));
	o->image(img_reload);	o->callback(mgl_oncemore_cb,this);
	o = new Fl_Button(265, 1, 25, 25);	o->tooltip(_("Copy image to clipboard"));
	o->image(img_copy);	o->callback(mgl_copyimg_cb,this);

	Fl_Counter *tet, *phi;
	tet = new Fl_Counter(295, 1, 90, 25, 0);	tet->callback(mgl_draw_cb,this);
	phi = new Fl_Counter(390, 1, 90, 25, 0);	phi->callback(mgl_draw_cb,this);
	tet->lstep(10);	tet->step(1);	tet->range(-180,180);
	tet->tooltip(_("Theta angle (tilt z-axis)"));
	phi->lstep(10);	phi->step(1);	phi->range(-180,180);
	phi->tooltip(_("Phi angle (rotate in x*y plane)"));
	progress = new Fl_Progress(485,0,ww-490,30);
	g->end();	g->resizable(progress);

	g = new Fl_Group(0,0,30,315);
	o = new Fl_Button(1, 30, 25, 25);		o->tooltip(_("Shift the picture up"));
	o->image(img_goU);		o->callback(mgl_su_cb,this);
	o = new Fl_Button(1, 55, 25, 25);		o->tooltip(_("Shift the picture left"));
	o->image(img_goL);	o->callback(mgl_sl_cb,this);
	o = new Fl_Button(1, 80, 25, 25);		o->tooltip(_("Zoom in the picture"));
	o->image(img_plus);	o->callback(mgl_sz_cb,this);
	o = new Fl_Button(1, 105, 25, 25);		o->tooltip(_("Zoom out the picture"));
	o->image(img_minus);	o->callback(mgl_so_cb,this);
	o = new Fl_Button(1, 130, 25, 25);		o->tooltip(_("Shift the picture right"));
	o->image(img_goR);	o->callback(mgl_sr_cb,this);
	o = new Fl_Button(1, 155, 25, 25);		o->tooltip(_("Shift the picture down"));
	o->image(img_goD);	o->callback(mgl_sd_cb,this);

	o = new Fl_Button(1, 185, 25, 25);		o->tooltip(_("Show previous frame in slideshow"));
	o->image(img_prev);	o->callback(mgl_sprev_cb,this);
	anim_bt = new Fl_Button(1, 210, 25, 25);	anim_bt->type(FL_TOGGLE_BUTTON);
	anim_bt->image(img_play);	anim_bt->callback(mgl_sshow_cb,this);
	anim_bt->tooltip(_("Run/Stop slideshow (graphics animation)"));
	o = new Fl_Button(1, 235, 25, 25);		o->tooltip(_("Show next frame in slideshow"));
	o->image(img_next);	o->callback(mgl_snext_cb,this);

	o = new Fl_Button(1, 265, 25, 25);		o->tooltip(_("Show custom dialog for plot setup"));
	o->image(img_form);	o->callback(mgl_dialog_cb,this);
#if MGL_HAVE_PTHR_WIDGET
	pause_bt = new Fl_Button(1, 290, 25, 25);	pause_bt->type(FL_TOGGLE_BUTTON);
	pause_bt->image(img_pause);	pause_bt->callback(mgl_pause_cb,this);
	pause_bt->tooltip(_("Pause on/off external calculations"));
#endif
	g->end();	g->resizable(0);

	scroll = new Fl_Scroll(30, 30, ww-30, hh-30);
	FMGL = new Fl_MathGL(30, 30, 800, 600);
	FMGL->tet_val = tet;	FMGL->phi_val = phi;
	FMGL->set_popup(pop_graph,FMGL,this);
	mglCanvasFL *gr = new mglCanvasFL;	gr->mgl = this;
	FMGL->set_graph(gr);
	scroll->end();	resizable(scroll);	end();	par=0;
	dlg_wnd = NULL;	dlg_done = false;
}
Fl_MGLView::~Fl_MGLView()
{
	delete dlg_wnd;
	for(size_t i=0;i<strs.size();i++)	free(strs[i]);
	strs.clear();	
}
//-----------------------------------------------------------------------------
//
//		class mglCanvasFL
//
//-----------------------------------------------------------------------------
mglCanvasFL::mglCanvasFL() : mglCanvasWnd()	{	Wnd=0;	mgl=0;	}
mglCanvasFL::~mglCanvasFL()		{	if(Wnd)	{	mgl->FMGL->gr=0;	delete Wnd;	}	}
//-----------------------------------------------------------------------------
void mglCanvasFL::GotoFrame(int d)
{
	int f = GetCurFig()+d;
	if(f>=GetNumFig())	f = 0;
	if(f<0)	f = GetNumFig()-1;
	if(GetNumFig()>0 && d)	{	SetCurFig(f);	mgl->FMGL->refresh();	}
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_makemenu_fltk(Fl_Menu_ *m, Fl_MGLView *w)
{
	m->add(_("Graphics/Alpha"), "^t", mgl_alpha_cb, w, FL_MENU_TOGGLE);
	m->add(_("Graphics/Light"), "^l", mgl_light_cb, w, FL_MENU_TOGGLE);
	m->add(_("Graphics/Grid"), "^g", mgl_grid_cb, w, FL_MENU_TOGGLE|FL_MENU_DIVIDER);

	m->add(_("Graphics/Restore"), "^ ", mgl_norm_cb, w);
	m->add(_("Graphics/Redraw"), FL_F+5, mgl_draw_cb, w);
	m->add(_("Graphics/Adjust size"), FL_F+6, mgl_adjust_cb, w);
	m->add(_("Graphics/Reload data"), FL_F+9, mgl_oncemore_cb, w);
	m->add(_("Graphics/Stop"), FL_F+7, mgl_stop_cb, w);
	m->add(_("Graphics/Copy graphics"), "+^c", mgl_copyimg_cb, w);
	m->add(_("Graphics/Copy click coor."), 0, copy_coor_cb, w);
	m->add(_("Graphics/Pause calc"), "^t", mgl_pause_cb, w, FL_MENU_TOGGLE);

#if MGL_HAVE_PNG
	m->add(_("Graphics/Export/as PNG"), "#p", mgl_export_png_cb, w);
	m->add(_("Graphics/Export/as solid PNG"), "#f", mgl_export_pngn_cb, w);
#endif
#if MGL_HAVE_JPEG
	m->add(_("Graphics/Export/as JPEG"), "#j", mgl_export_jpeg_cb, w);
#endif
#if MGL_HAVE_GIF
	m->add(_("Graphics/Export/as GIF"), "#g", mgl_export_gif_cb, w);
#endif
	m->add(_("Graphics/Export/as BMP"), 0, mgl_export_bmp_cb, w);
	m->add(_("Graphics/Export/as SVG"), "#s", mgl_export_svg_cb, w);
	m->add(_("Graphics/Export/as vector EPS"), "#e", mgl_export_eps_cb, w);
	m->add(_("Graphics/Export/as bitmap EPS"), 0, mgl_export_bps_cb, w);
	m->add(_("Graphics/Export/as TeX"), "#l", mgl_export_tex_cb, w, FL_MENU_DIVIDER);
	m->add(_("Graphics/Export/as PRC"), "#d", mgl_export_prc_cb, w);
	m->add(_("Graphics/Export/as OBJ"), "#o", mgl_export_obj_cb, w);
	m->add(_("Graphics/Export/as OFF"), 0, mgl_export_off_cb, w);
	m->add(_("Graphics/Export/as STL"), 0, mgl_export_stl_cb, w);
	m->add(_("Graphics/Export/as XYZ"), 0, mgl_export_xyz_cb, w);

	m->add(_("Graphics/Animation/Slideshow"), FL_CTRL+FL_F+5, mgl_sshow_cb, w, FL_MENU_TOGGLE);
	m->add(_("Graphics/Animation/Next frame"), "#<", mgl_snext_cb, w);
	m->add(_("Graphics/Animation/Prev frame"), "#>", mgl_sprev_cb, w);

	m->add(_("Graphics/Transform/Move left"), FL_ALT+FL_Left, mgl_sl_cb, w);
	m->add(_("Graphics/Transform/Move up"), FL_ALT+FL_Up, mgl_su_cb, w);
	m->add(_("Graphics/Transform/Zoom in"), "#=", mgl_sz_cb, w);
	m->add(_("Graphics/Transform/Zoom out"), "#-", mgl_so_cb, w);
	m->add(_("Graphics/Transform/Move down"), FL_ALT+FL_Down, mgl_sd_cb, w);
	m->add(_("Graphics/Transform/Move right"), FL_ALT+FL_Right, mgl_sr_cb, w);
}
//-----------------------------------------------------------------------------
void mglCanvasFL::Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p), const char *title, void *par, void (*reload)(void *p), bool maximize)
{
	static bool first=true;

	Fl_Preferences pref(Fl_Preferences::USER,"abalakin","mgllab");
	static const char *sch[4]={"base","gtk+","plastic","gleam"};
	int scheme;	pref.get("scheme",scheme,2);
	Fl::scheme(sch[scheme]);

	if(first)	{	Fl::lock();	first=false;	}

	SetDrawFunc(draw, par, reload);
	if(Wnd)	{	Wnd->label(title);	Wnd->show();	return;	}

	Wnd = new Fl_Double_Window(830,660,title);
	mgl = new Fl_MGLView(0,30,830,630);		mgl->par = this;
	mgl->menu = new Fl_Menu_Bar(0, 0, 830, 30);
	mgl_makemenu_fltk(mgl->menu, mgl);

	mgl->next = mgl_fl_next;	mgl->reload = mgl_fl_reload;
	mgl->prev = mgl_fl_prev;	mgl->delay= mgl_fl_delay;
	mgl->FMGL->set_graph(this);
	mgl->FMGL->set_draw(draw, par);
	mgl->FMGL->set_prop(mgl_prop_func, this);

	Wnd->end();
	Wnd->resizable(mgl);

	if(maximize)
	{
		int x,y,w,h;
		Fl::screen_xywh(x,y,w,h);
		Wnd->resize(x,y,w,h);
		Adjust();
	}

	static char ctmp[1];	ctmp[0]=0;
	static char *tmp[1];	tmp[0]=ctmp;
	Wnd->show(argv ? argc:0, argv ? argv:tmp);
}
//-----------------------------------------------------------------------------
HMGL MGL_EXPORT mgl_create_graph_fltk(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p))
{
	mglCanvasFL *g = new mglCanvasFL;
	g->Window(0,0,draw,title,par,load);
	g->mgl->FMGL->set_handle_key(true);
	return g;
}
MGL_EXPORT_PURE void* mgl_fltk_widget(HMGL gr)
{
	mglCanvasFL *g = dynamic_cast<mglCanvasFL *>(gr);
	return g?g->mgl:NULL;
}
int MGL_EXPORT mgl_fltk_run()		{	return Fl::run();	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_graph_fltk_(const char *title, int l)
{
	char *s = new char[l+1];	memcpy(s,title,l);	s[l]=0;
	uintptr_t t = uintptr_t(mgl_create_graph_fltk(0,s,0,0));
	delete []s;	return t;
}
int MGL_EXPORT mgl_fltk_run_()	{	return mgl_fltk_run();	}
//-----------------------------------------------------------------------------
static void *mgl_fltk_tmp(void *)
{	mgl_fltk_run();	return 0;	}
//-----------------------------------------------------------------------------
int MGL_EXPORT mgl_fltk_thr()		// NOTE: Qt couldn't be running in non-primary thread
{
#if MGL_HAVE_PTHR_WIDGET
	static pthread_t thr;
	pthread_create(&thr,0,mgl_fltk_tmp,0);
	pthread_detach(thr);
#endif
	return 0;	// stupid, but I don't want keep result returned by Fl::Run()
}
//-----------------------------------------------------------------------------
//
//		Custom dialog
//
//-----------------------------------------------------------------------------
static void mgl_upd_vals(Fl_Widget *, void *p)	{	((Fl_MGLView *)p)->get_values();	}
//-----------------------------------------------------------------------------
static void mgl_dlg_hide(Fl_Widget *, void *p)	{	((Fl_MGLView *)p)->dlg_hide();	}
//-----------------------------------------------------------------------------
void Fl_MGLView::dlg_window(const char *title)
{
	if(!title || *title==0)	title = "MGL dialog";
	if(!dlg_wnd)	delete dlg_wnd;
	dlg_wnd = new Fl_Double_Window(210,50,title);
// 	else
// 	{	dlg_wnd->hide();	dlg_wnd->label(title);
// 		dlg_wnd->clear();	dlg_wnd->begin();	}
	for(size_t i=0;i<strs.size();i++)	free(strs[i]);
	strs.clear();	dlg_ind = 0;
}
//-----------------------------------------------------------------------------
void Fl_MGLView::dlg_finish()
{
	if(!dlg_wnd)	dlg_window();
	if(!dlg_done)
	{
		Fl_Button *b;	dlg_wnd->size(210,dlg_ind*45+50);
		b = new Fl_Button(5, 20+45*dlg_ind, 80, 25, _("Close"));
		b->callback(mgl_dlg_hide,this);
		b = new Fl_Button(125, 20+45*dlg_ind, 80, 25, _("Update"));
		b->callback(mgl_upd_vals,this);
		dlg_wnd->end();	dlg_done=true;
	}
}
//-----------------------------------------------------------------------------
void Fl_MGLView::get_values()
{
	if(!dlg_wnd || !dlg_done)	return;
	for(unsigned i=0;i<dlg_ids.size();i++)
	{
		std::string s;
		Fl_Widget *w=dlg_wdgt[i];
		switch(dlg_kind[i])
		{
		case 'e':	//	input
		{	Fl_Input* o = (Fl_Input*)w;	s = o->value();	break;	}
		case 'v':	// spinner|counter
		case 's':	// slider
		{	Fl_Valuator* o = (Fl_Valuator*)w;
			double v = o->value();	char buf[32];
			sprintf(buf,"%g",v);	s = buf;	break;	}
		case 'b':	// check_box
		{	Fl_Check_Button* o = (Fl_Check_Button*)w;	s = o->value()?"1":"0";	break;	}
		case 'c':	// choice
		{	Fl_Choice* o = (Fl_Choice*)w;	s = o->text();	break;	}
		}
		FMGL->set_param(dlg_ids[i], s.c_str());
	}
	FMGL->update();
}
//-----------------------------------------------------------------------------
void Fl_MGLView::add_widget(char id, const char *args)
{
	static std::vector<std::string> buf;
	if(!dlg_wnd)	dlg_window();
	if(args[1]!='|')	return;	// wrong format
	char type = *args;
	if(!strchr("esvbc",type))	return;
	args += 2;
	Fl_Widget *w=NULL;
	char *lbl=0;
	for(size_t i=0;args[i];i++)	if(args[i]=='|')	// find label
	{	lbl = (char*)malloc(i);	lbl[i]=0;
		for(size_t j=0;j<i;j++)	lbl[j] = args[j];
		args += i;	break;	}
	if(!lbl)
	{	lbl = (char*)malloc(3);	lbl[0]='$';	lbl[1]=id;	lbl[2]=0;	}
	else	args++;
	strs.push_back(lbl);	dlg_wnd->size(210,dlg_ind*45+50);
	switch(type)
	{
	case 'e':	//	input
	{	Fl_Input* o = new Fl_Input(5, 20+45*dlg_ind, 200, 25, lbl);	w=o;
		o->align(Fl_Align(FL_ALIGN_TOP_LEFT));	o->value(args);
		break;	}
	case 'v':	// spinner|counter
	{	Fl_Counter* o = new Fl_Counter(5, 20+45*dlg_ind, 200, 25, lbl);	w=o;
		o->align(Fl_Align(FL_ALIGN_TOP_LEFT));	o->type(FL_SIMPLE_COUNTER);
		float v=0,v1=-1,v2=1,s1=1;
		sscanf(args,"%g|%g|%g|%g",&v,&v1,&v2,&s1);
		o->step(s1);	o->bounds(v1,v2);	o->value(v);
		break;	}
	case 's':	// slider
	{	Fl_Slider* o = new Fl_Value_Slider(5, 20+45*dlg_ind, 200, 25, lbl);	w=o;
		o->align(Fl_Align(FL_ALIGN_TOP_LEFT));	o->type(FL_HORIZONTAL);
		float v=0,v1=-1,v2=1,s=0;
		sscanf(args,"%g|%g|%g|%g",&v,&v1,&v2,&s);
		o->step(s);	o->bounds(v1,v2);	o->value(v);
		break;	}
	case 'b':	// check_box
	{	Fl_Check_Button* o = new Fl_Check_Button(5, 10+45*dlg_ind, 200, 25, lbl);	w=o;
		int v = atoi(args);	o->value(v!=0 || !strcmp(args,"on"));
		break;	}
	case 'c':	// choice
	{	Fl_Choice* o = new Fl_Choice(5, 20+45*dlg_ind, 200, 25, lbl);	w=o;
		lbl = (char*)malloc(strlen(args)+1);
		strcpy(lbl,args);	strs.push_back(lbl);
		o->align(Fl_Align(FL_ALIGN_TOP_LEFT));
		o->add(lbl);	o->value(0);
		break;	}
	}
	if(w)
	{	const char *tip = _("This is for parameter ");
		lbl = (char*)malloc(strlen(tip)+2);
		sprintf(lbl,"%s%c",tip,id);	strs.push_back(lbl);	w->tooltip(lbl);
		dlg_ind++;	dlg_done=false;	w->callback(mgl_upd_vals, this);
		dlg_wdgt.push_back(w);	dlg_ids.push_back(id);	dlg_kind.push_back(type);	}
}
//-----------------------------------------------------------------------------
void Fl_MGLView::set_progress(int value, int maximal)
{
	progress->maximum(maximal);
	progress->value(value);
	Fl::awake();
}
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_progress_fltk(int value, int maximal, HMGL gr)
{
	mglCanvasFL *g = dynamic_cast<mglCanvasFL *>(gr);
	if(g && g->mgl)	g->mgl->set_progress(value,maximal);
}
//-----------------------------------------------------------------------------
