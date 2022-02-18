/***************************************************************************
 * Fl_MathGL.h is part of Math Graphic Library
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
#ifndef _MGL_FL_MATHGL_H_
#define _MGL_FL_MATHGL_H_

#ifdef __MWERKS__
# define FL_DLL
#endif

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Counter.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Progress.H>
#include <mgl2/fltk.h>
class mglCanvas;
//-----------------------------------------------------------------------------
/// Class is FLTK widget which display MathGL graphics
class MGL_EXPORT Fl_MathGL : public Fl_Widget
{
	friend class Fl_MGLView;
public:
	Fl_Valuator	*tet_val;	///< pointer to external tet-angle validator
	Fl_Valuator	*phi_val;	///< pointer to external phi-angle validator
	mglCanvas *gr;			///< Built-in mglCanvas instance (mglCanvasFLTK is used by default)
	std::string prim;		///< manual primitives
	bool use_pthr;			///< use pthread for update plot

	Fl_MathGL(int x, int y, int w, int h, const char *label=0);
	virtual ~Fl_MathGL();
	
	/// Set drawing functions and its parameter
	inline void set_draw(int (*func)(mglBase *gr, void *par), void *par)
	{	if(draw_cl)	delete draw_cl;	draw_cl=0;	draw_func=func;	draw_par=par;	}
	/// Set drawing functions pointed on mglGraph
	inline void set_draw(int (*dr)(mglGraph *gr))
	{	set_draw(dr?mgl_draw_graph:0,(void*)dr);	}
	/// Set drawing based on instance of mglDraw class
	inline void set_draw(mglDraw *dr)
	{	if(draw_cl)	delete draw_cl;	draw_cl=dr;	draw_func=0;	}
	/// Set function for parameters
	inline void set_prop(void (*func)(char id, const char *val, void *par), void *par)
	{	prop_func=func;	prop_par=par;	}

	/// Refresh image (without executing update)
	void refresh();
	/// Update (redraw) plot
	virtual void update();
	/// Set angles for additional plot rotation
	inline void set_angle(double t, double p){	tet = t;	phi = p;	}
	/// Set bitwise flags for general state (1-Alpha, 2-Light)
	inline void set_flag(int f)	{	flag = f;	}
	/// Set flags for handling mouse
	void set_state(bool z, bool r, bool g=false)
	{	zoom = z;	rotate = r;	grid = g;	}
	/// Set zoom in/out region
	inline void set_zoom(double X1, double Y1, double X2, double Y2)
	{	x1 = X1;	x2 = X2;	y1 = Y1;	y2 = Y2;	update();	}
	/// Get zoom region
	inline void get_zoom(double *X1, double *Y1, double *X2, double *Y2)
	{	*X1 = x1;	*X2 = x2;	*Y1 = y1;	*Y2 = y2;	}
	/// Set popup menu pointer
	inline void set_popup(const Fl_Menu_Item *pmenu, Fl_Widget *wdg, void *v)
	{	popup = pmenu;	wpar = wdg;	vpar = v;	}

	/// Set grapher object instead of built-in one. 
	/// NOTE: Fl_MathGL will automatically delete this object
	void set_graph(HMGL gr);
	/// Set grapher object instead of built-in one. 
	/// NOTE: Fl_MathGL will automatically delete this object
	inline void set_graph(mglGraph *Gr)
	{	set_graph(Gr->Self());	}
	/// Get pointer to grapher
	inline HMGL get_graph()	{	return (HMGL)gr;	}

	/// Get mglDraw pointer or NULL
	inline mglDraw *get_class()
	{	mglDraw *d=0;
		if(draw_func==mgl_draw_class)	d = (mglDraw*)draw_par;
		if(draw_cl)	d = draw_cl;
		return d;	}
	inline void set_param(char id, const char *val)
	{	mglDraw *d=get_class();	if(d)	d->Param(id,val);	else	prop_func(id,val,prop_par);	}
	
	/// Show window with warnings after script parsing
	inline void set_show_warn(bool s)	{	show_warn=s;	}
	/// Ask to stop of script parsing
	void stop(bool stop=true);
	/// Enable/disable key handling as in mglview (default is false)
	inline void set_handle_key(bool val)	{	handle_keys=true;	}
	/// Get id of last clicked object
	inline int get_last_id()	{	return last_id;	}
	void draw_plot();	///< Single thread drawing itself
	/// Check if script is parsing now or not
	inline bool running()	{	return run;	}

protected:
	void *draw_par;		///< Parameters for drawing function draw_func().
	/// Drawing function for window procedure. It should return the number of frames.
	int (*draw_func)(mglBase *gr, void *par);
	void *prop_par;	///< Parameters for prop_func().
	/// Function for setting properties.
	void (*prop_func)(char id, const char *val, void *par);
	mglDraw *draw_cl;
	int last_id;				///< last selected object id

	const Fl_Menu_Item *popup;	///< pointer to popup menu items
	Fl_Widget *wpar;			///< widget for popup menu
	void *vpar;					///< parameter for popup menu
	double tet,phi;				///< rotation angles
	bool rotate;				///< flag for handle mouse
	bool zoom;					///< flag for zoom by mouse
	bool grid;					///< flag to draw grid and edit prim
	bool show_warn;				///< show window with warnings
	bool handle_keys;
	double x1,x2,y1,y2;			///< zoom region
	int flag;					///< bitwise flag for general state (1-Alpha, 2-Light)
	int x0,y0,xe,ye;			///< mouse position
	char mouse_pos[128];
	bool run;					///< flag that drawing in progress
	const unsigned char *img;	///< image for drawing
#if (MGL_HAVE_PTHREAD|MGL_HAVE_PTHR_WIDGET)
	pthread_t thr;				///< main thread for drawing
#endif

	virtual void draw();		///< quick drawing function
	int handle(int code);		///< handle mouse events
	void resize(int x, int y, int w, int h);	///< resize control
};
//-----------------------------------------------------------------------------
class MGL_EXPORT Fl_MGLView : public Fl_Window
{
public:
	Fl_MathGL *FMGL;		///< Control which draw graphics
	Fl_Scroll *scroll;
	Fl_Menu_Bar	*menu;

	void *par;				///< Parameter for handling animation
	void (*next)(void*);	///< Callback function for next frame
	void (*prev)(void*);	///< Callback function for prev frame
	mreal (*delay)(void*);	///< Callback function for delay
	void (*reload)(void*);	///< Callback function for reloading

	/// Toggle transparency (alpha) button
	void toggle_alpha()	{	toggle(alpha, alpha_bt, _("Graphics/Alpha"));	}
	/// Toggle lighting button
	void toggle_light()	{	toggle(light, light_bt, _("Graphics/Light"));	}
	/// Toggle slideshow button
	void toggle_sshow()	{	toggle(sshow, anim_bt, _("Graphics/Animation/Slideshow"));	}
	/// Toggle grid drawing button
	void toggle_grid()	{	toggle(grid, grid_bt, _("Graphics/Grid"));	}
	/// Toggle mouse zoom button
	void toggle_zoom()	{	toggle(zoom, zoom_bt);	}
	/// Toggle mouse rotate button
	void toggle_rotate(){	toggle(rotate, rotate_bt);	}
	/// Switch off zoom button
	void setoff_zoom()	{	setoff(zoom, zoom_bt);	}
	/// Switch off rotate button
	void setoff_rotate(){	setoff(rotate, rotate_bt);	}
	/// Check if slideshow running
	bool is_sshow()		{	return sshow;	}
	/// Toggle pause calculation button
	void toggle_pause()	{	toggle(pauseC, pause_bt, _("Graphics/Pause calc"));	exec_pause();	}
	/// Adjust image sizes to the current widget sizes
	void adjust()
	{	mgl_set_size(FMGL->get_graph(),scroll->w(),scroll->h());	FMGL->size(scroll->w(),scroll->h());	update();	}
	/// Get current grapher
	HMGL get_graph()	{	return FMGL->get_graph();	}
	/// Update picture by calling user drawing function
	void update();
	
	/// Create and show custom dialog 
	void dialog(const char *ids, char const * const *args, const char *title="")
	{
		if(!ids || *ids==0)	return;
		dlg_window(title);
		for(int i=0;ids[i];i++)	add_widget(ids[i], args[i]);
		dlg_finish();	dlg_wnd->show();
	}
	void dialog(const std::string &ids, const std::vector<std::string> &args, const char *title="")
	{
		std::vector<const char *> buf;	buf.reserve(args.size());
		for(size_t i=0;i<args.size();i++)	buf.push_back(args[i].c_str());
		dialog(ids.c_str(), &(buf[0]), title);
// 		dlg_window(title);
// 		for(size_t i=0;i<ids.length();i++)	add_widget(ids[i], args[i].c_str());
// 		dlg_finish();	dlg_wnd->show();
	}
	void dlg_window(const char *title="");	///< Create/label dialog window
	void add_widget(char id, const char *args);	///< Add widget to dialog
	void dlg_show()	{	dlg_finish();	dlg_wnd->show();	}	///< Show window
	void dlg_hide()	{	dlg_wnd->hide();	}	///< Close window
	void get_values();	///< Get all values from dialog window
	void set_progress(int value, int maximal);	///< Set progress

	Fl_MGLView(int x, int y, int w, int h, const char *label=0);
	virtual ~Fl_MGLView();
protected:
	Fl_Button *alpha_bt, *light_bt, *rotate_bt, *anim_bt, *zoom_bt, *grid_bt, *pause_bt;
	Fl_Progress *progress;

	int grid, alpha, light;	///< Current states of wire, alpha, light switches (toggle buttons)
	int sshow, rotate, zoom;///< Current states of slideshow, rotate, zoom switches (toggle buttons)
	int pauseC;	///< Current state of pause for calculations

	void toggle(int &val, Fl_Button *b, const char *txt=NULL);
	void setoff(int &val, Fl_Button *b, const char *txt=NULL);
	void exec_pause();

	Fl_Double_Window *dlg_wnd;	///< Dialog window itself
	std::vector<char*> strs;	///< Strings for widget labels
	bool dlg_done;		///< Dialog is created
	int dlg_ind;		///< Current index of widget
	std::vector<char> dlg_kind;			///< Kind of elements
	std::vector<Fl_Widget*> dlg_wdgt;	///< List of widgets
	std::vector<char> dlg_ids;			///< Id of elements
	std::vector<std::string> dlg_vals;	///< resulting strings
	void dlg_finish();	///< Finish dialog window creation

};
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_makemenu_fltk(Fl_Menu_ *m, Fl_MGLView *w);
MGL_EXPORT const char *mgl_file_chooser(const char *mess, const char *filter="", bool save=false);
MGL_EXPORT const char *mgl_dir_chooser(const char *mess, const char *path);
//-----------------------------------------------------------------------------
#endif
