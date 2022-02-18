/* udav.h is part of UDAV
 * Copyright (C) 2007-2014 Alexey Balakin <mathgl.abalakin@gmail.ru>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
//-----------------------------------------------------------------------------
#ifndef _UDAV_H_
#define _UDAV_H_
//-----------------------------------------------------------------------------
#ifdef __MWERKS__
# define FL_DLL
#endif
#include <FL/Fl.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Double_Window.H>
#include <FL/fl_ask.H>
#include <FL/Fl_File_Chooser.H>
#include <FL/Fl_Menu_Bar.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Return_Button.H>
#include <FL/Fl_Text_Buffer.H>
#include <FL/Fl_Text_Editor.H>
#include <FL/Fl_Pixmap.H>
#include <FL/Fl_Counter.H>
#include <FL/Fl_Scroll.H>
#include <FL/Fl_Tabs.H>
#include <FL/Fl_Help_View.H>
#include <FL/Fl_Table.H>
#include <FL/Fl_Round_Button.H>
#include <FL/Fl_Float_Input.H>
#include <FL/Fl_Multiline_Input.H>
#include <FL/Fl_Multiline_Output.H>
//-----------------------------------------------------------------------------
#include "mgl2/Fl_MathGL.h"
//-----------------------------------------------------------------------------
extern mglParse *Parse;
extern Fl_Menu_Item colors[];
struct Fl_MGL;
//-----------------------------------------------------------------------------
extern Fl_Preferences pref;
extern Fl_Text_Display::Style_Table_Entry styletable[10];
extern int changed;		///< flag of script is changed or not
extern std::string filename;	///< Current filename
extern std::string lastfiles[5];///< Last opened files
extern std::string search;		///< Text to search
extern Fl_Text_Buffer *textbuf;
//-----------------------------------------------------------------------------
extern int auto_exec;	///< Enable auto execution
extern int exec_save;	///< Save before running
extern int highlight;	///< Highlight current line
extern int mouse_zoom;	///< Use mouse wheel for zooming
extern std::string helpname;	///< Path to help files
extern std::string fontname;	///< Path to font files
extern int lang;		///< Locale for script and help files
extern int scheme;		///< FLTK scheme
extern int font_kind;	///< Editor font kind
extern int font_size;	///< Editor font size
extern int complete_word;	///< enable word completion
//-----------------------------------------------------------------------------
void set_scheme_lang(int s, int l);		///< Set FLTK scheme and locale
void set_style(int fkind, int fsize);	///< Change the style of highlight
void style_init();		///< Initialize the style buffer
void save_pref();		///< Apply and save preferences
void load_pref();		///< Load preferences
void add_filename(const char *fname);	///< Add filename to lastfiles
std::string wcstombs(std::wstring wcs);	///< Convert std::wstring to std::string
//-----------------------------------------------------------------------------
class Fl_Data_Table : public Fl_Table
{
private:
	int row, col;
	Fl_Input* input;
protected:
	void draw_cell(TableContext context, int R, int C, int X, int Y, int W, int H);
	static void event_callback(Fl_Widget*, void*v)
	{	((Fl_Data_Table*)v)->cell_click();	}
	void cell_click();

public:
	mglDataA *data;
	int nx, ny, sl;

	Fl_Data_Table(int x, int y, int w, int h, const char *l=0);
    ~Fl_Data_Table() { }

	void set_value();
    void rows(int val) { if (input->visible()) input->do_callback(); Fl_Table::rows(val); }
    void cols(int val) { if (input->visible()) input->do_callback(); Fl_Table::cols(val); }
    inline int rows() { return Fl_Table::rows(); }
    inline int cols() { return Fl_Table::cols(); }
};
//-----------------------------------------------------------------------------
class ScriptWindow;
struct Fl_MGL : public mglDraw
{
	ScriptWindow *e;
	Fl_MGLView *gr;
	std::vector<std::string> anim;
	mreal delay;
	std::string script;		///< script with settings
	size_t cur;				///< current frame
	double a1, a2, da;		///< animation loop parameters

	Fl_MGL(Fl_MGLView *GR);
	~Fl_MGL();

	void Reload();			///< Function for reloading data
	void Click();			///< Callback function on mouse click
	int Draw(mglGraph *);	///< Drawing itself
	void Param(char id, const char *val);	///< Function for setting parameter
	void update();			///< Update (redraw) plot
	void next_frame();		///< Show next frame
	void prev_frame();		///< Show prev frame
	HMGL get_graph()		///< Get pointer to grapher
	{	return gr->FMGL->get_graph();	}
};
//-----------------------------------------------------------------------------
struct TableWindow
{
public:
	TableWindow(ScriptWindow *e);
	~TableWindow();
	void update(mglDataA *v);
	void refresh();
	void set_slice(long s);
	inline long get_slice() {	return sl;	}
	inline long num_slice()	{	return nz;	}
	void go_home();
	void show()	{	w->show();	}

	ScriptWindow *main;
	Fl_Counter *slice;
	mglDataA *var;
protected:
	TableWindow(const TableWindow &){}	// copying is not allowed
	Fl_Data_Table *data;
	Fl_Menu_Bar	*menu;
	Fl_Double_Window *w;
//	long nx,ny,nz;
	long nz;
	long sl;		// current slice
	char sl_id[64];	// slice id
};
//-----------------------------------------------------------------------------
class ScriptWindow : public Fl_Double_Window
{
public:
	ScriptWindow(int w, int h, const char* t);
	~ScriptWindow()	{}

	Fl_Text_Editor *editor;
	Fl_Menu_Bar *menu;
	Fl_Tabs  *rtab;
	Fl_Help_View *hd;
	Fl_Input *link_cmd;
	Fl_Group *ghelp, *gplot;
	Fl_Browser *var;
	Fl_Box *status;

	void set_status(const char *txt);
	void mem_init();
	void mem_pressed(int n);
	Fl_MGLView *graph;
	Fl_MGL *draw;
};
//-----------------------------------------------------------------------------
class GeneralDlg
{
protected:
	Fl_Double_Window *w;
public:
//	GeneralDlg()	{	mgl_textdomain(NULL);	}
	mglDataA *dat;
	ScriptWindow *e;
	std::string result;
	virtual void cb_ok(){}
	virtual void init()	{	result.clear();	}
	void show()	{	init();	w->show();	}
	void hide()	{	w->hide();	}
	bool wait()	{	while(w->shown())	Fl::wait();	return result.empty();	}
};
void cb_dlg_cancel(Fl_Widget*, void*);
void cb_dlg_ok(Fl_Widget*, void*);
void cb_dlg_only(Fl_Widget*,void *v);
//-----------------------------------------------------------------------------
// Editor window functions
void insert_cb(Fl_Widget *, void *);
//-----------------------------------------------------------------------------
void paste_cb(Fl_Widget *, void *);
void select_all_cb(Fl_Widget *, void *);
void undo_cb(Fl_Widget *, void *);
void copy_cb(Fl_Widget *, void *);
void cut_cb(Fl_Widget *, void *);
void delete_cb(Fl_Widget *, void *);
void changed_cb(int, int nInserted, int nDeleted,int, const char*, void* v);
void ins_fname_cb(Fl_Widget *, void *);
void ins_path_cb(Fl_Widget *, void *);
void ins_fits_cb(Fl_Widget *, void *);
void ins_prim_cb(Fl_Widget *, void *);
void hide_cb(Fl_Widget*, void *);
void unhide_cb(Fl_Widget*, void *);
//-----------------------------------------------------------------------------
// General callback functions
void new_cb(Fl_Widget *, void *);
void open_cb(Fl_Widget *, void *);
void save_cb(Fl_Widget*, void*);
void saveas_cb(Fl_Widget*, void*);
void help_cb(Fl_Widget*, void*);
//-----------------------------------------------------------------------------
// Dialogs callback functions
void close_dlg_cb(Fl_Widget *w, void *);
void font_cb(Fl_Widget *, void *v);
void line_cb(Fl_Widget *, void *v);
void face_cb(Fl_Widget *, void *v);
void data_cb(Fl_Widget *, void *v);
//-----------------------------------------------------------------------------
int check_save(void);
void load_file(const char *newfile, int ipos, ScriptWindow *e);
void save_file(const char *newfile, ScriptWindow *e);
Fl_Widget *add_editor(ScriptWindow *w, int txtW, int wndH);
Fl_Widget *add_mem(ScriptWindow *w, int txtW, int wndW, int wndH);
void set_title(Fl_Window* w);
//-----------------------------------------------------------------------------
// Animation
bool animate_cb(Fl_MGL *dr);
void animate_dlg_cb(Fl_Widget *, void *v);
void fill_animate(const char *text, Fl_MGL *dr);
void argument_set(int id, const char *val);
//-----------------------------------------------------------------------------
Fl_Widget *add_help(ScriptWindow *w, int txtW, int wndW, int wndH);
void help_cb(Fl_Widget*, void*v);
void link_cb(Fl_Widget*, void*v);
void example_cb(Fl_Widget*, void*v);
void about_cb(Fl_Widget*, void*);
//-----------------------------------------------------------------------------
void prop_dlg_cb(Fl_Widget*, void*);
void calc_dlg_cb(Fl_Widget*, void*);
void args_dlg_cb(Fl_Widget*, void*);
void option_dlg_cb(Fl_Widget*, void*);
void dirsel_dlg_cb(Fl_Widget*, void*);
void datsel_dlg_cb(Fl_Widget*, void*);
void style_dlg_cb(Fl_Widget*, void*);
void newcmd_dlg_cb(Fl_Widget*,void*);
void setup_dlg_cb(Fl_Widget*,void *);
void inplot_dlg_cb(Fl_Widget*,void*);
void find_dlg_cb(Fl_Widget*,void*);
void find_next_cb(Fl_Widget*,void*);
void hint_dlg_cb(Fl_Widget*,void*);
void iconlist_cb(Fl_Widget*,void*);
void message_cb(Fl_Widget*,void*);
void message_set(const char *s, ScriptWindow *e);
void info_dlg_cb(mglDataA *d);
void prim_dlg_cb(Fl_Widget*, void* v);
void cb_args_set(const char *val);	///< set value for argument in newcmd_dlg
//-----------------------------------------------------------------------------
extern Fl_Text_Buffer *textbuf;
extern std::string filename;
extern int	changed;
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
