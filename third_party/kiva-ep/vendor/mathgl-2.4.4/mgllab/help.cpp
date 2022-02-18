/* help.cpp is part of UDAV
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
#include "mgllab.h"
#include "../widgets/image.h"
#include <ctype.h>
#include <FL/Fl_Select_Browser.H>
//-----------------------------------------------------------------------------
void help_cb(Fl_Widget*, void*v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	int cur = e->editor->insert_position(), br=0;
	int beg = textbuf->line_start(cur);
	const char *s = textbuf->text();
	for(int i=beg;i<cur;i++)
	{
		if(strchr("({[",s[i]))	br++;
		if(strchr(")}]",s[i]))	br--;
		if(br==0 && s[i]==':' && i+1<cur)	beg=i+1;
	}
	for(br=beg;s[br]>' ' && s[br]!=':';br++);
	std::string cmd(s+beg,br-beg);
	e->link_cmd->value(cmd.c_str());
	static std::string str;
	str = helpname+cmd;
	e->hd->load(str.c_str());
	if(e->rtab)	e->rtab->value(e->ghelp);
}
//-----------------------------------------------------------------------------
void link_cb(Fl_Widget*, void*v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	const char *s = e->link_cmd->value();
	if(!s)	s="Examples";
	static std::string str;
	str = helpname+s;
	e->hd->load(str.c_str());
	if(e->rtab)	e->rtab->value(e->ghelp);
}
//-----------------------------------------------------------------------------
void example_cb(Fl_Widget*, void*v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	static std::string str = helpname+"Examples";
	e->hd->load(str.c_str());	e->rtab->value(e->ghelp);
	if(e->rtab)	e->rtab->value(e->ghelp);
}
//-----------------------------------------------------------------------------
void help_in_cb(Fl_Widget*, void*v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	e->hd->textsize(e->hd->textsize()+1);
}
//-----------------------------------------------------------------------------
void help_out_cb(Fl_Widget*, void*v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	e->hd->textsize(e->hd->textsize()-1);
}
//-----------------------------------------------------------------------------
#include "xpm/udav.xpm"
void about_cb(Fl_Widget*, void*)
{
	static char s[128];
	snprintf(s,128,_("mgllab v. 2.%g\n(c) Alexey Balakin, 2017\nhttp://mathgl.sf.net/"), MGL_VER2);
	Fl_Double_Window* w = new Fl_Double_Window(355, 130, "About UDAV");
	Fl_Box* o = new Fl_Box(10, 15, 65, 65);
	o->box(FL_UP_BOX);	o->color(55);	o->image(new Fl_Pixmap(udav_xpm));
	o = new Fl_Box(85, 15, 260, 65);	o->label(s);
	Fl_Button *b = new Fl_Return_Button(255, 90, 90, 30, "Close");
	b->callback(close_dlg_cb,w);
	w->end();	w->set_modal();	w->show();
}
//-----------------------------------------------------------------------------
#include "xpm/zoom-out.xpm"
#include "xpm/zoom-in.xpm"
#include "xpm/help-faq.xpm"
Fl_Widget *add_help(ScriptWindow *w, int txtW, int wndW, int wndH)
{
	Fl_Window *w1=new Fl_Window(txtW,30,wndW-txtW,wndH-80,0);
	Fl_Group *g = new Fl_Group(0,0,230,30);
	Fl_Button *o;

	w->link_cmd = new Fl_Input(0,1,150,25);
	w->link_cmd->when(FL_WHEN_CHANGED);
	w->link_cmd->callback(link_cb,w);

	o = new Fl_Button(155, 1, 25, 25);	o->tooltip(_("MGL samples and hints"));
	o->image(new Fl_Pixmap(help_faq_xpm));	o->callback(example_cb,w);
	o = new Fl_Button(180, 1, 25, 25);	o->tooltip(_("Increase font size"));
	o->image(new Fl_Pixmap(zoom_in_xpm));	o->callback(help_in_cb,w);
	o = new Fl_Button(205, 1, 25, 25);	o->tooltip(_("Decrease font size"));
	o->image(new Fl_Pixmap(zoom_out_xpm));	o->callback(help_out_cb,w);

	g->end();	g->resizable(0);

	w->hd = new Fl_Help_View(0,28,wndW-txtW,wndH-110);
	w1->end();	link_cb(w,w);
	w1->resizable(w->hd);	return w1;
}
//-----------------------------------------------------------------------------
void mem_dlg_cb0(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_pressed(0);	}
//-----------------------------------------------------------------------------
void mem_dlg_cb1(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_pressed(1);	}
//-----------------------------------------------------------------------------
void mem_dlg_cb2(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_pressed(2);	}
//-----------------------------------------------------------------------------
void mem_dlg_cb3(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_pressed(3);	}
//-----------------------------------------------------------------------------
void mem_dlg_cb4(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_pressed(4);	}
//-----------------------------------------------------------------------------
void mem_update_cb(Fl_Widget *, void *v)
{	((ScriptWindow*)v)->mem_init();	}
//-----------------------------------------------------------------------------
void delete_all_cb(Fl_Widget *, void *v)
{
	if(fl_choice(_("Are you sure that you want to delete ALL data arrays?"), _("Yes"), _("No"), NULL) == 0)
	{	Parse->DeleteAll();	((ScriptWindow*)v)->mem_init();	}
}
//-----------------------------------------------------------------------------
Fl_Widget *add_mem(ScriptWindow *w, int txtW, int wndW, int wndH)
{
	static int widths[] = {220,205,205,0};
	Fl_Button *o;
	Fl_Box *b;
	int ww = wndW-txtW, ws = widths[0]+widths[1]+widths[2];
	Fl_Window *wnd = new Fl_Window(txtW,30,ww,wndH-80,0);

	Fl_Group *g = new Fl_Group(0,0,ws,30);
	b = new Fl_Box(0, 10, ww, 25, _("Existing data arrays"));	b->labeltype(FL_ENGRAVED_LABEL);
	b = new Fl_Box(0, 35, widths[0], 25, _("name"));
	b->box(FL_THIN_UP_BOX);	b->align(FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
	b = new Fl_Box(widths[0], 35, widths[1], 25, _("dimensions"));
	b->box(FL_THIN_UP_BOX);	b->align(FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
	b = new Fl_Box(widths[0]+widths[1], 35, widths[2], 25, _("mem. usage"));
	b->box(FL_THIN_UP_BOX);	b->align(FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
	g->end();	g->resizable(0);

	w->var = new Fl_Select_Browser(0, 60, ww, wndH+(335-510));	w->var->column_char('\t');
	w->var->align(FL_ALIGN_TOP);	w->var->column_widths(widths);
	w->var->tooltip(_("List of available data."));

	int dx = (ww-(40+90))/5, yy = wndH-80-30;
	o = new Fl_Button(20, yy, 90, 25, _(" Edit"));	o->callback(mem_dlg_cb0,w);
	o->image(img_grid);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Open table with selected data for editing."));
	o = new Fl_Button(dx+20, yy, 90, 25, _(" Info"));	o->callback(mem_dlg_cb1,w);
	o->image(img_info);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Data information and preview."));
	o = new Fl_Button(2*dx+20, yy, 90, 25, _(" Delete"));	o->callback(mem_dlg_cb2,w);
	o->image(img_delete);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Delete selected data."));
	o = new Fl_Button(3*dx+20, yy, 90, 25, _(" New"));	o->callback(mem_dlg_cb3,w);
	o->image(img_new);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Open dialog for new data creation."));
	o = new Fl_Button(4*dx+20, yy, 90, 25, _(" Save"));	o->callback(mem_dlg_cb4,w);
	o->image(img_save);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Save selected data to file."));
// 	o = new Fl_Button(420, yy, 90, 25, _(" Refresh"));	o->callback(mem_update_cb,w);
// 	o->image(img_update);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
// 	o->tooltip(_("Refresh list of variables."));
	o = new Fl_Button(5*dx+20, yy, 90, 25, _(" Del.all"));	o->callback(delete_all_cb,w);
	o->image(img_clear);	o->align(FL_ALIGN_IMAGE_NEXT_TO_TEXT);
	o->tooltip(_("Delete @b all@. data arrays."));
	wnd->end();	wnd->resizable(w->var);	return wnd;
}
//-----------------------------------------------------------------------------
void ScriptWindow::mem_init()
{
	char str[128];
	var->clear();
	for(long i=0;i<Parse->GetNumVar();i++)
	{
		mglDataA *v = Parse->GetVar(i);
		if(v && !v->temp)
		{
			long sv = 0;
			if(dynamic_cast<mglData*>(v))	sv = v->GetNN()*sizeof(mreal)+sizeof(mglData);
			else if(dynamic_cast<mglDataC*>(v))	sv = v->GetNN()*sizeof(dual)+sizeof(mglDataC);
			else if(dynamic_cast<mglDataV*>(v))	sv = sizeof(mglDataV);
			else if(dynamic_cast<mglDataW*>(v))	sv = sizeof(mglDataW);
			else if(dynamic_cast<mglDataF*>(v))	sv = sizeof(mglDataF);
			else if(dynamic_cast<mglDataR*>(v))	sv = sizeof(mglDataR);
			else if(dynamic_cast<mglDataT*>(v))	sv = sizeof(mglDataT);
			const char *ext[]={_("unknown"),"b","Kb","Mb","Gb","Tb","Pb","Eb","Zb","Yb"}, *e;
			if(sv==0)	e = ext[0];
#if MGL_SIZEOF_LONG>4
//			else if((sv>>80L)>0)	{	e=ext[9];	sv = sv>>80L;	}
//			else if((sv>>70L)>0)	{	e=ext[8];	sv = sv>>70L;	}
			else if((sv>>60L)>0)	{	e=ext[7];	sv = sv>>60L;	}
			else if((sv>>50L)>0)	{	e=ext[6];	sv = sv>>50L;	}
			else if((sv>>40L)>0)	{	e=ext[5];	sv = sv>>40L;	}
#endif
			else if((sv>>30L)>0)	{	e=ext[4];	sv = sv>>30L;	}
			else if((sv>>20L)>0)	{	e=ext[3];	sv = sv>>20L;	}
			else if((sv>>10L)>0)	{	e=ext[2];	sv = sv>>10L;	}
			else	e=ext[1];
			snprintf(str,128,"%ls\t%ld*%ld*%ld\t%ld %s", v->Name(), v->GetNx(), v->GetNy(), v->GetNz(), sv, e);
			var->add(str,v);
		}
	}
	for(long i=0;i<Parse->GetNumConst();i++)
	{
		mglNum *v = Parse->GetConst(i);
		snprintf(str,128,"%ls\t%s\t%zu b", v->s.w, ("const="+mgl_str_num(v->c)).c_str(), sizeof(mglNum));
		var->add(str,v);
	}
}
//-----------------------------------------------------------------------------
void ScriptWindow::mem_pressed(int kind)
{
	TableWindow *w;
	int ind = var->value();
	mglDataA *v = (mglDataA *)var->data(ind);
	const char *s = var->text(ind);
	bool dat = s && !strstr(s,"\tconst");
	if(!v && kind!=3)	return;
	if(dat && kind==0)
	{
		w = (TableWindow *)v->o;
		if(!w)	w = new TableWindow(this);
		w->update(v);	w->show();
	}
	else if(dat && kind==1)	info_dlg_cb(v);
	else if(dat && kind==2)	Parse->DeleteVar(v->Name());
	else if(kind==3)
	{
		const char *name = fl_input(_("Enter name for new variable"),"dat");
		if(!name)	return;
		v = Parse->AddVar(name);
		w = v->o? (TableWindow*)v->o:new TableWindow(this);
		w->update(v);	w->show();
	}
	else if(dat && kind==4)
	{
		const char *newfile = mgl_file_chooser(_("Save Data?"),
				_("DAT Files \t*.{dat,csv}\nHDF Files \t*.{h5,hdf}"), true);
		if(newfile)
		{
			const char *ext = fl_filename_ext(newfile);
			if(!strcmp(ext,"h5") || !strcmp(ext,"hdf"))	// this is HDF file
			{
				std::string name = wcstombs(v->Name());
				v->SaveHDF(newfile, name.c_str());
			}
			else	v->Save(newfile);
		}
	}
	mem_init();
}
//-----------------------------------------------------------------------------
void cb_hint_prev(Fl_Widget*,void*);
void cb_hint_next(Fl_Widget*,void*);
class HintDlg : public GeneralDlg
{
	Fl_Help_View *hint;
	Fl_Check_Button *start;
	int cur;
public:
	HintDlg() : GeneralDlg()
	{
		Fl_Button *o;
		w = new Fl_Double_Window(280, 265);	cur=0;
		hint = new Fl_Help_View(10, 10, 260, 185);
		hint->value(mgl_hints[0]);
		start = new Fl_Check_Button(10, 200, 260, 25, _("Show hint on startup"));
		o = new Fl_Button(10, 230, 80, 25, _("@<-  Prev"));
		o->callback(cb_hint_prev);
		o = new Fl_Button(100, 230, 80, 25, _("Next @->"));
		o->callback(cb_hint_next);
		o = new Fl_Return_Button(190, 230, 80, 25, _("Close"));
		o->callback(cb_dlg_ok,this);
		w->end();
	}
	void init()
	{	int sh;	pref.get("show_hint",sh,1);	start->value(sh);	}
	void cb_ok()
	{	pref.set("show_hint",start->value());	hide();	}
	void prev()
	{
		int n=0;	while(mgl_hints[n])	n++;
		cur = cur>0?cur-1:n-1;
		hint->value(mgl_hints[cur]);
	}
	void next()
	{
		int n=0;	while(mgl_hints[n])	n++;
		cur = cur<n-1?cur+1:0;
		hint->value(mgl_hints[cur]);
	}
} hint_dlg;
//-----------------------------------------------------------------------------
void cb_hint_prev(Fl_Widget*,void*)	{	hint_dlg.prev();	}
void cb_hint_next(Fl_Widget*,void*)	{	hint_dlg.next();	}
//-----------------------------------------------------------------------------
void hint_dlg_cb(Fl_Widget*,void *)	{	hint_dlg.show();	}
//-----------------------------------------------------------------------------
void cb_info_prev(Fl_Widget*,void*);
void cb_info_next(Fl_Widget*,void*);
void cb_info_1d(Fl_Widget*,void*);
void cb_info_2d(Fl_Widget*,void*);
void cb_info_3d(Fl_Widget*,void*);
class InfoDlg : public GeneralDlg
{
	Fl_Multiline_Output *out;
	Fl_MathGL *gr;
	long nx, ny, nz;
	long sl;
	int plot;
	std::string name;
public:
	InfoDlg() : GeneralDlg()
	{
		nx=ny=nz=sl=plot=0;
		Fl_Button *o;
		w = new Fl_Double_Window(420, 530);
		out = new Fl_Multiline_Output(10, 25, 400, 150, _("Information"));
		out->align(FL_ALIGN_TOP_LEFT);
		gr = new Fl_MathGL(10, 220, 400, 300);	gr->box(FL_ENGRAVED_BOX);	gr->use_pthr = false;
		mgl_set_size(gr->get_graph(),400,300);
		o = new Fl_Button(10, 185, 25, 25, "@<-");
		o->callback(cb_info_prev,this);
		o = new Fl_Button(40, 185, 75, 25, _("1D view"));
		o->callback(cb_info_1d,this);
		o = new Fl_Button(120, 185, 75, 25, _("2D view"));
		o->callback(cb_info_2d,this);
		o = new Fl_Button(200, 185, 75, 25, _("3D view"));
		o->callback(cb_info_3d,this);
		o = new Fl_Button(280, 185, 25, 25, "@->");
		o->callback(cb_info_next,this);
		o = new Fl_Return_Button(335, 185, 75, 25, _("Close"));
		o->callback(cb_dlg_cancel,this);
		w->set_modal();	w->end();
	}
	void update()
	{
		if(!dat)	return;
		std::string script;
		char buf[32];
		switch(plot)
		{
		case 0:
			if(sl<0)	sl=ny-1;
			if(sl>=ny)	sl=0;
			snprintf(buf,31,"%ld",sl);
			script = "subplot 1 1 0 '<_':xrange 0 1:yrange " + name + ":plot " + name + "(:," + buf + "):axis:box:text 1 1 'sl=" + buf + "' 'r:aR'";
			break;
		case 1:
			if(sl<0)	sl=nz-1;
			if(sl>=nz)	sl=0;
			snprintf(buf,31,"%ld",sl);
			script = "subplot 1 1 0 '':crange " + name + ":dens " + name + "(:,:," + buf + "):box:text 1 1 'sl=" + buf + "' 'r:aR'";
			break;
		case 2:
			script = "rotate 40 60:light on:crange " + name + ":surf3 " + name + ":box";
			break;
		}
		script = "clf:"+script;
		mgl_set_def_param(gr->get_graph());
		mgl_parse_text(gr->get_graph(), Parse->Self(), script.c_str());
		gr->update();
	}
	void prev()		{	sl--;	update();	}
	void next()		{	sl++;	update();	}
	void plot_1d()	{	plot=0;	update();	}
	void plot_2d()	{	if(ny>1)	{	plot=1;	update();	}	}
	void plot_3d()	{	if(nz>1)	{	plot=2;	update();	}	}
	void init()
	{
		nx=ny=nz=sl=plot=0;
		if(dat)
		{
			nx=dat->GetNx();	ny=dat->GetNy();	nz=dat->GetNz();
			result = dat->PrintInfo();	out->value(result.c_str());
			name = wcstombs(dat->Name());
			if(nz>1)		plot_3d();
			else if(ny>1)	plot_2d();
			else			plot_1d();
		}
	}
} info_dlg;
//-----------------------------------------------------------------------------
void info_dlg_cb(mglDataA *d)
{	if(d)	{	info_dlg.dat=d;	info_dlg.show();	}	}
//-----------------------------------------------------------------------------
void cb_info_prev(Fl_Widget*,void*)	{	info_dlg.prev();	}
void cb_info_next(Fl_Widget*,void*)	{	info_dlg.next();	}
void cb_info_1d(Fl_Widget*,void*)	{	info_dlg.plot_1d();	}
void cb_info_2d(Fl_Widget*,void*)	{	info_dlg.plot_2d();	}
void cb_info_3d(Fl_Widget*,void*)	{	info_dlg.plot_3d();	}
//-----------------------------------------------------------------------------
class IconListDlg : public GeneralDlg
{
public:
	IconListDlg() : GeneralDlg()
	{
		Fl_Button* o;
		w = new Fl_Double_Window(210, 190, "mgllab icons");
		o = new Fl_Button(5, 5, 25, 25);	o->image(img_load);	o->tooltip("img_load");
		o = new Fl_Button(30, 5, 25, 25);	o->image(img_save);	o->tooltip("img_save");
		o = new Fl_Button(55, 5, 25, 25);	o->image(img_calc);	o->tooltip("img_calc");
		o = new Fl_Button(80, 5, 25, 25);	o->image(img_undo);	o->tooltip("img_undo");
		o = new Fl_Button(105, 5, 25, 25);	o->image(img_redo);	o->tooltip("img_redo");
		o = new Fl_Button(130, 5, 25, 25);	o->image(img_copy);	o->tooltip("img_copy");
		o = new Fl_Button(155, 5, 25, 25);	o->image(img_paste);o->tooltip("img_paste");
		o = new Fl_Button(180, 5, 25, 25);	o->image(img_find);	o->tooltip("img_find");
		o = new Fl_Button(5, 30, 25, 25);	o->image(img_prop);	o->tooltip("img_prop");
		o = new Fl_Button(30, 30, 25, 25);	o->image(img_alpha);o->tooltip("img_alpha");
		o = new Fl_Button(55, 30, 25, 25);	o->image(img_light);o->tooltip("img_light");
		o = new Fl_Button(80, 30, 25, 25);	o->image(img_grid);	o->tooltip("img_grid");
		o = new Fl_Button(105, 30, 25, 25);	o->image(img_move);	o->tooltip("img_move");
		o = new Fl_Button(130, 30, 25, 25);	o->image(img_orig);	o->tooltip("img_orig");
		o = new Fl_Button(155, 30, 25, 25);	o->image(img_update);	o->tooltip("img_update");
		o = new Fl_Button(180, 30, 25, 25);	o->image(img_stop);	o->tooltip("img_stop");
		o = new Fl_Button(5, 55, 25, 25);	o->image(img_insert);	o->tooltip("img_insert");
		o = new Fl_Button(30, 55, 25, 25);	o->image(img_print);o->tooltip("img_print");
		o = new Fl_Button(55, 55, 25, 25);	o->image(img_goL);	o->tooltip("img_goL");
		o = new Fl_Button(80, 55, 25, 25);	o->image(img_goU);	o->tooltip("img_goU");
		o = new Fl_Button(105, 55, 25, 25);	o->image(img_zoomIn);	o->tooltip("img_zoomIn");
		o = new Fl_Button(130, 55, 25, 25);	o->image(img_zoomOut);	o->tooltip("img_zoomOut");
		o = new Fl_Button(155, 55, 25, 25);	o->image(img_goD);	o->tooltip("img_goD");
		o = new Fl_Button(180, 55, 25, 25);	o->image(img_goR);	o->tooltip("img_goR");
		o = new Fl_Button(5, 80, 25, 25);	o->image(img_next);	o->tooltip("img_next");
		o = new Fl_Button(30, 80, 25, 25);	o->image(img_play);	o->tooltip("img_play");
		o = new Fl_Button(55, 80, 25, 25);	o->image(img_prev);	o->tooltip("img_prev");
		o = new Fl_Button(80, 80, 25, 25);	o->image(img_plot);	o->tooltip("img_plot");
		o = new Fl_Button(105, 80, 25, 25);	o->image(img_system);	o->tooltip("img_system");
		o = new Fl_Button(130, 80, 25, 25);	o->image(img_info);	o->tooltip("img_info");
		o = new Fl_Button(155, 80, 25, 25);	o->image(img_help);	o->tooltip("img_help");
		o = new Fl_Button(180, 80, 25, 25);	o->image(img_delete);	o->tooltip("img_delete");
		o = new Fl_Button(5, 105, 25, 25);	o->image(img_objectU);	o->tooltip("img_objectU");
		o = new Fl_Button(30, 105, 25, 25);	o->image(img_objectD);	o->tooltip("img_objectD");
		o = new Fl_Button(55, 105, 25, 25);	o->image(img_layer);o->tooltip("img_layer");
		o = new Fl_Button(80, 105, 25, 25);	o->image(img_new);	o->tooltip("img_new");
		o = new Fl_Button(105, 105, 25, 25);o->image(img_clear);o->tooltip("img_clear");
		o = new Fl_Button(130, 105, 25, 25);o->image(img_plus);	o->tooltip("img_plus");
		o = new Fl_Button(155, 105, 25, 25);o->image(img_minus);o->tooltip("img_minus");
		o = new Fl_Button(180, 105, 25, 25);o->image(img_fname);o->tooltip("img_fname");
		o = new Fl_Button(5, 130, 25, 25);	o->image(img_curve);o->tooltip("img_curve");
		o = new Fl_Button(30, 130, 25, 25);	o->image(img_svn);	o->tooltip("img_svn");
		o = new Fl_Button(55, 130, 25, 25);	o->image(img_adjust);	o->tooltip("img_adjust");
		o = new Fl_Button(80, 130, 25, 25);	o->image(img_reload);	o->tooltip("img_reload");
		o = new Fl_Button(105, 130, 25, 25);o->image(img_zoom12);	o->tooltip("img_zoom12");
		o = new Fl_Button(130, 130, 25, 25);o->image(img_zoom21);	o->tooltip("img_zoom21");
		o = new Fl_Button(155, 130, 25, 25);o->image(img_pause);	o->tooltip("img_pause");
//		o = new Fl_Button(180, 130, 25, 25);o->image(img_save);	o->tooltip("img_save");
		o = new Fl_Button(130, 160, 75, 25, _("Close"));
		o->callback(cb_dlg_cancel,this);
		w->end();
	}
} iconlist;
void iconlist_cb(Fl_Widget*,void*)	{	iconlist.show();	}
//-----------------------------------------------------------------------------
