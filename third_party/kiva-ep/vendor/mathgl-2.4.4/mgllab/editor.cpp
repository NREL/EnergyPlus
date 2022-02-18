/* editor.cpp is part of UDAV
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
#include <ctype.h>
#include <errno.h>
#ifdef __MWERKS__
# define FL_DLL
#endif
#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif
#include "mgllab.h"
//-----------------------------------------------------------------------------
int changed = 0;
std::string filename;
Fl_Text_Buffer *textbuf = 0;
//-----------------------------------------------------------------------------
// Syntax highlighting
Fl_Text_Buffer	 *stylebuf = 0;
Fl_Text_Display::Style_Table_Entry styletable[10] = {	// Style table
		{ FL_BLACK,		FL_COURIER,		14, 0 },		// A - Plain
		{ FL_DARK_GREEN,FL_COURIER_ITALIC,	14, 0 },	// B - Line comments
		{ FL_BLUE,		FL_COURIER,		14, 0 },		// C - Number
		{ FL_RED,		FL_COURIER,		14, 0 },		// D - Strings
		{ FL_DARK_BLUE,	FL_COURIER,		14, 0 },		// E - Usual command
		{ FL_DARK_CYAN,	FL_COURIER,		14, 0 },		// F - Flow command
		{ FL_DARK_MAGENTA,	FL_COURIER,	14, 0 },		// G - New-data command
		{ FL_DARK_RED,	FL_COURIER,		14, 0 },		// H - Option
		{ FL_DARK_GREEN,FL_COURIER,		14, 0 },		// I - Inactive command
		{ FL_MAGENTA,	FL_COURIER,		14, 0 }			// J - Error line ???
	};
int font_kind;	///< Editor font kind
int font_size;	///< Editor font size
//-----------------------------------------------------------------------------
void set_style(int kind, int size)
{
	if(kind<0 || kind>2)	kind = 1;
	if(size<1)	size = 14;
	for(int i=0;i<10;i++)	// set font for styles
	{	styletable[i].size = size;	styletable[i].font = 4*kind;	}
	styletable[1].font = 4*kind+2;
	font_kind = kind;	font_size = size;
}
//-----------------------------------------------------------------------------
bool MGL_FUNC_PURE is_sfx(const char *s)	// suffix
{
	size_t i,n=strlen(s);
	for(i=0;i<n && s[i]>='a';i++);
	if(i==1 && s[0]=='a')	return true;
	if(i==2 && strchr("nmawsk",s[0]) && strchr("axyz",s[1]))	return true;
	if(i==3 && (!strncmp("fst",s,3) || !strncmp("lst",s,3) || !strncmp("max",s,3) ||
				!strncmp("min",s,3) || !strncmp("sum",s,3)))
		return true;
	if(i==3 && s[0]=='m' && strchr("xyz",s[1]) && strchr("fl",s[2]))	return true;
	return false;
//	char *t = new char[i+1];	memcpy(t,s,i*sizeof(char));	t[i]=0;
}
//-----------------------------------------------------------------------------
bool MGL_FUNC_PURE is_opt(const char *s)	// option
{
	const char *o[13]={"xrange","yrange","zrange","crange","alpha",
					"cut","value","meshnum","size","legend",
					"ambient","diffuse","light"};
	int l[13] = {6,6,6,6,5, 3,5,7,4,6, 7,7,5};
	for(size_t i=0;i<13;i++)	if(!strncmp(o[i],s,l[i]) && s[l[i]]<=' ')	return true;
	return false;
}
//-----------------------------------------------------------------------------
bool MGL_FUNC_PURE is_num(const char *s)	// number
{
	size_t n=strlen(s);
//	if(s[0]==':' && (s[1]<=' ' || s[1]==';'))	return true;
	if(s[0]<=' ' || s[0]==';' || s[0]==':')	return false;
	if(n>=2 && (s[2]<=' ' || s[2]==';' || s[2]==':'))
	{
		if(!strncmp("pi",s,2))	return true;
		if(!strncmp("on",s,2))	return true;
	}
	if(n>=3 && (s[3]<=' ' || s[3]==';' || s[3]==':'))
	{
		if(!strncmp("off",s,3))	return true;
		if(!strncmp("nan",s,3))	return true;
		if(!strncmp("inf",s,3))	return true;
		if(!strncmp("all",s,3))	return true;
	}
	for(size_t i=0;i<n;i++)
	{
		if(s[i]<=' ' || s[i]==';' || s[i]==':')	break;
		if(!strchr("+-.eE0123456789",s[i]))	return false;
	}
	return true;
//	char *t = new char[i+1];	memcpy(t,s,i*sizeof(char));	t[i]=0;
}
//-----------------------------------------------------------------------------
char is_cmd(const char *s)	// command
{
	long i,n=strlen(s)+1;
	char res=0, *w=new char[n];	strcpy(w,s);
	for(i=0;i<n;i++)	if(!isalnum(s[i]))	w[i]=0;
	int rts = Parse->CmdType(w);
	if(rts==5)		res = 'G';
	else if(rts==7)	res = 'F';
	else if(rts)	res = 'E';
	delete []w;		return res;
}
//-----------------------------------------------------------------------------
// Parse text and produce style data.
void style_parse(const char *text, char *style, int /*length*/)
{
	size_t n=strlen(text);
	bool nl=true;
	// Style letters:
	// A - Plain
	// B - Line comments
	// C - Number
	// D - Strings
	// E - Usual command
	// F - Flow command
	// G - New data command
	// H - Option

	for(size_t i=0;i<n;i++)
	{
		char ch = text[i], r;	style[i] = 'A';
		if(ch=='#')	// comment
			for(;i<n && text[i]!='\n';i++)	style[i]='B';
		else if(ch=='\'')	// string
		{
			style[i]='D';	i++;
			for(;i<n && text[i]!='\n' && text[i]!='\'';i++)	style[i]='D';
			style[i]='D';
		}
		else if(ch=='\n' || ch==':')	{	nl=true;	continue;	}
		else if(nl && (r=is_cmd(text+i)) )	// command name
		{	for(;i<n && isalnum(text[i]);i++)	style[i]=r;	i--;	}
		else if(!nl && is_opt(text+i))	// option
		{	for(;i<n && isalpha(text[i]);i++)	style[i]='H';	i--;	}
		else if(!nl && is_num(text+i))	// number
		{
			for(;i<n && strchr("+-.eE0123456789pionaf",text[i]);i++)	style[i]='C';
			i--;
		}
		else if(ch=='.' && is_sfx(text+i+1))	// option (suffix)
		{
			style[i]='H';	i++;
			for(;i<n && isalpha(text[i]);i++)	style[i]='H';
		}
		else if(ch=='.' || (ch>='0' && ch<='9'))
		{	style[i]='C';	if(text[i+1]=='e' || text[i+1]=='E')	style[i+1]='C';	}
		nl = false;
	}
}
//-----------------------------------------------------------------------------
// Initialize the style buffer
void style_init()
{
	long len = textbuf->length();
	char *style = new char[len + 1];
	char *text = textbuf->text();
	memset(style, 'A', len);	style[len] = '\0';
	if(!stylebuf)	stylebuf = new Fl_Text_Buffer(len);
	style_parse(text, style, len);
	stylebuf->text(style);
	delete []style;	free(text);
}
//-----------------------------------------------------------------------------
// Update unfinished styles.
void style_unfinished_cb(int, void*) {}
//-----------------------------------------------------------------------------
// Update the style buffer...
void style_update(int pos, int nInserted, int nDeleted, int	/*nRestyled*/, const char */*deletedText*/, void *cbArg)
{
	long start, end;	// Start and end of text
	char last,		// Last style on line
		*style,		// Style data
		*text;		// Text data

	// If this is just a selection change, just unselect the style buffer...
	if (nInserted == 0 && nDeleted == 0) {	stylebuf->unselect();	return;  }
	// Track changes in the text buffer...
	if (nInserted > 0)
	{
		// Insert characters into the style buffer...
		style = new char[nInserted + 1];
		memset(style, 'A', nInserted);
		style[nInserted] = '\0';

		stylebuf->replace(pos, pos + nDeleted, style);
		delete[] style;
	}
	else	// Just delete characters in the style buffer...
		stylebuf->remove(pos, pos + nDeleted);
	// Select the area that was just updated to avoid unnecessary callbacks...
	stylebuf->select(pos, pos + nInserted - nDeleted);
	// Re-parse the changed region; we do this by parsing from the
	// beginning of the previous line of the changed region to the end of
	// the line of the changed region...  Then we check the last
	// style character and keep updating if we have a multi-line
	// comment character...
	start = textbuf->line_start(pos);
	end   = textbuf->line_end(pos + nInserted);
	text  = textbuf->text_range(start, end);
	style = stylebuf->text_range(start, end);
	if (start==end)	last = 0;
	else	last = style[end-start-1];
	style_parse(text, style, end - start);
	stylebuf->replace(start, end, style);
	((Fl_Text_Editor *)cbArg)->redisplay_range(start, end);

	if (start==end || last != style[end-start-1])
	{
		// Either the user deleted some text, or the last character on
		// the line changed styles, so reparse the remainder of the buffer...
		free(text);	free(style);
		end   = textbuf->length();
		text  = textbuf->text_range(start, end);
		style = stylebuf->text_range(start, end);
		style_parse(text, style, end - start);
		stylebuf->replace(start, end, style);
		((Fl_Text_Editor *)cbArg)->redisplay_range(start, end);
	}
	free(text);	free(style);
}
//-----------------------------------------------------------------------------
ScriptWindow::ScriptWindow(int w, int h, const char* t) : Fl_Double_Window(w, h, t)
{	editor = 0;	}
//-----------------------------------------------------------------------------
void set_path(char *buf)
{
#ifdef WIN32
	char sep='\\';
#else
	char sep='/';
#endif
	for(long i=strlen(buf)-1;i>=0;i--)	if(buf[i]==sep)
	{	buf[i]=0;	break;	}
	if(!chdir(buf))	printf("chdir to '%s'\n",buf);
}
//-----------------------------------------------------------------------------
void add_filename(const char *fname, ScriptWindow *e)
{
	static char buf[FL_PATH_MAX];
	fl_filename_absolute(buf, FL_PATH_MAX, fname);	fname=buf;
	if(!fname || !fname[0] || lastfiles[0]==fname)
	{	set_path(buf);	return;	}
	pref.set("last_file",fname);
	int ii=4;
	for(int i=1;i<5;i++)
		if(lastfiles[i]==fname)	{	ii=i;	break;	}
	for(int i=ii;i>0;i--)	lastfiles[i]=lastfiles[i-1];
	lastfiles[0]=fname;
	int ir = e->menu->find_index(_("File/Recent files"));
	if(ir<0)	ir = 6;
	e->menu->replace(ir+1, lastfiles[0].c_str());
	e->menu->replace(ir+2, lastfiles[1].c_str());
	e->menu->replace(ir+3, lastfiles[2].c_str());
	e->menu->replace(ir+4, lastfiles[3].c_str());
	e->menu->replace(ir+5, lastfiles[4].c_str());
	set_path(buf);	save_pref();
}
//-----------------------------------------------------------------------------
int check_save(void)
{
  if (!changed) return 1;
  int r = fl_choice(_("The current file has not been saved.\n"
					"Would you like to save it now?"),
					_("Cancel"), _("Save"), _("Don't Save"));
  if(r==1)	{	save_cb(0,0);	return !changed;	} // Save the file...
  return (r==2) ? 1 : 0;
}
//-----------------------------------------------------------------------------
void data_file(const char *fn)
{
	static int num=0;
	static char name[32], res[256];
	snprintf(name,32,"mgl_%d",num);	num++;
	mglDataA *v = Parse->AddVar(name);
	mglData *d = dynamic_cast<mglData*>(v);
	mglDataC *c = dynamic_cast<mglDataC*>(v);
	if(d)
	{
		d->Read(fn);
		if(d->nz>1)
			snprintf(res,256,"#read %s '%s'\nrotate 40 60\ncrange %s\nbox\nsurf3 %s\n", name, fn, name, name);
		else if(d->ny>1)
			snprintf(res,256,"#read %s '%s'\nrotate 40 60\ncrange %s\nzrange %s\nbox\nsurf %s\n", name, fn, name, name, name);
		else
			snprintf(res,256,"#read %s '%s'\nyrange %s\nbox\nplot %s\n", name, fn, name, name);
		textbuf->text(res);
	}
	else if(c)
	{
		c->Read(fn);
		if(c->nz>1)
			snprintf(res,256,"#read %s '%s'\nrotate 40 60\ncrange %s\nbox\nsurf3 %s\n", name, fn, name, name);
		else if(c->ny>1)
			snprintf(res,256,"#read %s '%s'\nrotate 40 60\ncrange %s\nzrange %s\nbox\nsurf %s\n", name, fn, name, name, name);
		else
			snprintf(res,256,"#read %s '%s'\nyrange %s\nbox\nplot %s\n", name, fn, name, name);
		textbuf->text(res);
	}
}
//-----------------------------------------------------------------------------
int loading = 0;
void load_file(const char *newfile, int ipos, ScriptWindow *e)
{
	long len = strlen(newfile);
	if(ipos==-1 && (!strcmp(newfile+len-4,".dat") || !strcmp(newfile+len-4,".csv")))
	{
		data_file(newfile);
		filename = newfile;	filename += ".mgl";
		add_filename(filename.c_str(),e);
	}
	else
	{
		loading = 1;
		int insert = (ipos != -1);
		changed = insert;
		if(!insert) filename="";
		long r;
		if(!insert)	r = textbuf->loadfile(newfile);
		else r = textbuf->insertfile(newfile, ipos);

		char *t = textbuf->text();
#ifndef WIN32
		size_t i,l=strlen(t);
		for(i=0;i<l;i++)	if(t[i]=='\r')	t[i]=' ';
		textbuf->text(t);
#endif
		fill_animate(t, e->draw);	free(t);

		if (r)
			fl_alert(_("Error reading from file \'%s\':\n%s."), newfile, strerror(errno));
		else	if(!insert)
		{	filename = newfile;	add_filename(filename.c_str(),e);	}
		loading = 0;
		textbuf->call_modify_callbacks();
	}
}
//-----------------------------------------------------------------------------
void save_file(const char *newfile, ScriptWindow *e)
{
	if (textbuf->savefile(newfile))
		fl_alert(_("Error writing to file \'%s\':\n%s."), newfile, strerror(errno));
	else
	{
		filename = newfile;	add_filename(filename.c_str(),e);
		changed = 0;	textbuf->call_modify_callbacks();
	}
}
//-----------------------------------------------------------------------------
void undo_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	Fl_Text_Editor::kf_undo(0, e->editor);
}
//-----------------------------------------------------------------------------
void select_all_cb(Fl_Widget *, void *v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	Fl_Text_Editor::kf_select_all(0, e->editor);
}
//-----------------------------------------------------------------------------
void copy_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	Fl_Text_Editor::kf_copy(0, e->editor);
}
//-----------------------------------------------------------------------------
void cut_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	Fl_Text_Editor::kf_cut(0, e->editor);
}
//-----------------------------------------------------------------------------
void hide_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	int p1,p2;
	textbuf->selection_position(&p1, &p2);
	if(!textbuf->selected())	p2=p1=e->editor->insert_position();
	p1 = textbuf->line_start(p1);
	while(p1<p2)
	{
		textbuf->insert(p1,"#h ");
		int p = textbuf->line_start(textbuf->line_end(p1)+1);
		if(p!=p1)	p1=p;	else	return;
	}
}
//-----------------------------------------------------------------------------
void unhide_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	int p1,p2;
	textbuf->selection_position(&p1, &p2);
	if(!textbuf->selected())	p2=p1=e->editor->insert_position();
	p1 = textbuf->line_start(p1);
	while(p1<p2)
	{
		if(textbuf->char_at(p1)=='#')
		{
			if(textbuf->char_at(p1+1)=='h' && textbuf->char_at(p1+2)==' ')
				textbuf->remove(p1,p1+3);
			else	textbuf->remove(p1,p1+1);
		}
		int p = textbuf->line_start(textbuf->line_end(p1)+1);
		if(p!=p1)	p1=p;	else	return;
	}
}
//-----------------------------------------------------------------------------
void delete_cb(Fl_Widget*, void*) {	textbuf->remove_selection();	}
//-----------------------------------------------------------------------------
void cb_descr(Fl_Widget*,void *v)
{
	static size_t len=0;
	ScriptWindow *w = (ScriptWindow*)v;
	if(!textbuf || !Parse || !w)	return;
	int cur = w->editor->insert_position(), br=0;
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
	const char *desc = Parse->CmdDesc(cmd.c_str());
	const char *form = Parse->CmdFormat(cmd.c_str());
	static std::string txt;
	txt = desc?std::string(desc)+":  "+form : "";
	w->set_status(txt.c_str());

	size_t ll = strlen(s);
	if(complete_word && br==cur+1 && br-beg>2 && len<ll)	// try complete word
	{
		long n = Parse->GetCmdNum();
		std::vector<std::string> vars;
		for(long i=0;i<n;i++)
		{
			const char *c = Parse->GetCmdName(i);
			if(!strncmp(c,cmd.c_str(),cmd.length()))	vars.push_back(c);
		}
		for(size_t i=0;i<vars.size();i++)
			if(vars[i].length()>cmd.length())
			{
				std::string suggest = vars[i].substr(cmd.length());
				textbuf->insert(cur+1, suggest.c_str());
				textbuf->select(cur+1, cur+suggest.length()+1);
				break;
			}
	}
	len = ll;
}
//-----------------------------------------------------------------------------
void ScriptWindow::set_status(const char *txt)
{	if(txt && status)	{	status->label(txt);	redraw();	}	}
//-----------------------------------------------------------------------------
void changed_cb(int pos, int nInserted, int nDeleted, int nRestyled, const char *deletedText, void* v)
{
	if ((nInserted || nDeleted) && !loading) changed = 1;
	ScriptWindow *w = (ScriptWindow *)v;
	cb_descr(0,v);
	set_title(w);
	style_update(pos, nInserted, nDeleted, nRestyled, deletedText, w->editor);
	if (loading) w->editor->show_insert_position();
}
//-----------------------------------------------------------------------------
void insert_cb(Fl_Widget*, void *v)
{
	const char *newfile = mgl_file_chooser(_("Insert file content?"));
	ScriptWindow *w = (ScriptWindow *)v;
	if (newfile != NULL) load_file(newfile, w->editor->insert_position(),w);
}
//-----------------------------------------------------------------------------
void paste_cb(Fl_Widget*, void* v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	Fl_Text_Editor::kf_paste(0, e->editor);
}
//-----------------------------------------------------------------------------
#include "../widgets/image.h"
#include "xpm/box.xpm"
Fl_Widget *add_editor(ScriptWindow *w, int txtW, int wndH)
{
	Fl_Window *w1=new Fl_Window(0,30,txtW,wndH-55,0);
	Fl_Group *g = new Fl_Group(0,0,txtW-10,30);
	Fl_Button *o;

	o = new Fl_Button(0, 1, 25, 25);	o->image(img_load);	o->callback(open_cb,w);
	o->tooltip(_("Open script or data file"));
	o = new Fl_Button(25, 1, 25, 25);	o->image(img_save);	o->callback(save_cb,w);
	o->tooltip(_("Save script to file"));

	o = new Fl_Button(55, 1, 25, 25);	o->image(img_copy);	o->callback(copy_cb,w);
	o->tooltip(_("Copy selection to clipboard"));
	o = new Fl_Button(80, 1, 25, 25);	o->image(img_paste);o->callback(paste_cb,w);
	o->tooltip(_("Paste text from clipboard"));
	o = new Fl_Button(105, 1, 25, 25);	o->image(img_find);	o->callback(find_dlg_cb,w);
	o->tooltip(_("Find or replace text"));

	o = new Fl_Button(135, 1, 25, 25);	o->image(img_insert);	o->callback(newcmd_dlg_cb,w);
	o->tooltip(_("Insert MGL command"));
	o = new Fl_Button(160, 1, 25, 25);	o->image(img_fname);	o->callback(ins_fname_cb,w);
	o->tooltip(_("Insert filename"));
	o = new Fl_Button(185, 1, 25, 25);	o->image(new Fl_Pixmap(box_xpm));	o->callback(inplot_dlg_cb,w);
	o->tooltip(_("Insert inplot command"));

	o = new Fl_Button(210, 1, 25, 25);	o->image(img_calc);	o->callback(calc_dlg_cb,w);
	o->tooltip(_("Show calculator window"));
	o = new Fl_Button(240, 1, 25, 25);	o->image(img_curve);o->callback(prim_dlg_cb,w);
	o->tooltip(_("Show calculator window"));
	g->end();	g->resizable(0);

	w->editor = new Fl_Text_Editor(0, 28, txtW, wndH-85);
	w->editor->textfont(FL_COURIER);
	w->editor->buffer(textbuf);
	w->editor->highlight_data(stylebuf, styletable, sizeof(styletable) / sizeof(styletable[0]), 'A', style_unfinished_cb, 0);
#if MGL_HAVE_FL_COPY
	w->editor->linenumber_width(30);
#endif
//	w->editor->when(FL_WHEN_RELEASE_ALWAYS);	w->editor->callback(cb_descr,w);

	textbuf->add_modify_callback(changed_cb, w);
	textbuf->call_modify_callbacks();

	w1->end();	w1->resizable(w->editor);
	return w1;
}
//-----------------------------------------------------------------------------
void cp_find_next(Fl_Widget*,void*);
void cp_repl_next(Fl_Widget*,void*);
void cp_repl_all(Fl_Widget*,void*);
class FindDlg : public GeneralDlg
{
	Fl_Input *find, *replace;
	Fl_Check_Button *mcase, *sback;
public:
	FindDlg() : GeneralDlg()
	{
		Fl_Button* o;
		w = new Fl_Double_Window(375, 130, _("Find/Replace"));
		find = new Fl_Input(90, 10, 180, 25, _("Find what:"));
		o = new Fl_Return_Button(275, 10, 95, 25, _("Find"));	o->callback(cp_find_next);
		replace = new Fl_Input(90, 40, 180, 25, _("Replace by:"));
		o = new Fl_Button(275, 40, 95, 25, _("Replace"));		o->callback(cp_repl_next);
		mcase = new Fl_Check_Button(5, 70, 265, 25, _("Match case"));
		sback = new Fl_Check_Button(5, 95, 265, 25, _("Search backward"));
		o = new Fl_Button(275, 70, 95, 25, _("Replace all"));	o->callback(cp_repl_all);
		o = new Fl_Button(275, 100, 95, 25, _("Close"));	o->callback(cb_dlg_cancel,this);
		w->end();
	}
	const char *to_find()	{	return find->value();	}
	void find_next()
	{
		const char *s = find->value();
		int c = mcase->value(), b = sback->value();
		if(s && *s)
		{
			int pos = e->editor->insert_position();
			int found = b ? textbuf->search_backward(pos,s,&pos,c) : textbuf->search_forward(pos,s,&pos,c);
			if(found)
			{	// Found a match; select and update the position...
				size_t len = strlen(s);
				textbuf->select(pos, pos+len);
				e->editor->insert_position(pos+len);
				e->editor->show_insert_position();
			}
			else fl_alert(_("No occurrences of \'%s\' found!"), s);
		}
	}
	void repl_next()
	{
		const char *s = find->value();
		const char *r = replace->value();
		int c = mcase->value(), b = sback->value();
		if(s && *s)
		{
			int pos = e->editor->insert_position();
			int found = b ? textbuf->search_backward(pos,s,&pos,c) : textbuf->search_forward(pos,s,&pos,c);
			if(found)
			{	// Found a match; select and update the position...
				size_t len = strlen(r);
				textbuf->select(pos, pos+strlen(s));
				textbuf->remove_selection();
				textbuf->insert(pos, r);
				textbuf->select(pos, pos+len);
				e->editor->insert_position(pos+len);
				e->editor->show_insert_position();
			}
			else fl_alert(_("No occurrences of \'%s\' found!"), s);
		}
	}
	void repl_all()
	{
		const char *s = find->value();
		const char *r = replace->value();
		int c = mcase->value(), b = sback->value();
		int found = (s && *s)?1:0;
		long num=0;
		while(found)
		{
			int pos = e->editor->insert_position();
			int found = b ? textbuf->search_backward(pos,s,&pos,c) : textbuf->search_forward(pos,s,&pos,c);
			if(!found)	break;
			size_t len = strlen(r);
			textbuf->select(pos, pos+strlen(s));
			textbuf->remove_selection();
			textbuf->insert(pos, r);
			textbuf->select(pos, pos+len);
			e->editor->insert_position(pos+len);
			e->editor->show_insert_position();
			num++;
		}
		if(num) fl_message(_("Replaced %ld occurrences."), num);
		else fl_alert(_("No occurrences of \'%s\' found!"), s);
	}
} find_dlg;
//-----------------------------------------------------------------------------
void cp_find_next(Fl_Widget*,void*)	{	find_dlg.find_next();	}
void cp_repl_next(Fl_Widget*,void*)	{	find_dlg.repl_next();	}
void cp_repl_all(Fl_Widget*,void*)	{	find_dlg.repl_all();	}
//-----------------------------------------------------------------------------
void find_dlg_cb(Fl_Widget*,void *v)
{	find_dlg.e = (ScriptWindow*)v;	find_dlg.show();	}
//-----------------------------------------------------------------------------
void find_next_cb(Fl_Widget*,void *v)
{
	find_dlg.e = (ScriptWindow*)v;
	const char *s = find_dlg.to_find();
	if(s && *s)	find_dlg.find_next();
	else	find_dlg.show();
}
//-----------------------------------------------------------------------------
void ins_fname_cb(Fl_Widget *, void *v)
{	// TODO: use previous file name?!?
	ScriptWindow* e = (ScriptWindow*)v;
	const char *s = mgl_file_chooser(_("Select file name"), "DAT files \t*.{dat,csv}\nHDF files \t*.{hdf,h5}\nImage files \t*.{png,jpg,jpeg}");
	if(s)
	{
		std::string ss=s;	ss = '\''+ss+'\'';
		if(e)	e->editor->insert(ss.c_str());
		else	cb_args_set(ss.c_str());
	}
}
//-----------------------------------------------------------------------------
void ins_path_cb(Fl_Widget *, void *v)
{
	static std::string prev;
	ScriptWindow* e = (ScriptWindow*)v;
	const char *s = mgl_dir_chooser(_("Select folder name"), prev.c_str());
	if(s)
	{
		std::string ss=prev=s;	ss = '\''+ss+'\'';
		if(e)	e->editor->insert(ss.c_str());
		else	cb_args_set(ss.c_str());
	}
}
//-----------------------------------------------------------------------------
void ins_fits_cb(Fl_Widget *, void *v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	HMGL gr = e->graph->get_graph();
	std::string ss=mgl_get_fit(gr);
	if(ss.empty())	fl_alert(_("There is no fitted formula."));
	else	{	ss = '\''+ss+'\'';	e->editor->insert(ss.c_str());	}
}
//-----------------------------------------------------------------------------
void ins_prim_cb(Fl_Widget *, void *v)
{
	ScriptWindow* e = (ScriptWindow*)v;
	std::string ss = "subplot 1 1 0 '#'\n"+e->graph->FMGL->prim+"subplot 1 1 0\n###### end of primitives\n";
	e->editor->insert(ss.c_str());
	e->graph->FMGL->prim.clear();
}
//-----------------------------------------------------------------------------
