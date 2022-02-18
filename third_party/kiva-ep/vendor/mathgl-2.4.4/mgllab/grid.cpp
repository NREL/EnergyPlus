/* grid.cpp is part of UDAV
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
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Int_Input.H>
#include <FL/Fl_Value_Slider.H>
#include <FL/fl_draw.H>
#include "mgllab.h"
//-----------------------------------------------------------------------------
Fl_Callback input_cb;
//-----------------------------------------------------------------------------
void input_cb(Fl_Widget*, void* v)	{ ((Fl_Data_Table*)v)->set_value(); }
//-----------------------------------------------------------------------------
Fl_Data_Table::Fl_Data_Table(int x, int y, int w, int h, const char *l) : Fl_Table(x,y,w,h,l)
{
	callback(&event_callback, (void*)this);
	input = new Fl_Input(w/2,h/2,0,0);
	input->hide();
	input->callback(input_cb, (void*)this);
	input->when(FL_WHEN_ENTER_KEY_ALWAYS);
	input->maximum_size(16);
	nx=ny=sl = 0;	row=col=-1;	data = NULL;
//	(new Fl_Box(9999,9999,0,0))->hide();  // HACK: prevent flickering in Fl_Scroll
	end();
}
//-----------------------------------------------------------------------------
// Handle drawing all cells in table
void Fl_Data_Table::draw_cell(TableContext context, int R, int C, int X, int Y, int W, int H)
{
	static char s[64];
	fl_push_clip(X, Y, W, H);
	switch ( context )
	{
	case CONTEXT_COL_HEADER:
		fl_draw_box(FL_THIN_UP_BOX, X, Y, W, H, col_header_color());
		fl_font(FL_HELVETICA | FL_BOLD, 14);
		fl_color(FL_BLACK);		snprintf(s,32,"%d",C);
		fl_draw(s, X, Y, W, H, FL_ALIGN_CENTER);
		break;
	case CONTEXT_ROW_HEADER:
		fl_draw_box(FL_THIN_UP_BOX, X, Y, W, H, col_header_color());
		fl_font(FL_HELVETICA | FL_BOLD, 14);
		fl_color(FL_BLACK);		snprintf(s,32,"%d",R);
		fl_draw(s, X, Y, W, H, FL_ALIGN_CENTER);
		break;
	case CONTEXT_CELL:
	    if (R == row && C == col && input->visible())	break;
		fl_draw_box(FL_THIN_DOWN_BOX, X, Y, W, H, FL_WHITE);
		fl_pop_clip();
		fl_push_clip(X+3, Y+3, W-6, H-6);
		fl_font(FL_HELVETICA, 14);
		if(mgl_isnan(data->v(C,R,sl)))	strcpy(s,"nan");
		else if(mgl_isbad(data->v(C,R,sl)))	strcpy(s,data->v(C,R,sl)>0?"inf":"-inf");
		else	mgl_strncpy(s,mgl_str_num(data->vc(C,R,sl)).c_str(),64);
		{	dual vc = data->vc(C,R,sl);
			mreal v = data->v(C,R,sl);
			std::vector<mreal> vn;
			if(C>0)	vn.push_back(data->v(C-1,R,sl));
			if(R>0)	vn.push_back(data->v(C,R-1,sl));
			if(C<data->GetNx()-1)	vn.push_back(data->v(C+1,R,sl));
			if(R<data->GetNy()-1)	vn.push_back(data->v(C,R+1,sl));
			bool v1=true, v2=true;
			for(size_t i=0;i<vn.size();i++)	{	if(vn[i]<=v)	v1=false;	if(vn[i]>=v)	v2=false;	}
			if(v2)	fl_color(FL_MAGENTA);
			else if(v1)	fl_color(FL_CYAN);
			else if(real(vc)>0)		fl_color(FL_RED);
			else if(real(vc)<0)	fl_color(FL_BLUE);
			else if(imag(vc)>0)	fl_color(FL_DARK_MAGENTA);
			else if(imag(vc)<0)	fl_color(FL_DARK_CYAN);
			else	fl_color(FL_BLACK);	}
		fl_draw(s, X+3, Y+3, W-6, H-6, FL_ALIGN_RIGHT);
		break;
	case CONTEXT_RC_RESIZE:
		if (!input->visible()) break;
		find_cell(CONTEXT_TABLE, row, col, X, Y, W, H);
		if (X!=input->x() || Y!=input->y() || W!=input->w() || H!=input->h())
			input->resize(X,Y,W,H);
		break;
	default:	break;
	}
	fl_pop_clip();
}
//-----------------------------------------------------------------------------
void Fl_Data_Table::cell_click()
{
    int R = callback_row(), C = callback_col();
    TableContext context = callback_context();

    if(context==CONTEXT_CELL)
	{
		if (input->visible())	set_value();
		row = R;	col = C;
		int XX,YY,WW,HH;
		find_cell(CONTEXT_CELL, R, C, XX, YY, WW, HH);
		input->resize(XX,YY,WW,HH);
		std::string s;
		if(mgl_isnan(data->v(C,R,sl)))	s = "nan";
		else if(mgl_isbad(data->v(C,R,sl)))	s = data->v(C,R,sl)>0?"inf":"-inf";
		else	s = mgl_str_num(data->vc(C,R,sl));
		input->value(s.c_str());	input->show();
		input->take_focus();
	}
}
//-----------------------------------------------------------------------------
void Fl_Data_Table::set_value()
{
	const char *s = input->value();
	if(s[0]==0 || !strcmp(s,"nan"))	data->set_v(NAN, col,row,sl);
	else if(!strcmp(s,"inf"))	data->set_v(INFINITY, col,row,sl);
	else if(!strcmp(s,"-inf"))	data->set_v(-INFINITY, col,row,sl);
	else
	{
		dual v = mgl_atoc(s,true);
		if(imag(v)==0)	data->set_v(real(v), col,row,sl);
		else
		{
			HADT c = dynamic_cast<HADT>(data);
			if(c)	c->a[col+c->nx*(row+c->ny*sl)] = v;
			else	data->set_v(abs(v),col,row,sl);
		}
	}
}
//-----------------------------------------------------------------------------
