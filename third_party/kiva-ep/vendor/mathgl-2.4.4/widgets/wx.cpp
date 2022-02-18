/***************************************************************************
 * wx.cpp is part of Math Graphic Library                              *
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
//-----------------------------------------------------------------------------
#include <wx/dcclient.h>
#include <wx/msgdlg.h>
#include <wx/clipbrd.h>
#include <wx/dataobj.h>
#include <wx/menu.h>
#include <wx/scrolwin.h>

#undef _

#include "mgl2/canvas_wnd.h"
#include "mgl2/wx.h"
//-----------------------------------------------------------------------------
class MGL_EXPORT mglCanvasWX : public mglCanvasWnd
{
friend class wxMathGL;
public:
	int sshow;			///< Current state of animation switch (toggle button)
	wxMathGL *WMGL;		///< Control which draw graphics
	wxWindow *Wnd;		///< Pointer to window

	mglCanvasWX();
	virtual ~mglCanvasWX();

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

protected:
	wxScrolledWindow *scroll;	///< Scrolling area
	wxMenu *popup;			///< Popup menu
//	wxSpinCtrl *tet, *phi;	///< Spin box for angles // TODO

	void MakeMenu();		///< Create menu, toolbar and popup menu
};
//-----------------------------------------------------------------------------
const wxString ScriptName(L"default");
enum
{
	TIMER_ID=1000,
	LAST_ID
};
BEGIN_EVENT_TABLE(wxMathGL, wxWindow)
	EVT_TIMER	(TIMER_ID,	wxMathGL::OnNextSlide)
	EVT_PAINT	(wxMathGL::OnPaint)
	EVT_SIZE	(wxMathGL::OnSize)
	EVT_LEFT_DOWN	(wxMathGL::OnMouseLeftDown)
	EVT_RIGHT_DOWN	(wxMathGL::OnMouseDown)
	EVT_MIDDLE_DOWN	(wxMathGL::OnMouseDown)
	EVT_LEFT_UP		(wxMathGL::OnMouseLeftUp)
	EVT_RIGHT_UP	(wxMathGL::OnMouseRightUp)
	EVT_MOTION		(wxMathGL::OnMouseMove)
END_EVENT_TABLE()
//-----------------------------------------------------------------------------
//
//		class wxMathGL
//
//-----------------------------------------------------------------------------
wxMathGL::wxMathGL(wxWindow *parent, wxWindowID id, const wxPoint& pos, const wxSize& size, long style, const wxString& name) : wxWindow(parent,id,pos,size,style,name)
{
	AutoResize = false;	draw_par = 0;	draw_func = 0;
	gr = new mglCanvas;	popup = 0;		draw_cl = 0;
	phi = tet = per = 0;	x0=y0=xe=ye=0;
	x1 = y1 = 0;	x2 = y2 = 1;
	alpha = light = zoom = rotate = false;
//	SetSize(600, 400);
	timer = new wxTimer(this,TIMER_ID);
}
//-----------------------------------------------------------------------------
wxMathGL::~wxMathGL()	{	if(mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);	}
//-----------------------------------------------------------------------------
double wxMathGL::GetRatio()	{	return double(mgl_get_width(gr))/mgl_get_height(gr);	}
//-----------------------------------------------------------------------------
void wxMathGL::SetGraph(HMGL GR)
{
	mglCanvas *gg = dynamic_cast<mglCanvas *>(GR);
	if(!gg)	return;
	if(mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);
	gr=gg;	mgl_use_graph(gg,1);
}
//-----------------------------------------------------------------------------
void wxMathGL::OnPaint(wxPaintEvent& )
{
	wxPaintDC dc(this);
	dc.DrawBitmap(pic,0,0);
//	if(zoom)	dc.DrawRectangle(x0,y0,xe-x0,ye-y0);
	if(mgl_get_flag(gr,MGL_SHOW_POS) && !MousePos.IsEmpty())
		dc.DrawText(MousePos,0,12);
	// TODO: add grid drawing here (from Qt)
	// TODO: add active points drawing here (from Qt)
}
//-----------------------------------------------------------------------------
void wxMathGL::OnSize(wxSizeEvent& event)
{
	wxSize ev = event.GetSize();
	if(mgl_get_width(gr)==ev.GetWidth() && mgl_get_height(gr)==ev.GetHeight())
		return;
	if(AutoResize && ev.GetWidth()>0 && ev.GetHeight()>0)
	{	mgl_set_size(gr, ev.GetWidth(), ev.GetHeight());	Update();	}
	else 	SetSize(mgl_get_width(gr), mgl_get_height(gr));
}
//-----------------------------------------------------------------------------
void wxMathGL::OnNextSlide(wxTimerEvent& )	{	NextSlide();	}
//-----------------------------------------------------------------------------
void wxMathGL::SetPer(int p)
{	if(100*per!=p && p>=0 && p<100)	{	per = 0.01*p;	Repaint();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetPhi(int p)
{	if(phi!=p)	{	phi = p; 	Repaint();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetTet(int t)
{	if(tet!=t)	{	tet = t; 	Repaint();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetAlpha(bool a)
{	if(alpha!=a)	{	alpha = a;	Update();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetLight(bool l)
{	if(light!=l)	{	light = l;	Update();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetZoom(bool z)
{	if(zoom!=z)	{	zoom=z;	rotate=false;	Repaint();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::SetRotate(bool r)
{	if(rotate!=r)	{	zoom=false;	rotate=r;	Repaint();	}	}
//-----------------------------------------------------------------------------
void wxMathGL::ShiftDown()
{	mreal d=(y2-y1)/3;	y1+=d;	y2+=d;	Repaint();	}
//-----------------------------------------------------------------------------
void wxMathGL::ShiftUp()
{	mreal d=(y2-y1)/3;	y1-=d;	y2-=d;	Repaint();	}
//-----------------------------------------------------------------------------
void wxMathGL::ShiftRight()
{	mreal d=(x2-x1)/3;	x1-=d;	x2-=d;	Repaint();	}
//-----------------------------------------------------------------------------
void wxMathGL::ShiftLeft()
{	mreal d=(x2-x1)/3;	x1+=d;	x2+=d;	Repaint();	}
//-----------------------------------------------------------------------------
void wxMathGL::Restore()
{
	SetPhi(0);	SetTet(0);	SetPer(0);
	x1=y1=0;	x2=y2=1;	zoom=rotate=false;
	Repaint();
}
//-----------------------------------------------------------------------------
void wxMathGL::ZoomIn()
{
	mreal d;
	d = (y2-y1)/4;	y1 += d;	y2 -= d;
	d = (x2-x1)/4;	x1 += d;	x2 -= d;
	Repaint();
}
//-----------------------------------------------------------------------------
void wxMathGL::ZoomOut()
{
	mreal d;
	d = (y2-y1)/2;	y1 -= d;	y2 += d;
	d = (x2-x1)/2;	x1 -= d;	x2 += d;
	Repaint();
}
//-----------------------------------------------------------------------------
void wxMathGL::Update()
{
	if(draw_func || draw_cl)
	{
		if(mgl_get_flag(gr,MGL_CLF_ON_UPD))	mgl_set_def_param(gr);
		mgl_set_def_param(gr);		mgl_reset_frames(gr);
		mgl_set_alpha(gr,alpha);	mgl_set_light(gr,light);
		if(draw_func)	draw_func(gr, draw_par);	// drawing itself
		else 	if(draw_cl)	{	mglGraph g(gr);	draw_cl->Draw(&g);	}
		const char *buf = mgl_get_mess(gr);
		if(*buf)
		{
			wxMessageDialog dlg(this, wxString(buf,wxConvLocal), appName, wxOK);
			dlg.ShowModal();
		}
	}
	else if(mgl_get_num_frame(gr)>0)
	{
		mgl_set_alpha(gr,alpha);	mgl_set_light(gr,light);
//		mgl_zoom(gr,x1,y1,x2,y2);	mgl_view(gr,-phi,-tet,0);
		mgl_get_frame(gr,0);
	}
	MousePos.Empty();	Repaint();
}
//-----------------------------------------------------------------------------
wxBitmap MGL_EXPORT ConvertFromGraph(HMGL gr)
{
	const unsigned char *bb = mgl_get_rgb(gr);
	int w=mgl_get_width(gr), h=mgl_get_height(gr);
	unsigned char *tmp = (unsigned char *)malloc(3*w*h);
	memcpy(tmp,bb,3*w*h);
	wxImage img(w, h);	img.SetData(tmp);
	return wxBitmap(img);
}
//-----------------------------------------------------------------------------
void wxMathGL::Repaint()
{
	mgl_zoom(gr,x1,y1,x2,y2);	mgl_view(gr,-phi,-tet,0);	mgl_ask_perspective(gr, per);
	pic = ConvertFromGraph(gr);
	wxSize sz=GetSize();
	if(pic.GetWidth()!=sz.GetWidth() || pic.GetHeight()!=sz.GetHeight())
		SetSize(pic.GetWidth(), pic.GetHeight());
	Refresh();
}
//-----------------------------------------------------------------------------
void wxMathGL::OnMouseLeftDown(wxMouseEvent &ev)
{
	long x=ev.GetX(), y=ev.GetY();
	if(!zoom && !rotate)
	{
		mglPoint p = gr->CalcXYZ(x, y);
		MousePos.Printf(wxT("x=%g, y=%g, z=%g"),p.x,p.y,p.z);
		Refresh();
//		emit mouseClick(p.x,p.y,p.z);
	}
	xe=x0=x;	ye=y0=y;	ev.Skip();
}
//-----------------------------------------------------------------------------
void wxMathGL::OnMouseDown(wxMouseEvent &ev)
{	xe=x0=ev.GetX();	ye=y0=ev.GetY();	ev.Skip();	}
//-----------------------------------------------------------------------------
void wxMathGL::OnMouseLeftUp(wxMouseEvent &)
{
	if(zoom)
	{
		int w1=GetSize().GetWidth(),h1=GetSize().GetHeight();
		mreal _x1,_x2,_y1,_y2;
		_x1 = x1+(x2-x1)*(x0-GetPosition().x)/mreal(w1);
		_y1 = y2-(y2-y1)*(ye-GetPosition().y)/mreal(h1);
		_x2 = x1+(x2-x1)*(xe-GetPosition().x)/mreal(w1);
		_y2 = y2-(y2-y1)*(y0-GetPosition().y)/mreal(h1);
		x1=_x1;		x2=_x2;		y1=_y1;		y2=_y2;
		if(x1>x2)	{	_x1=x1;	x1=x2;	x2=_x1;	}
		if(y1>y2)	{	_x1=y1;	y1=y2;	y2=_x1;	}
		x0 = xe;	y0 = ye;
		Update();
	}
}
//-----------------------------------------------------------------------------
void wxMathGL::OnMouseRightUp(wxMouseEvent &ev)
{	if(popup && !rotate)	PopupMenu(popup, ev.GetPosition());	}
//-----------------------------------------------------------------------------
void wxMathGL::OnMouseMove(wxMouseEvent &ev)
{
	long w=GetSize().GetWidth(), h=GetSize().GetHeight();
	xe=ev.GetX();	ye=ev.GetY();
	if(rotate)
	{
		if(ev.ButtonDown(wxMOUSE_BTN_LEFT))	// rotate
		{
			mreal ff = 240/sqrt(mreal(w*h));
			phi += int((x0-xe)*ff);
			tet += int((y0-ye)*ff);
			if(phi>180)		phi-=360;
			if(phi<-180)	phi+=360;
			if(tet>180)		tet-=360;
			if(tet<-180)	tet+=360;
//			Update();
		}
		if(ev.ButtonDown(wxMOUSE_BTN_RIGHT))	// zoom and perspective
		{
			mreal ff = 2.*(y0-ye)/w, gg = 0.5*(xe-x0)/h;
			mreal cx = (x1+x2)/2, cy = (y1+y2)/2;
			x1 = cx+(x1-cx)*exp(-ff);	x2 = cx+(x2-cx)*exp(-ff);
			y1 = cy+(y1-cy)*exp(-ff);	y2 = cy+(y2-cy)*exp(-ff);
			per = per + gg;
			if(per<0)	per = 0;
			if(per>=1)	per = 0.9999;
//			Update();
		}
		if(ev.ButtonDown(wxMOUSE_BTN_MIDDLE))	// shift
		{
			mreal ff = 1./sqrt(mreal(w*h));
			mreal dx = (x0-xe)*ff*(x2-x1), dy = (y0-ye)*ff*(y2-y1);
			x1 += dx;	x2 += dx;	y1 -= dy;	y2 -= dy;
		}
		x0 = xe;	y0 = ye;
		Update();
	}
//	if(zoom)	Update();
	if(zoom)	Refresh(0);
}
//-----------------------------------------------------------------------------
wxString mglSetExtension(wxString &fname, const char *ext)
{
	wxString oname;
	if(fname.Right(4)!=wxChar('.')+wxString(ext,*wxConvCurrent))
		oname = fname+wxChar('.')+wxString(ext,*wxConvCurrent);
	return oname;
}
//-----------------------------------------------------------------------------
// NOTE: this is replacement for wxString::char_str() which is for v.2.8 or later
const char *mglw_str(const wxString &str)
{
	static char *buf=0;
	if(buf)	delete []buf;
	size_t i, n=str.Len();
	buf = new char[n+1];	buf[n]=0;
	for(i=0;i<n;i++)	buf[i] = str.GetChar(i);
	return buf;
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportPNG(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else	mgl_write_png(gr,mglw_str(mglSetExtension(fname,"png")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportPNGs(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else	mgl_write_png_solid(gr,mglw_str(mglSetExtension(fname,"png")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportJPG(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else	mgl_write_jpg(gr,mglw_str(mglSetExtension(fname,"jpg")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportBPS(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else
		mgl_write_bps(gr,mglw_str(mglSetExtension(fname,"eps")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportEPS(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else
		mgl_write_eps(gr,mglw_str(mglSetExtension(fname,"eps")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::ExportSVG(wxString fname)
{
	if(fname.IsEmpty())	fname = ScriptName;
	if(fname.IsEmpty())	wxMessageBox(appName, wxT("No filename."),wxOK|wxICON_ERROR ,this);
	else
		mgl_write_svg(gr,mglw_str(mglSetExtension(fname,"eps")), mglw_str(appName));
}
//-----------------------------------------------------------------------------
void wxMathGL::Copy()
{
	if (wxTheClipboard->Open())
	{
		wxTheClipboard->SetData( new wxBitmapDataObject(pic) );
		wxTheClipboard->Close();
	}
}
//-----------------------------------------------------------------------------
void wxMathGL::SetSize(int w, int h)
{	mgl_set_size(gr,w,h);	wxWindow::SetSize(w, h);	Update();	}
//-----------------------------------------------------------------------------
void wxMathGL::Adjust()
{
	wxSize sz=GetSize();
	mgl_set_size(gr,sz.GetWidth(),sz.GetHeight());
	Repaint();
}
//-----------------------------------------------------------------------------
void wxMathGL::NextSlide()
{
	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	if(g && g->GetNumFig()>1)	g->NextFrame();
}
//-----------------------------------------------------------------------------
void wxMathGL::PrevSlide()
{
	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	if(g && g->GetNumFig()>1)	g->PrevFrame();
}
//-----------------------------------------------------------------------------
void wxMathGL::Animation(bool st)
{
	if(st)	timer->Start(int(mgl_wnd_get_delay(gr)*1000));
	else	timer->Stop();
}
//-----------------------------------------------------------------------------
void wxMathGL::About()
{
	wxString s = wxT("MathGL v. 2.") + wxString::Format(wxT("%g"),MGL_VER2) +
		wxT("\n(c) Alexey Balakin, 2007\nhttp://mathgl.sourceforge.net/");
	wxMessageBox(s, wxT("MathGL - about"), wxOK|wxICON_INFORMATION, this);
}
//-----------------------------------------------------------------------------
