/***************************************************************************
 * wx_example.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <wx/wx.h>
#undef _
#include "mgl2/wx.h"
//-----------------------------------------------------------------------------
int test_wnd(mglGraph *gr);
int sample(mglGraph *gr);
int sample_1(mglGraph *gr);
int sample_2(mglGraph *gr);
int sample_3(mglGraph *gr);
int sample_d(mglGraph *gr);
//-----------------------------------------------------------------------------
class testApp : public wxApp
{
public:
	virtual bool OnInit();
};
//-----------------------------------------------------------------------------
class testFrame: public wxFrame
{
public:
	testFrame(wxFrame *frame, const wxString& title);
	~testFrame() {}
private:
	enum	{	idMenuQuit = 1000	};
	void OnClose(wxCloseEvent& )	{	Destroy();	}
	void OnQuit(wxCommandEvent& )	{	Destroy();	}

	wxScrolledWindow *scroll;
	wxMathGL *mgl;
	DECLARE_EVENT_TABLE()
};
//-----------------------------------------------------------------------------
IMPLEMENT_APP(testApp)
//-----------------------------------------------------------------------------
bool testApp::OnInit()
{
	testFrame* frame = new testFrame(0L, _("MathGL + wxWidgets sample"));
	frame->Show();
	return true;
}
//-----------------------------------------------------------------------------
BEGIN_EVENT_TABLE(testFrame, wxFrame)
	EVT_CLOSE(testFrame::OnClose)
	EVT_MENU(idMenuQuit, testFrame::OnQuit)
END_EVENT_TABLE()
//-----------------------------------------------------------------------------
testFrame::testFrame(wxFrame *frame, const wxString& title) : wxFrame(frame, -1, title)
{
	// create a menu bar
	wxMenuBar* mbar = new wxMenuBar();
	wxMenu* fileMenu = new wxMenu(_T(""));
	fileMenu->Append(idMenuQuit, _("&Quit\tAlt-F4"), _("Quit the application"));
	mbar->Append(fileMenu, _("&File"));
	SetMenuBar(mbar);
	SetSize(800,620);

	scroll = new wxScrolledWindow(this);
	mgl = new wxMathGL(scroll);
	mgl->SetDraw(mgl_draw_graph,(void*)sample);
	mgl->Update();
}
//-----------------------------------------------------------------------------
