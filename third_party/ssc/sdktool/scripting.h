/**
BSD-3-Clause
Copyright 2019 Alliance for Sustainable Energy, LLC
Redistribution and use in source and binary forms, with or without modification, are permitted provided 
that the following conditions are met :
1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
and the following disclaimer.
2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
and the following disclaimer in the documentation and/or other materials provided with the distribution.
3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED.IN NO EVENT SHALL THE COPYRIGHT HOLDER, CONTRIBUTORS, UNITED STATES GOVERNMENT OR UNITED STATES 
DEPARTMENT OF ENERGY, NOR ANY OF THEIR EMPLOYEES, BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


#ifndef __automation_h
#define __automation_h

#include <wx/wx.h>
#include "dllinvoke.h"

#include <lk/absyn.h>
#include <lk/env.h>

class wxLKScriptCtrl;


class EditorWindow : public wxPanel
{
private:
	wxLKScriptCtrl *m_editor;
	wxStaticText *m_statusLabel;
	wxString m_fileName;
	wxButton *m_stopButton;
	wxString m_lastFindStr;
public:
	EditorWindow( wxWindow *parent );
	virtual ~EditorWindow();
	
	wxString GetFileName() { return m_fileName; }
	void OnCommand( wxCommandEvent &evt );
	void Open();
	bool Save();
	bool SaveAs();
	bool CloseDoc();
	bool Write( const wxString &file );
	bool Load( const wxString &file );	
	void Exec();


	wxLKScriptCtrl *GetEditor() { return m_editor; }

	DECLARE_EVENT_TABLE()
};


#endif

