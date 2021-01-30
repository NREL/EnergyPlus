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


#ifndef __SC_h
#define __SC_h

#include <wx/wx.h>
#include <wx/wfstream.h>
#include <wx/datstrm.h>

#include "dllinvoke.h"

class DataView;
class SCFrame;
class SCDocWin;
class wxNotebook;
class EditorWindow;
class wxConfig;

extern SCFrame *app_frame;
extern wxConfig *app_config;

void applog(const wxString &s);
void applog(const char *fmt, ...);

//extern int SC_major_ver;
//extern int SC_minor_ver;
//extern int SC_micro_ver;

class SCApp : public wxApp
{
public:
	virtual bool OnInit();
	virtual int OnExit();
};

DECLARE_APP(SCApp)

class wxMetroNotebook;
class wxExtGridCtrl;

class SCFrame : public wxFrame
{
public:
	SCFrame();
	virtual ~SCFrame();
		
	bool CloseDocument();
	bool LoadBdat( wxString fn = wxEmptyString );

	bool LoadScript(wxString fn = wxEmptyString);


	void SaveBdat();
	bool WriteBdatToDisk(const wxString &fn);
	
	void ChooseDynamicLibrary();
	void LoadUnloadLibrary();

	std::vector<bool> Start();
	void ClearLog();
	void Log(const wxString &, bool wnl=true);
	
	static void Copy(ssc_module_t p_mod, ssc_data_t p_data, var_table *vt, bool clear_first);
	static void Copy(var_table *vt, ssc_data_t p_data, bool clear_first);

	void Progress(const wxString &text, float percent);

	wxString LastFileName() { return m_lastFile; }

	DataView *GetDataView() { return m_dataView; }
	var_table *GetVarTable() { return m_varTable; }


	void SetProgress( int percent, const wxString &msg = wxEmptyString );
	
	wxArrayString GetAvailableCMs();
	void LoadCMs();
	void SetCurrentCM( const wxString & cm ) { m_currentCM->SetStringSelection( cm ); }
	wxString GetCurrentCM() { return m_currentCM->GetStringSelection(); }
	void UpdateCMForm();
	void OnRun( wxCommandEvent & );
	void OnCMListSelect(wxCommandEvent &evt);
	void OnCopyToClipboard(wxCommandEvent &);

	bool UpdateIsStopFlagSet();

private:	
	void WriteVarTable( wxDataOutputStream &o, var_table &vt );
	bool ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first );

	void UpdateUI();

	void OnCommand(wxCommandEvent &evt);
	void OnCloseFrame(wxCloseEvent &evt);
	
	wxExtGridCtrl *m_gridCM;
	wxChoice *m_currentCM;
	wxListBox *m_listCM;

	wxStaticText *m_statusLabel;
	wxGauge *m_progressBar;

	wxString m_currentAppDir;
	wxString m_lastFile;
	wxString m_dllPath;

	wxTextCtrl *m_txtOutput;

	wxMetroNotebook *m_notebook;
	DataView *m_dataView;
	EditorWindow *m_scriptWindow;

	var_table *m_varTable;
		
	DECLARE_EVENT_TABLE()
};

#endif
