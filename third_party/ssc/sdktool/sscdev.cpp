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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>
#include <wx/imaglist.h>
#include <wx/dynlib.h>

#include <wx/config.h>
#include <wx/scrolbar.h>
#include <wx/print.h>
#include <wx/printdlg.h>
#include <wx/accel.h>
#include <wx/image.h>
#include <wx/fs_zip.h>
#include <wx/html/htmlwin.h>
#include <wx/snglinst.h>
#include <wx/progdlg.h>
#include <wx/busyinfo.h>
#include <wx/dir.h>
#include <wx/stdpaths.h>
#include <wx/generic/helpext.h>
#include <wx/clipbrd.h>
#include <wx/splitter.h>
#include <wx/snglinst.h>
#include <wx/statline.h>
#include <wx/filepicker.h>
#include <wx/grid.h>
#include <wx/notebook.h>

#include <wex/lkscript.h>
#include <wex/metro.h>
#include <wex/extgrid.h>

#include "sscdev.h"
#include "dataview.h"
#include "scripting.h"


#ifdef _MSC_VER
	#ifdef _WIN64
	#define PLAT_BITS 64
	#else
	#define PLAT_BITS 32
	#endif
#else
#define PLAT_BITS 64
#endif


/* exported application global variables */

//int SC_major_ver = 2017;
//int SC_minor_ver = 2;
//int SC_micro_ver = 28;

SCFrame *app_frame = NULL;
wxConfig *app_config = NULL;

void applog(const wxString &s)
{
	if (app_frame) app_frame->Log(s);
}

/* ************************************************************
   ************ SC Application (set up handlers/config) ******
   ************************************************************ */

IMPLEMENT_APP(SCApp)

bool SCApp::OnInit()
{
#ifdef __WXMSW__
    typedef BOOL (WINAPI *SetProcessDPIAware_t)(void); 
    wxDynamicLibrary dllUser32(wxT("user32.dll")); 
    SetProcessDPIAware_t pfnSetProcessDPIAware = 
        (SetProcessDPIAware_t)dllUser32.RawGetSymbol(wxT("SetProcessDPIAware")); 
    if ( pfnSetProcessDPIAware ) 
        pfnSetProcessDPIAware(); 
#endif

	SetAppName( "SDKtool" );
	
	// set the current working directory to locate .pdb on crash
	if ( argc > 0 )
		wxSetWorkingDirectory( wxPathOnly(argv[0]) );

	app_config = new wxConfig( "ssc-sdk-tool", "WXAPPS" );
	
	wxInitAllImageHandlers();

    wxFileSystem::AddHandler(new wxZipFSHandler);

	app_frame = new SCFrame;
	SetTopWindow(app_frame);

	app_frame->Show();

	if (argc > 1)
	{
		if (argv[1].Right(3).Lower() == ".lk")
			app_frame->LoadScript(argv[1]);
		else
			app_frame->LoadBdat(argv[1]);
	}

	bool first_load = true;
//	wxString fl_key = wxString::Format("FirstLoad_%d",
//		SC_major_ver*10000
//		+SC_minor_ver*100
//		+SC_micro_ver );
	wxString fl_key ="first_load";
	app_config->Read(fl_key, &first_load, true);
	if (first_load)
	{
		// register the first load
		app_config->Write(fl_key, false);

		// on first load, maximize, and show help 'Getting Started'
		app_frame->SetPosition(wxPoint(10,10));
		app_frame->SetClientSize(700, 600);
		
		wxString dll_path;
		app_config->Read(wxString::Format("DllPath%d", PLAT_BITS), &dll_path);
		if (!wxFileExists(dll_path))
		{
			if ( wxYES==wxMessageBox("The SSC dynamic library is not loaded.  "
				"Would you like to select the proper library?\n\n"
				"Your selection will be saved for the next time you run SSC SDKtool.", 
				"Notice - first load", wxYES_NO, app_frame ) )
				app_frame->ChooseDynamicLibrary();
		}

		
	}

	return true;
}

int SCApp::OnExit()
{	
	if (app_config)
		delete app_config;
			
	return 0;
}

enum{ 
		ID_LOAD_UNLOAD_DLL= wxID_HIGHEST+1248,
		ID_CHOOSE_DLL,
		ID_LOAD_BDAT, ID_SAVE_BDAT,

		ID_CopyToClipboardCM, ID_listCM, ID_gridCM
};

BEGIN_EVENT_TABLE(SCFrame, wxFrame)

	EVT_BUTTON(ID_LOAD_BDAT, SCFrame::OnCommand)
	EVT_BUTTON(ID_SAVE_BDAT, SCFrame::OnCommand)

	EVT_BUTTON( wxID_EXECUTE, SCFrame::OnCommand )
	EVT_BUTTON( ID_LOAD_UNLOAD_DLL,       SCFrame::OnCommand )
	EVT_BUTTON( ID_CHOOSE_DLL,            SCFrame::OnCommand )
	
	EVT_LISTBOX(ID_listCM, SCFrame::OnCMListSelect )
	EVT_BUTTON(ID_CopyToClipboardCM, SCFrame::OnCopyToClipboard )

	EVT_CLOSE( SCFrame::OnCloseFrame )
		
END_EVENT_TABLE()

SCFrame::SCFrame()
   : wxFrame(NULL, wxID_ANY, wxString::Format("SSC SDKtool (%d bit)",  PLAT_BITS ), wxDefaultPosition, wxSize(800,600))
{
	m_varTable = new var_table;
	
#ifdef __WXMSW__
	SetIcon( wxIcon("appicon") );
#endif

	m_statusLabel = new wxStaticText( this, wxID_ANY, "Ready" );
	m_progressBar = new wxGauge( this, wxID_ANY, 100 );
	m_progressBar->Hide();
	
	wxSplitterWindow *split_win = new wxSplitterWindow( this, wxID_ANY,
		wxPoint(0,0), wxSize(800,700), wxSP_LIVE_UPDATE|wxBORDER_NONE );
	
	m_notebook = new wxMetroNotebook( split_win, wxID_ANY );
		
	wxPanel *cm_browser = new wxPanel( m_notebook );

	m_currentCM = new wxChoice( cm_browser, wxID_ANY );	
	
	m_listCM = new wxListBox(cm_browser, ID_listCM);
		
	m_gridCM = new wxExtGridCtrl(cm_browser, wxID_ANY);
	m_gridCM->CreateGrid(2,2);
	m_gridCM->EnableEditing(false);
	m_gridCM->DisableDragCell();
	m_gridCM->DisableDragColSize();
	m_gridCM->DisableDragRowSize();
	m_gridCM->DisableDragColMove();
	m_gridCM->DisableDragGridSize();
	m_gridCM->SetRowLabelSize(23);
	m_gridCM->SetColLabelSize(23);	
	m_gridCM->EnableDragColSize();
	
	wxBoxSizer *szh_run = new wxBoxSizer( wxHORIZONTAL );
	szh_run->Add( m_currentCM, 1, wxALL|wxEXPAND, 3 );
	szh_run->Add( new wxButton( cm_browser, wxID_EXECUTE, "Run" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT),0, wxALL|wxALIGN_CENTER_VERTICAL, 3);
	
	wxBoxSizer *szleft = new wxBoxSizer( wxVERTICAL );
	szleft->Add( new wxStaticText( cm_browser, wxID_ANY, " Available modules:" ), 0, wxALL|wxEXPAND, 1 );
	szleft->Add( m_listCM, 1, wxALL|wxEXPAND, 3 );	
	szleft->Add( new wxButton(cm_browser, ID_CopyToClipboardCM, "Copy table to clipboard...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 3);
	szleft->Add( new wxStaticLine( cm_browser, wxID_ANY ), 0, wxALL|wxEXPAND, 2 );
	szleft->Add( szh_run, 0, wxALL|wxEXPAND, 3 );

	wxBoxSizer *szcenter = new wxBoxSizer(wxHORIZONTAL );
	szcenter->Add( szleft, 1, wxALL|wxEXPAND, 0 );
	szcenter->Add( m_gridCM, 5, wxALL|wxEXPAND, 0 );

	wxBoxSizer *szmaintools = new wxBoxSizer( wxHORIZONTAL );

	szmaintools->Add( new wxButton( cm_browser, ID_LOAD_UNLOAD_DLL, "Load/unload library", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2 );
	szmaintools->Add( new wxButton( cm_browser, ID_CHOOSE_DLL, "Choose SSC library...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2 );
	szmaintools->Add( new wxButton( cm_browser, ID_LOAD_BDAT, "Load data file...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2 );
	szmaintools->Add( new wxButton( cm_browser, ID_SAVE_BDAT, "Save data file...", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2 );
	szmaintools->AddStretchSpacer();
	
	wxBoxSizer *szvertmain = new wxBoxSizer( wxVERTICAL );
	szvertmain->Add( szmaintools, 0, wxALL|wxEXPAND, 2 );
	szvertmain->Add( szcenter, 1, wxALL|wxEXPAND, 0 );
	cm_browser->SetSizer( szvertmain );

	
	m_dataView = new DataView(m_notebook);
	m_dataView->SetDataObject( m_varTable );

	m_scriptWindow = new EditorWindow(m_notebook);

	m_notebook->AddPage( cm_browser, "Module Browser", true );
	m_notebook->AddPage( m_dataView, "Data Container", false );
	m_notebook->AddPage( m_scriptWindow, "Script Editor", false );

	m_txtOutput = new wxTextCtrl(split_win, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize,
		wxTE_READONLY | wxTE_MULTILINE | wxHSCROLL | wxTE_DONTWRAP | wxBORDER_NONE );
	m_txtOutput->SetFont( wxFont(10, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "consolas") );
	m_txtOutput->SetForegroundColour( *wxBLUE );
	
	split_win->SplitHorizontally( m_notebook, m_txtOutput, -180 );
	split_win->SetSashGravity( 1 );

	wxBoxSizer *sz_stat = new wxBoxSizer( wxHORIZONTAL );
	sz_stat->Add( m_progressBar, 1, wxALL|wxEXPAND, 3 );
	sz_stat->Add( m_statusLabel, 5, wxALL|wxEXPAND, 3 );

	wxBoxSizer *sz_main = new wxBoxSizer( wxVERTICAL );
	sz_main->Add( split_win, 1, wxALL|wxEXPAND, 0 );
	sz_main->Add( sz_stat, 0, wxALL|wxEXPAND, 0 );

	SetSizer( sz_main );

	app_config->Read("CurrentDirectory", &m_currentAppDir);
	
	// restore window position
	bool b_maximize = false;
	int f_x,f_y,f_width,f_height;
	app_config->Read("FrameX", &f_x, -1);
	app_config->Read("FrameY", &f_y, -1);
	app_config->Read("FrameWidth", &f_width, -1);
	app_config->Read("FrameHeight", &f_height, -1);
	app_config->Read("FrameMaximized", &b_maximize, false);

	if (b_maximize)
	{
		this->Maximize();
	}
	else
	{
		if (f_width > 100 && f_height > 100)
			this->SetClientSize(f_width, f_height);

		if (f_x > 0 && f_y > 0)
			this->SetPosition(wxPoint(f_x,f_y));
	}

	UpdateUI();
	
	wxString dll_path;
	app_config->Read(wxString::Format("DllPath%d", PLAT_BITS), &dll_path);
	if (wxFileExists(dll_path))
	{
		m_dllPath = dll_path;
		LoadUnloadLibrary();
	}
	
	wxAcceleratorEntry entries[10];
	entries[0].Set(::wxACCEL_CMD, 's', wxID_SAVE);
	entries[1].Set(::wxACCEL_CMD, 'o', wxID_OPEN);
	entries[2].Set(::wxACCEL_NORMAL, WXK_F5, wxID_EXECUTE);
	wxAcceleratorTable acceltab(3,entries);
	SetAcceleratorTable(acceltab);
}

bool SCFrame::UpdateIsStopFlagSet()
{
	return m_scriptWindow->GetEditor()->IsStopFlagSet();
}

SCFrame::~SCFrame()
{
	delete m_varTable;
}


void SCFrame::SetProgress( int percent, const wxString & )
{
	m_progressBar->SetValue( percent );
}

void SCFrame::UpdateUI()
{
	wxString status;

	if (sscdll_isloaded())
	{
		int ver = 0;
		wxString build = "no info";
		try {
			ver = ssc_version();
			build = ssc_build_info();
		} catch (sscdll_error &e) {
			status = wxString(e.text.c_str()) + " ";
			ver = -999;
		}
		status += m_dllPath + " Version " + wxString::Format("%d, %s", ver, build);
	}
	else
		status = "SSC dynamic library not loaded.";

	m_statusLabel->SetLabel( status );
}
	
void SCFrame::OnCloseFrame( wxCloseEvent &evt )
{
	if (evt.CanVeto() && !CloseDocument())
	{
		evt.Veto();
		return;
	}	

	/* save window position */
	bool b_maximize = this->IsMaximized();
	int f_x,f_y,f_width,f_height;

	this->GetPosition(&f_x,&f_y);
	this->GetClientSize(&f_width, &f_height);
	
	app_config->Write("FrameX", f_x);
	app_config->Write("FrameY", f_y);
	app_config->Write("FrameWidth", f_width);
	app_config->Write("FrameHeight", f_height);
	app_config->Write("FrameMaximized", b_maximize);
	app_config->Write("CurrentDirectory", m_currentAppDir);
	app_config->Write(wxString::Format("DllPath%d", PLAT_BITS), m_dllPath );

	sscdll_unload(); // make sure dll is unloaded;

	Destroy();
}

void SCFrame::SaveBdat()
{
	wxFileDialog dlg(this, "Save SDKtool State", wxPathOnly(m_lastFile),
		m_lastFile, "Binary Data File (*.bdat)|*.bdat", wxFD_SAVE);
	
	int ret = dlg.ShowModal();
	m_lastFile = dlg.GetPath();

	if (ret!=wxID_OK) return;

	if(!WriteBdatToDisk(m_lastFile))
		wxMessageBox("Error writing:\n\n" + m_lastFile );
}

bool SCFrame::CloseDocument()
{
	return (m_scriptWindow->CloseDoc());
}

void SCFrame::LoadUnloadLibrary()
{	
	if (!sscdll_isloaded())
	{
		if ( !sscdll_load( m_dllPath.c_str() ) )
		{
			wxMessageBox("Error loading " + m_dllPath + "\n\nCheck path, architecture (32/64 bit), and version.");			
			m_dllPath.Empty();
		}
		else
			m_currentAppDir = wxPathOnly(m_dllPath);
	}
	else
	{
		sscdll_unload();
	}
	
	UpdateCMForm();
	LoadCMs();
	UpdateUI();
}

void SCFrame::ChooseDynamicLibrary()
{
	wxFileDialog fd(this, "Choose SSC dynamic library", wxPathOnly(m_dllPath), m_dllPath, 
#ifdef __WXMSW__
		"Dynamic Link Libraries (*.dll)|*.dll"
#endif
#ifdef __WXOSX__
		"Dynamic Libraries (*.dylib)|*.dylib"
#endif
#ifdef __WXGTK__
		"Shared Libraries (*.so)|*.so"
#endif
		, wxFD_OPEN);
	if (fd.ShowModal() != wxID_OK) return;
	wxString file = fd.GetPath();
	m_dllPath = file;
	m_currentAppDir = wxPathOnly(file);
	sscdll_unload();

	LoadUnloadLibrary();
}

void SCFrame::OnCommand(wxCommandEvent &evt)
{	
	switch(evt.GetId())
	{
	case wxID_EXECUTE:
		if ( m_notebook->GetSelection() == 0 )
		{
			app_frame->ClearLog();
			app_frame->Start();
		}
		else
		{
			m_scriptWindow->Exec();
		}
		break;
	case ID_LOAD_BDAT:
		LoadBdat();
		break;
	case ID_SAVE_BDAT:
		SaveBdat();
		break;
	case ID_LOAD_UNLOAD_DLL:
		LoadUnloadLibrary();
		break;
	case ID_CHOOSE_DLL:
		ChooseDynamicLibrary();
		break;
	}
}

void SCFrame::WriteVarTable( wxDataOutputStream &o, var_table &vt )
{
	o.Write16( 0xae ); // start identifier, versioner
	o.Write32( vt.size() );
	const char *key = vt.first();
	while (key != 0)
	{
		o.WriteString( key );
		var_data *v = vt.lookup( key );
		o.Write8( v->type );
		switch( v->type )
		{
		case SSC_STRING:
			o.WriteString( wxString( (const char*)v->str.c_str() ) ); break;
		case SSC_NUMBER:
			o.WriteDouble( v->num ); break;
		case SSC_ARRAY:
			o.Write32( v->num.length() );
			for (size_t i=0;i<v->num.length(); i++)
				o.WriteDouble( v->num[i] );
			break;
		case SSC_MATRIX:
			o.Write32( v->num.nrows() );
			o.Write32( v->num.ncols() );
			for (size_t r=0;r<v->num.nrows();r++)
				for (size_t c=0;c<v->num.ncols();c++)
					o.WriteDouble( v->num.at(r,c) );
			break;
		case SSC_TABLE:
			WriteVarTable(o, v->table );
			break;
		}

		key = vt.next();
	}
	o.Write16( 0xae ); // end identifier
}

bool SCFrame::ReadVarTable( wxDataInputStream &o, var_table &vt, bool clear_first )
{
	if (clear_first)
		vt.clear();

	int code = o.Read16();
	int size = o.Read32();
	size_t len, nrows, ncols;
	for (int nn=0;nn<size;nn++)
	{
		var_data vv;

		wxString key = o.ReadString();
		vv.type = (unsigned char) o.Read8();
		switch( vv.type )
		{
		case SSC_STRING:
			vv.str = (const char*)o.ReadString().c_str(); break;
		case SSC_NUMBER:
			vv.num = (ssc_number_t)o.ReadDouble(); break;
		case SSC_ARRAY:
			len = (size_t)o.Read32();
			vv.num.resize( len );
			for (size_t i=0;i<len;i++)
				vv.num[i] = (ssc_number_t) o.ReadDouble();
			break;
		case SSC_MATRIX:
			nrows = (size_t)o.Read32();
			ncols = (size_t)o.Read32();
			vv.num.resize(nrows,ncols);
			for (size_t r=0;r<nrows;r++)
				for (size_t c=0;c<ncols;c++)
					vv.num.at(r,c) = (ssc_number_t)o.ReadDouble();
			break;
		case SSC_TABLE:
			if (!ReadVarTable( o, vv.table, true ))
				return false;
			break;
		}

		vt.assign( key.ToStdString(), vv );
	}

	return (o.Read16() == code);
}

bool SCFrame::LoadScript(wxString fn)
{
	if (fn.IsEmpty())
	{

		wxFileDialog dlg(this, "Load lk script",
			wxPathOnly(m_lastFile),
			m_lastFile,
			"Script file (*.lk)|*.lk",
			wxFD_OPEN);

		if (dlg.ShowModal() == wxID_OK)
		{
			m_lastFile = dlg.GetPath();
			fn = m_lastFile;
		}
		else
			return false;
	}
	bool ret = m_scriptWindow->Load(fn);
	m_notebook->SetSelection(2);
	return ret;

}

bool SCFrame::LoadBdat( wxString fn )
{
	if (fn.IsEmpty())
	{
	
		wxFileDialog dlg(this, "Load SDKtool State",
			wxPathOnly(m_lastFile),
			m_lastFile,
			"Binary Data File (*.bdat)|*.bdat",
			wxFD_OPEN);

		if (dlg.ShowModal() == wxID_OK)
		{
			m_lastFile = dlg.GetPath();
			fn = m_lastFile;
		}
		else
			return false;
	}

	wxBusyInfo busy("Loading: " + fn);

	wxFileInputStream fp( fn );
	if (!fp.Ok()) return false;
	wxDataInputStream in( fp );
		
	m_varTable->clear();
	UpdateUI();
	m_dataView->UpdateView();

	int code = in.Read16(); // start header code, versioner

	
	wxString cm = in.ReadString();
	SetCurrentCM( cm );

	wxArrayString sel_vars;
	std::vector<int> cwl;

	int nn = in.Read32();
	for (int i=0;i<nn;i++)
		sel_vars.Add( in.ReadString() );

	nn = in.Read32();
	for (int i=0;i<nn;i++)
		cwl.push_back( in.Read32() );

	bool vtok = ReadVarTable( in, *m_varTable, true );

	m_dataView->UpdateView();	
	m_dataView->SetSelections( sel_vars );
	m_dataView->UpdateView();
	m_dataView->SetColumnWidths( cwl );

	UpdateUI();
	
	return vtok && in.Read16() == code;	
}

bool SCFrame::WriteBdatToDisk(const wxString &fn)
{
	wxBusyInfo busy("Writing: " + fn);

	wxFileOutputStream fp( fn );
	if (!fp.Ok()) return false;
	wxDataOutputStream o( fp );
	o.Write16( 0xe3 );

	wxString cm = GetCurrentCM();
	o.WriteString( cm );

	wxArrayString selvars = m_dataView->GetSelections();
	o.Write32( selvars.Count() );
	for (size_t i=0;i<selvars.Count(); i++)
		o.WriteString( selvars[i] );

	std::vector<int> cwl = m_dataView->GetColumnWidths();
	o.Write32( cwl.size() );
	for (size_t i=0;i<cwl.size();i++)
		o.Write32( cwl[i] );

	WriteVarTable( o, *m_varTable );

	o.Write16( 0xe3 );

	UpdateUI();

	return true;
}

void SCFrame::Log(const wxString &text, bool wnl)
{
	if (wnl) m_txtOutput->AppendText(text + "\n");
	else m_txtOutput->AppendText(text);
}

void SCFrame::ClearLog()
{
	m_txtOutput->Clear();
}

/*
class default_sync_proc : public util::sync_piped_process
{
private:
	ssc_handler_t m_handler;
public:
	default_sync_proc( ssc_handler_t ph ) : m_handler(ph) {  }

	virtual void on_stdout(const std::string &line_text)
	{
		::ssc_module_extproc_output( m_handler, line_text.c_str() );
	}
};
*/

ssc_bool_t my_handler( ssc_module_t , ssc_handler_t , int action, 
	float f0, float f1, const char *s0, const char *, void * )
{
	if (action == SSC_LOG)
	{
		// print log message to console
		wxString msg;
		switch( (int)f0 )
		{
		case SSC_NOTICE: msg << "Notice: " << s0 << " time " << f1; break;
		case SSC_WARNING: msg << "Warning: " << s0 << " time " << f1; break;
		case SSC_ERROR: msg << "Error: " << s0 << " time " << f1; break;
		default: msg << "Log notice uninterpretable: " << f0 << " time " << f1; break;
		}

		app_frame->Log(msg);
		return 1;
	}
	else if (action == SSC_UPDATE)
	{
		// print status update to console
		app_frame->SetProgress( (int) f0, s0 );
		wxGetApp().Yield(true);
		return !app_frame->UpdateIsStopFlagSet();
	}
/*
	else if (action == SSC_EXECUTE)
	{
		// run the executable, pipe the output, and return output to p_mod
		// **TODO**
		default_sync_proc exe( p_handler );
		return exe.spawn( s0, s1 ) == 0;
	}
*/
	else
		return 0;
}

void SCFrame::Copy( ssc_module_t p_mod, ssc_data_t p_data, var_table *vt, bool clear_first)
{
	if (clear_first)
		::ssc_data_clear( p_data );

	int pidx = 0;
	while (const ssc_info_t p_inf = ssc_module_var_info(p_mod, pidx++))
	{
		int var_type = ssc_info_var_type(p_inf);   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
//		int data_type = ssc_info_data_type(p_inf); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX		
		const char *name = (ssc_info_name(p_inf)); // assumed to be non-null
		wxString reqd(ssc_info_required(p_inf));


		if (var_type == SSC_INPUT || var_type == SSC_INOUT)
		{
			var_data *v = vt->lookup(name);

			if (v)
			{
				switch (v->type)
				{
				case SSC_STRING:
					::ssc_data_set_string(p_data, name, v->str.c_str());
					break;
				case SSC_NUMBER:
				{
					::ssc_data_set_number(p_data, name, (ssc_number_t)v->num);
					wxString nm(name);
				}
				break;
				case SSC_ARRAY:
					::ssc_data_set_array(p_data, name, v->num.data(), v->num.length());
					break;
				case SSC_MATRIX:
					::ssc_data_set_matrix(p_data, name, v->num.data(), v->num.nrows(), v->num.ncols());
					break;
				case SSC_TABLE:
					::ssc_data_set_table(p_data, name, (&v->table));
					break;
				}
			}
		}
		//name = vt->next();
	}
}

void SCFrame::Copy( var_table *vt,  ssc_data_t p_data, bool clear_first )
{	
	if (clear_first) vt->clear();

	const char *name = ::ssc_data_first( p_data );
	while (name)
	{
		int type = ::ssc_data_query( p_data, name );
		switch( type )
		{
		case SSC_STRING:
			{
				const char *s = ::ssc_data_get_string( p_data, name );
				if (s) vt->assign( name, var_data(  std::string(s) ) );
			}
			break;
		case SSC_NUMBER:
			{
				ssc_number_t val = 0.0;
				if ( ::ssc_data_get_number( p_data, name, &val ) )
					vt->assign( name, var_data( val ) );
				wxString nm(name);
			}
			break;
		case SSC_ARRAY:
			{
				int len = 0;
				const ssc_number_t *pvals = ::ssc_data_get_array( p_data, name, &len );
				if (pvals)
					vt->assign( name, var_data( pvals, len ) );
			}
			break;
		case SSC_MATRIX:
			{
				int nrows = 0, ncols = 0;
				const ssc_number_t *pmat = ::ssc_data_get_matrix( p_data, name, &nrows, &ncols );
				if (pmat)
					vt->assign( name, var_data( pmat, nrows, ncols ) );
			}
			break;
		case SSC_TABLE:
			{
				ssc_data_t table = ::ssc_data_get_table( p_data, name );
				var_table *src = (var_table*)table;

				var_data x;
				x.type = SSC_TABLE;
				x.table = *src; // deep copy
				vt->assign( name, x );				
			}
			break;
		}

		name = ::ssc_data_next( p_data );
	}
}

std::vector<bool> SCFrame::Start()
{
	std::vector<bool> ok;

	m_progressBar->Show();
	Layout();
	wxGetApp().Yield();

	wxString cm = GetCurrentCM();
	if ( cm.IsEmpty() )
	{
		wxMessageBox("No compute modules selected for simulation.\n\nSelect one or more on the Module Browser tab.");
		return ok;
	}

	try {
		ssc_module_t p_mod = ::ssc_module_create( (const char*) cm.c_str() );
			
		if (p_mod != 0)
		{
			ssc_data_t p_data = ::ssc_data_create();
			Copy(p_mod, p_data, m_varTable, true);
			wxStopWatch sw;
			sw.Start();			
			if (! ::ssc_module_exec_with_handler( p_mod, p_data,
				my_handler, 0) )
			{
				Log("EXEC_FAIL: "+cm);
				ok.push_back( false );
			}
			else
				ok.push_back( true );

			::ssc_module_free( p_mod );
			Copy(m_varTable, p_data, false);
			::ssc_data_free(p_data);

		}
		else			
			Log("CREATE_FAIL: " + cm );

		m_dataView->UpdateView();

	 
	} catch(sscdll_error &e) {
		wxMessageBox("Library error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}

	m_progressBar->Hide();
	Layout();
	
	return ok;
}


wxArrayString SCFrame::GetAvailableCMs()
{
	wxArrayString list;
	try {

		int idx=0;
		while (const ssc_entry_t p_entry = ::ssc_module_entry(idx++))
			list.Add( ::ssc_entry_name(p_entry) );

	} catch(sscdll_error &) {
	}

	return list;
}


void SCFrame::LoadCMs()
{
	m_listCM->Clear();
	m_gridCM->ClearGrid();
	wxArrayString l = GetAvailableCMs();
	for (size_t i=0;i<l.Count();i++)
		m_listCM->Append( l[i] );
}

void SCFrame::OnCopyToClipboard(wxCommandEvent &)
{
	wxBusyInfo info("Copying data to clipboard...");
	m_gridCM->Copy(true);
	wxMilliSleep(350);
}

void SCFrame::OnCMListSelect(wxCommandEvent &)
{
	try {
		wxString cm_name = m_listCM->GetStringSelection();
	
		ssc_module_t p_mod = ::ssc_module_create( (const char*)cm_name.c_str() );
		if ( p_mod == 0 )
		{
			wxMessageBox("Could not create a module of type: " + cm_name );
			return;
		}

		std::vector<wxArrayString> vartab;

		int idx=0;
		while( const ssc_info_t p_inf = ssc_module_var_info( p_mod, idx++ ) )
		{
			int var_type = ssc_info_var_type( p_inf );   // SSC_INPUT, SSC_OUTPUT, SSC_INOUT
			int data_type = ssc_info_data_type( p_inf ); // SSC_STRING, SSC_NUMBER, SSC_ARRAY, SSC_MATRIX
		

			wxArrayString row;

			switch( var_type )
			{
			case SSC_INPUT: row.Add("SSC_INPUT"); break;
			case SSC_OUTPUT: row.Add("SSC_OUTPUT"); break;
			case SSC_INOUT: row.Add("SSC_INOUT"); break;
			default: row.Add("<unknown>"); break;
			}

			switch( data_type )
			{
			case SSC_STRING: row.Add("SSC_STRING"); break;
			case SSC_NUMBER: row.Add("SSC_NUMBER"); break;
			case SSC_ARRAY: row.Add("SSC_ARRAY"); break;
			case SSC_MATRIX: row.Add("SSC_MATRIX"); break;
			case SSC_TABLE: row.Add("SSC_TABLE"); break;
			default: row.Add("<unknown>"); break;
			}
			
			row.Add( ssc_info_name( p_inf ) );
			row.Add( ssc_info_label( p_inf ) );
			row.Add( ssc_info_units( p_inf ) );
			row.Add( ssc_info_meta( p_inf ) );
			row.Add( ssc_info_group( p_inf ) );
			row.Add( ssc_info_required( p_inf ) );
			row.Add( ssc_info_constraints( p_inf ) );

			vartab.push_back(row);
		}

		int nrows = (int)vartab.size();
		int ncols = 9;
		
		m_gridCM->Freeze();
		m_gridCM->ResizeGrid( nrows, ncols);
		m_gridCM->SetColLabelValue( 0, "TYPE" );
		m_gridCM->SetColLabelValue( 1, "DATA" );
		m_gridCM->SetColLabelValue( 2, "NAME" );
		m_gridCM->SetColLabelValue( 3, "LABEL" );
		m_gridCM->SetColLabelValue( 4, "UNITS" );
		m_gridCM->SetColLabelValue( 5, "META" );
		m_gridCM->SetColLabelValue( 6, "GROUP" );
		m_gridCM->SetColLabelValue( 7, "REQUIRE" );
		m_gridCM->SetColLabelValue( 8, "CONSTRAINT" );

		for (int r=0;r<nrows;r++)
			for (int c=0;c<ncols;c++)
				m_gridCM->SetCellValue( r, c, vartab[r][c]  );

		m_gridCM->AutoSizeColumns(false);
		m_gridCM->Thaw();

		::ssc_module_free( p_mod );
	
	} catch(sscdll_error &e) {
		wxMessageBox("Dynamic library error: " + wxString(e.func.c_str()) + ": " + wxString(e.text.c_str()) );
	}
}


void SCFrame::UpdateCMForm()
{
	wxString sel = m_listCM->GetStringSelection();
	wxString run = m_currentCM->GetStringSelection();

	wxArrayString list = GetAvailableCMs();
	m_listCM->Clear();
	m_listCM->Append( list );
	m_currentCM->Clear();
	m_currentCM->Append( list );
	if (list.Index( sel ) != wxNOT_FOUND )
		m_listCM->SetStringSelection( sel );

	if (list.Index( run ) != wxNOT_FOUND )
		m_currentCM->SetStringSelection( run );
}
