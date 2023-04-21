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

#define _CRT_SECURE_NO_WARNING 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/wx.h>

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
#include <wx/aui/aui.h>
#include <wx/datstrm.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>
#include <wx/splitter.h>
#include <wx/aui/auibook.h>
#include <wx/statline.h>
#include <wx/progdlg.h>

#include <wx/accel.h>

#include <wex/utils.h>
#include <wex/extgrid.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/numeric.h>
#include <wex/metro.h>

#include <tcskernel.h>

#include "tcslayout.h"
#include "tcsscript.h"
#include "tcsmain.h"

void ClearLog()
{
	tcFrame::Instance()->ClearLog();
}

void Log( const wxString &text )
{
	tcFrame::Instance()->Log( text );
}

void Log( const char *fmt, ... )
{
	static char buf[2048];
	va_list ap;
	va_start(ap, fmt);
#if defined(_MSC_VER)||defined(_WIN32)
	_vsnprintf(buf, 2046, fmt, ap);
#else
	vsnprintf(buf, 2046, fmt, ap);
#endif
	va_end(ap);
	tcFrame::Instance()->Log( buf );	
}

tcKernel::tcKernel( tcFrame *frm, tcstypeprovider *prov )
	: tcskernel( prov ), m_frame(frm), m_start(0), m_end(0), m_step(0)
{
	m_storeArrMatData = false;
}
tcKernel::~tcKernel()
{
}

void tcKernel::log( const std::string &s )
{
	m_frame->Log( wxString(s.c_str()) );
}

#ifdef _MSC_VER
#define mysnprintf _snprintf
#else
#define mysnprintf snprintf
#endif

bool tcKernel::converged( double time )
{
	if (m_step != 0.0 && m_progressDialog != 0)
	{
		int istep = (int) (time-m_start)/m_step;
		int nstep = (int) (m_end-m_start)/m_step;
		int nnsteps = nstep/200;
		if ( nnsteps == 0 ) nnsteps = 1;		
		if (istep % nnsteps == 0)
		{
			double percent = 100 * (((double)istep) / ((double)nstep) );
			double elapsed = m_watch.Time() * 0.001;
			if ( !m_progressDialog->Update( (int) percent,
				wxString::Format("%.1lf %% complete, %.2lf seconds elapsed, hour %.1lf", 
					percent, elapsed, time/3600 )) )
			{
				return false; // progess dialog was canceled, abort simulation
			}
		}
	}

	std::string buf;
	char ibuf[128];
	size_t j,k;
	for ( size_t i=0;i<m_results.size(); i++ )
	{
		tcsvalue &v = m_results[i].u->values[ m_results[i].idx ];
		switch( m_results[i].type )
		{
		case TCS_NUMBER:
			m_results[i].values[ m_dataIndex ].dval = v.data.value;
			break;
		case TCS_STRING:
			m_results[i].values[ m_dataIndex ].sval = v.data.cstr;
			break;
		case TCS_ARRAY:
			if ( m_storeArrMatData )
			{
				buf = "[ ";
				for (j=0;j<v.data.array.length;j++)
				{
					mysnprintf(ibuf, 126, "%lg%c", v.data.array.values[j],
						j < v.data.array.length-1 ? ',' : ' ');
					buf += ibuf;
				}
				buf += "]";
				m_results[i].values[ m_dataIndex ].sval = buf;
			}
			break;
		case TCS_MATRIX:
			if ( m_storeArrMatData )
			{
				mysnprintf( ibuf, 126, "{ %dx%d ", v.data.matrix.nrows, v.data.matrix.ncols );
				buf = ibuf;
				for (j=0;j<v.data.matrix.nrows;j++)
				{
					buf += " [";
					for (k=0;k<v.data.matrix.ncols;k++)
					{
						mysnprintf(ibuf, 126, "%lg%c", TCS_MATRIX_INDEX(&v, j, k),
							k < v.data.matrix.ncols-1 ? ',' : ' ');
						buf += ibuf;
					}
					buf += "]";
				}
				buf += " }";
				m_results[i].values[ m_dataIndex ].sval = buf;		
			}
			break;
		}
	}

	m_dataIndex++;

	return true;
}

int tcKernel::simulate( double start, double end, double step, wxProgressDialog *pd, double *time_sec )
{
	wxBusyInfo *info = new wxBusyInfo("preparing simulation data vectors...");
	m_progressDialog = pd;

	// find all output variables and add to results vector
	m_start = start;
	m_end = end;
	m_step = step;
	m_dataIndex = 0;

	if ( end <= start || step <= 0 )
	{
		delete info;
		return -77;
	}

	int nsteps = (int)( (end-start)/step ) + 1;

	size_t ndatasets = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx=0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			idx++;
			ndatasets++;
		}
	}

	if ( ndatasets < 1 )
	{
		delete info;
		return -88;
	}

	m_results.resize( ndatasets );

	size_t idataset = 0;
	for (size_t i=0;i<m_units.size();i++)
	{
		tcsvarinfo *vars = m_units[i].type->variables;
		int idx = 0;
		while( vars[idx].var_type != TCS_INVALID )
		{
			dataset &d = m_results[ idataset++ ];
			char buf[32];
			sprintf(buf, "%d", (int)i);
			d.u = &m_units[i];
			d.uidx = i;
			d.idx = idx;
			d.group = "Unit " + std::string(buf) + " (" + std::string(m_units[i].type->name) + ")";//: " + m_units[i].name;
			d.name = vars[idx].name;
			d.units = vars[idx].units;
			d.type = vars[idx].data_type;
			d.values.resize( nsteps, dataitem(0.0) );

			idx++;
		}
	}
	
	delete info; // close the wxBusyInfo
	wxGetApp().Yield( true );

	m_watch.Start();
	int code = tcskernel::simulate( start, end, step );
	if (time_sec) *time_sec = ((double)m_watch.Time())*0.001;
	return code;
}

tcKernel::dataset *tcKernel::get_results(int idx)
{
	if (idx >= (int) m_results.size()) return 0;
	else return &m_results[idx];
}


ResultsTable::ResultsTable( ) { }
void ResultsTable::AddResult( tcKernel::dataset *d )
{
	m_results.push_back( d );
}

int ResultsTable::GetNumberRows()
{
	size_t nr = 0;
	for (size_t i=0;i<m_results.size();i++)
	{
		if ( m_results[i]->values.size() > nr )
			nr = m_results[i]->values.size();
	}
	return (int)nr;
}
int ResultsTable::GetNumberCols()
{
	return m_results.size();
}
wxString ResultsTable::GetValue( int row, int col )
{
	if ( col < 0 || col >= (int)m_results.size() 
		|| row < 0 || row >= (int)m_results[col]->values.size() ) return wxEmptyString;
	tcKernel::dataitem &it = m_results[col]->values[row];
	if ( m_results[col]->type == TCS_NUMBER )
		return wxString::Format("%lf", it.dval);
	else
		return wxString( it.sval.c_str() );
}
wxString ResultsTable::GetColLabelValue( int col )
{
	if ( col < 0 || col >= (int)m_results.size() ) return wxEmptyString;
	return wxString( std::string(m_results[col]->group + "\n" + m_results[col]->name + "\n(" + m_results[col]->units + ")").c_str());
}

void ResultsTable::ReleasePointers()
{
	m_results.clear();
}

enum { ID_SIMULATE = 2324, ID_GRID, ID_VARSELECTOR, 
	ID_DVPLOT, ID_STARTTIME, ID_ENDTIME, ID_TIMESTEP, ID_MAXITER };

BEGIN_EVENT_TABLE( tcFrame, wxFrame )
	EVT_CHECKLISTBOX( ID_VARSELECTOR, tcFrame::OnSelectVar )
	EVT_CLOSE( tcFrame::OnCloseFrame )
END_EVENT_TABLE()

static tcFrame *__g_tcframe = 0;

tcFrame *tcFrame::Instance()
{
	return __g_tcframe;
}

class tcNotebook : public wxAuiNotebook
{
public:
	tcNotebook( wxWindow *parent, int id )
		: wxAuiNotebook( parent, id, wxDefaultPosition, wxDefaultSize, 
		wxAUI_NB_TOP|wxAUI_NB_SCROLL_BUTTONS|wxAUI_NB_TAB_MOVE|wxAUI_NB_TAB_SPLIT|
		wxAUI_NB_WINDOWLIST_BUTTON|wxBORDER_NONE )
	{
		m_mgr.GetArtProvider()->SetMetric(wxAUI_DOCKART_PANE_BORDER_SIZE,0);
	}
};

#ifdef _WIN64
	#define STR_BITS "64"
#elif _WIN32
	#define STR_BITS "32"
#elif __APPLE__
	#define STR_BITS "64"
#elif __linux
	#if defined(__LP64__) || defined(_LP64)
	#define STR_BITS "64"
	#else
	#define STR_BITS "32"
#endif
#else
	#error "could not determine platform architecture"
#endif

tcFrame::tcFrame()
	: wxFrame( 0, -1, wxEmptyString, wxDefaultPosition, wxSize(900,700) )
{
	__g_tcframe = this;
	SetTitle( "TCS Console (" STR_BITS " bit)" );
#ifdef __WXMSW__
	SetIcon( wxIcon("appicon") );
#endif
		
	wxSplitterWindow *split = new wxSplitterWindow( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSP_LIVE_UPDATE|wxSP_3DSASH );

	m_notebook = new wxMetroNotebook( split, wxID_ANY );

	m_visualEditor = new tcVisualEditor( m_notebook );
	m_notebook->AddPage( m_visualEditor, "Visual Editor" );

	m_scriptEditor = new tcScriptEditor( m_notebook );
	m_notebook->AddPage( m_scriptEditor, "Script Editor" );

	m_plot = new wxDVPlotCtrl( m_notebook, ID_DVPLOT );
	m_notebook->AddPage( m_plot, "Timeseries Graphs" );

	wxPanel *gpanel = new wxPanel( m_notebook );
	m_varSelector = new wxCheckListBox( gpanel, ID_VARSELECTOR );
	m_grid = new wxExtGridCtrl( gpanel, ID_GRID );	
	//m_grid->DisableDragRowSize();
	m_grid->DisableDragColMove();
	//m_grid->DisableDragGridSize();
	m_grid->SetRowLabelSize(-1);
	m_grid->SetColLabelSize(69);
	m_grid->SetDefaultCellAlignment( wxALIGN_LEFT, wxALIGN_CENTER );
	m_grid->SetRowLabelAlignment( wxALIGN_LEFT, wxALIGN_CENTER );

	wxBoxSizer *gpanel_sizer = new wxBoxSizer( wxHORIZONTAL );
	gpanel_sizer->Add( m_varSelector, 0, wxALL|wxEXPAND, 0 );
	gpanel_sizer->Add( m_grid, 1, wxALL|wxEXPAND, 0 );
	gpanel->SetSizer( gpanel_sizer );

	m_notebook->AddPage( gpanel, "Data Tables" );
	
	m_textOut = new wxTextCtrl( split, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY );


	wxBoxSizer *szmain = new wxBoxSizer(wxVERTICAL);
	szmain->Add( split, 1, wxALL|wxEXPAND, 0 );
	SetSizer(szmain);

	Layout();
	
	split->SplitHorizontally( m_notebook, m_textOut, -200 );
	split->SetSashGravity( 0.9 );
	split->SetMinimumPaneSize( 100 );
	
	Log( "Current folder: " + wxGetCwd()  + "\n\n" );
	
	m_provider.add_search_path( "." );
	m_provider.add_search_path( ".." );
	m_provider.add_search_path( "../.." );
	m_provider.add_search_path( "../../.." );
	m_provider.add_search_path( "../../../.." );
	m_provider.add_search_path( (const char*)wxGetCwd().c_str() );
		
	m_kernel = new tcKernel( this, &m_provider );

	std::vector<tcstypeprovider::typedata> list = m_provider.types();
	for (size_t i=0;i<list.size();i++)
		m_visualEditor->GetLayout()->AddType( wxString(list[i].type.c_str()), 
			list[i].info, 
			list[i].dyn ? wxString(list[i].dyn->path.c_str()) : wxString(wxEmptyString) );

	m_visualEditor->GetLayout()->CreatePopupMenu();
	m_visualEditor->UpdateTypes();

	int sel;
	wxString file;
	wxConfig cfg;
	if (cfg.Read( "visual", &file ) && !file.IsEmpty() && wxFileExists(file)) 
		m_visualEditor->LoadFile( file );
	if (cfg.Read( "script", &file ) && !file.IsEmpty() && wxFileExists(file)) 
	{
		m_scriptEditor->Load( file );
		wxSetWorkingDirectory( wxPathOnly( file ) );
	}
	if (cfg.Read( "tabsel", &sel ) && sel >= 0 && sel < (int)m_notebook->GetPageCount()) 
		m_notebook->SetSelection( sel );

}

tcFrame::~tcFrame()
{
	delete m_kernel;
}

void tcFrame::Log( const wxString &s )
{
	m_textOut->AppendText(s);
}

void tcFrame::ClearLog()
{
	m_textOut->Clear();
}


wxArrayString tcFrame::GetTypes()
{
	wxArrayString list;
	std::vector<tcstypeprovider::typedata> types = m_provider.types();
	for (size_t i=0;i<types.size();i++)
		list.Add( types[i].type.c_str() );
	return list;
}

void tcFrame::ShowTypeDataDialog( const wxString &type )
{
	std::vector<tcstypeprovider::typedata> types = m_provider.types();
	size_t i;
	for (i=0;i<types.size();i++)
		if ( wxString(types[i].type.c_str()) == type )
			break;

	if ( i == types.size() )
	{
		wxMessageBox("type " + type + " not loaded.  cannot show information.");
		return;
	}

	wxFrame *frame = new wxFrame( this, wxID_ANY, "Information for: " + type, 
		wxDefaultPosition, wxSize( 600, 500 ), 
		wxFRAME_FLOAT_ON_PARENT|wxFRAME_NO_TASKBAR|wxCAPTION|wxRESIZE_BORDER|wxCLOSE_BOX|wxMAXIMIZE_BOX|wxSYSTEM_MENU );
	tcLayoutCtrl::CreateUnitDataGrid( frame, types[i].info );
	frame->Show();
}

class tcsDVDataSet : public wxDVTimeSeriesDataSet
{
private:
	double m_start, m_step;
	tcKernel::dataset *m_pData;
public:
	tcsDVDataSet( tcKernel::dataset *d, double start, double step )
	{
		m_pData = d;
		m_start = start;
		m_step = step;
	}
	virtual wxRealPoint At(size_t i) const
	{
		if ( m_pData->type == TCS_NUMBER
			&& i < m_pData->values.size() )
		{
			return wxRealPoint( (m_start + i*m_step)/3600.0, m_pData->values[i].dval );
		}
		else return wxRealPoint( (m_start + i*m_step)/3600.0, 0.0 );
	}

	virtual size_t Length() const
	{
		return m_pData->values.size();
	}

	virtual double GetTimeStep() const
	{
		return m_step/3600.0;
	}

	virtual wxString GetSeriesTitle() const
	{
		return wxString(m_pData->name.c_str());
	}

	virtual wxString GetUnits() const
	{
		return m_pData->units.c_str();
	}

	virtual double GetOffset() const
	{
		return m_start;
	}
};

void tcFrame::ClearDataTableSelections( )
{
	for (int i=0; i<(int)m_varSelector->GetCount(); i++)
		if (m_varSelector->IsChecked(i))
			m_varSelector->Check(i,false);
	UpdateGrid();
}

void tcFrame::AddVariableToDataTable( const wxString &varname )
{
	int ndx = -1;
	for (int i=0; i<(int)m_varSelector->GetCount(); i++)
		if (m_varSelector->GetString(i).Lower().Find( varname.Lower() ) > -1)
		{
			ndx = i;
			if (!m_varSelector->IsChecked(ndx))
				m_varSelector->Check(ndx);
		}

	if ( ndx > -1) UpdateGrid();
}

int tcFrame::Simulate( double start, double end, double step, int iter, bool store_arrmat, bool proceed_anyway )
{
	wxDVPlotCtrlSettings view( m_plot->GetPerspective() );
	m_plot->RemoveAllDataSets();

	Log( wxString::Format("*** simulating [%.2lf --> %.2lf] step %.2lf maxiter %d ***\n", start, end, step, iter ) );
	
	m_kernel->set_max_iterations( iter, proceed_anyway );
	m_kernel->set_store_array_matrix_data( store_arrmat );

	wxProgressDialog progdlg("Simulation", "In progress...", 100, this, wxPD_CAN_ABORT|wxPD_AUTO_HIDE );
	progdlg.SetClientSize( 400, 130 );
	wxPoint pt = GetPosition();
	pt.x += 20;
	pt.y += 20;
	progdlg.SetPosition( pt );
	progdlg.Show();

	double time = 0.0;
	int code = m_kernel->simulate( start, end, step, &progdlg, &time );

	Log( wxString::Format("\n*** simulator kernel finished in %.3lf sec with code %d ***\n\n", time, code ) );


	wxBusyInfo info2("updating plots and data tables, please wait...");

	wxArrayString selections;
	for ( size_t i=0;i<m_varSelector->GetCount();i++ )
		if ( m_varSelector->IsChecked( i ) )
			selections.Add( m_varSelector->GetString(i) );
	
	std::vector<int> colsizes;
	for ( int i=0;i<m_grid->GetNumberCols();i++ )
		colsizes.push_back( m_grid->GetColSize( i ) );

	m_varSelector->Freeze();
	m_varSelector->Clear();
	m_plot->Freeze();
	int idx = 0;
	while( tcKernel::dataset *d = m_kernel->get_results(idx++) )
	{
		if ( d->type == TCS_NUMBER && d->values.size() > 0)
		{
			tcsDVDataSet *dvset = new tcsDVDataSet( d, start, step );
			dvset->SetGroupName( d->group );
			m_plot->AddDataSet( dvset );
		}

		wxString text = wxString(d->group.c_str()) + ":  " + wxString(d->name.c_str()) + " (" + wxString(d->units.c_str()) + ")";
		idx = m_varSelector->Append( text );
		if ( selections.Index( text ) >= 0 )
			m_varSelector->Check( idx, true );
	}
	m_plot->Thaw();
	m_varSelector->Thaw();
	m_varSelector->GetParent()->Layout();

	m_plot->SetPerspective( view );

	UpdateGrid();
	
	for ( size_t i=0;(int)i<m_grid->GetNumberCols()
		&& i < colsizes.size();i++ )
		m_grid->SetColSize( i, colsizes[i] );
	
	return code;
}

void tcFrame::UpdateGrid()
{
	ResultsTable *rt = new ResultsTable;
	int idx = 0;
	while( tcKernel::dataset *d = m_kernel->get_results(idx) )
	{
		if ( m_varSelector->IsChecked( idx ) )
			rt->AddResult( d );
		idx ++;
	}

	m_grid->SetTable( rt, true );
	m_grid->Refresh();
}

void tcFrame::OnSelectVar( wxCommandEvent & )
{
	UpdateGrid();
}


void tcFrame::OnCloseFrame( wxCloseEvent &evt )
{
	wxConfig cfg;
	cfg.Write( "script", m_scriptEditor->GetFileName());
	cfg.Write( "visual", m_visualEditor->GetFileName() );
	cfg.Write( "tabsel", m_notebook->GetSelection() );

	if ( (m_visualEditor->IsModified()
		&& wxNO==wxMessageBox("There are modifications in the visual editor.  Quit anyways?", "Query", wxYES_NO))
		|| !m_scriptEditor->CloseDoc() )
	{
		evt.Veto();
		return;
	}

	Destroy();
}





BEGIN_EVENT_TABLE( tcVisualEditor, wxPanel )	
	EVT_BUTTON( wxID_NEW, tcVisualEditor::OnAction )
	EVT_BUTTON( wxID_OPEN, tcVisualEditor::OnAction )
	EVT_BUTTON( wxID_SAVE, tcVisualEditor::OnAction )
	EVT_BUTTON( wxID_SAVEAS, tcVisualEditor::OnAction )
	EVT_BUTTON( wxID_FORWARD, tcVisualEditor::OnAction )
	EVT_BUTTON( wxID_APPLY, tcVisualEditor::OnAction )
	EVT_COMBOBOX( wxID_PROPERTIES, tcVisualEditor::OnAction )
END_EVENT_TABLE()


tcVisualEditor::tcVisualEditor( wxWindow *parent )
	: wxPanel( parent )
{
	//SetBackgroundColour( *wxWHITE );

	m_startTime = new wxNumericCtrl( this, ID_STARTTIME, 0 ); 
	m_startTime->SetFormat(2,false,wxEmptyString," hr");
	m_endTime = new wxNumericCtrl( this, ID_ENDTIME, 0 ); 
	m_endTime->SetFormat(2,false,wxEmptyString," hr");
	m_timeStep = new wxNumericCtrl( this, ID_TIMESTEP, 0 ); 
	m_timeStep->SetFormat(2,false,wxEmptyString," hr");
	m_maxIter = new wxNumericCtrl( this, ID_MAXITER, 100, wxNUMERIC_INTEGER ); 
	m_maxIter->SetFormat(-1,false,wxEmptyString, " iter");

	m_storeArrMat = new wxCheckBox(this, wxID_ANY, "Store array/matrix data from simulation" );
	m_storeArrMat->SetValue( false );
	m_proceedAnyway = new wxCheckBox( this, wxID_ANY, "Proceed even if max iterations reached" );
	m_proceedAnyway->SetValue( true );
	
	m_startTime->SetValue( 1 );
	m_endTime->SetValue( 8760 );
	m_timeStep->SetValue( 1 );
	
	wxArrayString choices;
	m_typeChoice = new wxComboBox( this, wxID_PROPERTIES, wxEmptyString, wxDefaultPosition, wxDefaultSize, choices, wxCB_READONLY );
	m_statusLabel = new wxStaticText( this, wxID_ANY, wxEmptyString );
	wxBoxSizer *sztools = new wxBoxSizer( wxHORIZONTAL );
	sztools->Add( new wxButton( this, wxID_NEW, "New" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_OPEN, "Open" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_SAVE, "Save" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_SAVEAS, "Save as" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_FORWARD, "Simulate" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_APPLY, "Script", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( m_statusLabel, 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );

	wxBoxSizer *szsetup = new wxBoxSizer( wxHORIZONTAL );
	szsetup->Add( new wxStaticText(this, wxID_ANY, "   Start:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	szsetup->Add( m_startTime, 0,wxALL|wxEXPAND, 2 );
	szsetup->Add( new wxStaticText(this, wxID_ANY, "   End:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	szsetup->Add( m_endTime, 0, wxALL|wxEXPAND, 2 );
	szsetup->Add( new wxStaticText(this, wxID_ANY, "   Step:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	szsetup->Add( m_timeStep, 0, wxALL|wxEXPAND, 2 );
	szsetup->Add( new wxStaticText(this, wxID_ANY, "   Max iter:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	szsetup->Add( m_maxIter, 0, wxALL|wxEXPAND, 2 );
	szsetup->Add( new wxStaticText(this, wxID_ANY, "   Types:"), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );
	szsetup->Add( m_typeChoice, 0, wxALL|wxEXPAND, 2 );

	
	wxBoxSizer *szopts = new wxBoxSizer( wxHORIZONTAL );
	szopts->Add( m_storeArrMat, 0, wxALL|wxEXPAND, 2 );
	szopts->Add( m_proceedAnyway, 0, wxALL|wxEXPAND, 2 );

	m_layout = new tcLayoutCtrl( this, wxID_ANY );

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( sztools, 0, wxALL|wxEXPAND, 2 );
	szmain->Add( szsetup, 0, wxALL|wxEXPAND, 2 );
	szmain->Add( szopts, 0, wxALL|wxEXPAND, 2 );
	szmain->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0 );
	szmain->Add( m_layout, 1, wxALL|wxEXPAND, 0 );
	SetSizer( szmain );
}

void tcVisualEditor::UpdateTypes()
{
	m_typeChoice->Clear();
	m_typeChoice->Append( tcFrame::Instance()->GetTypes() );
}

bool tcVisualEditor::LoadFile( const wxString &file )
{
	wxFFileInputStream fis( file );
	if ( !fis.IsOk() )
	{
		wxMessageBox("Could not open file for reading:\n\n" + file);
		return false;
	}

	wxBusyInfo info("Loading data file...");
	wxYield();
	if (!m_layout->Read( fis ))
	{
		wxMessageBox("Error reading: " + m_layout->GetError() );
		return false;
	}
	else
	{
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		m_layout->SetModified(false);
		return true;
	}
}

void tcVisualEditor::OnAction( wxCommandEvent &evt )
{
	if (evt.GetId() == wxID_OPEN )
	{
		wxFileDialog dlg( this, "Open File", wxPathOnly(m_fileName), m_fileName, "TCS Files (*.tcs)|*.tcs", wxFD_OPEN );
		if (dlg.ShowModal() != wxID_OK) return;
	
		LoadFile( dlg.GetPath() );
	}
	else if (evt.GetId() == wxID_NEW )
	{
		if (m_layout->IsModified()
			&& wxNO==wxMessageBox("The system has been changed.  Erase anyways?", "Query", wxYES_NO))
			return;

		m_layout->Clear();
		m_layout->Refresh();
		m_layout->SetModified(false);
		m_fileName = "";
		m_statusLabel->SetLabel( wxEmptyString );
	}
	else if (evt.GetId() == wxID_SAVE )
	{
		wxString file = m_fileName;

		if ( file.IsEmpty() )
		{
			wxFileDialog dlg( this, "Save File", wxPathOnly(m_fileName), m_fileName,
				"TCS Files (*.tcs)|*.tcs", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );

			if (dlg.ShowModal() != wxID_OK) return;
			file = dlg.GetPath();
		}

		WriteToDisk( file );
	}
	else if (evt.GetId() == wxID_SAVEAS)
	{
		wxFileDialog dlg( this, "Save File", wxPathOnly(m_fileName), m_fileName,
			"TCS Files (*.tcs)|*.tcs", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );

		if (dlg.ShowModal() != wxID_OK) return;
		WriteToDisk( dlg.GetPath() );
	}
	else if (evt.GetId() == wxID_FORWARD )
	{
		if (!m_fileName.IsEmpty())
		{
			wxFFileOutputStream fos( m_fileName + "~" );
			if (fos.IsOk())	m_layout->Write( fos );
		}

		ClearLog();
		tcKernel *kern = tcFrame::Instance()->GetKernel();
		if (!m_layout->LoadSystemInKernel( kern ))
		{
			wxMessageBox("Error loading system into kernel:\n\n" + m_layout->GetError());
			return;
		}

		std::string nl = kern->netlist();
		Log(nl.c_str());
		
		tcFrame::Instance()->Simulate( m_startTime->AsDouble() * 3600.0,
			m_endTime->AsDouble() * 3600.0,
			m_timeStep->AsDouble() * 3600.0,
			m_maxIter->AsInteger(),
			m_storeArrMat->GetValue(),
			m_proceedAnyway->GetValue() );
	}
	else if (evt.GetId() == wxID_APPLY)
	{
		if (wxTheClipboard->Open())
		{
			wxTheClipboard->SetData( new wxTextDataObject( m_layout->GetLKScript() ) );
			wxTheClipboard->Close();
			wxBusyInfo info( "Script code representing the system was copied to the clipboard.  You may paste it into the editor.");
			wxMilliSleep(1000);
		}
	}
	else if ( evt.GetId() == wxID_PROPERTIES )
	{
		tcFrame::Instance()->ShowTypeDataDialog( m_typeChoice->GetStringSelection() );
	}
}

bool tcVisualEditor::WriteToDisk( const wxString &file )
{	
	wxFFileOutputStream fos( file );
	if (!fos.IsOk())
	{
		wxMessageBox("Could not open file for writing:\n\n" + file);
		return false;
	}

	if (! m_layout->Write( fos ))
	{
		wxMessageBox("Error writing: " + m_layout->GetError());
		return false;
	}
	else
	{
		tcFrame::Instance()->Log( "Visual editor wrote to disk: " + file + " (" + wxNow() + ")\n" );
		m_fileName = file;
		m_layout->SetModified(false);
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
}

bool tcVisualEditor::IsModified()
{
	return m_layout->IsModified();
}










bool tcApp::OnInit() {
	wxInitAllImageHandlers();

	SetAppName( "TCS Console" );
	SetVendorName( "NREL" );

	tcFrame *f = new tcFrame;
	f->Show();
	f->SetClientSize(1000, 700 );
	SetTopWindow(f);
	return true;
}

IMPLEMENT_APP( tcApp );
