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


#include <wx/wx.h>
#include <wx/imaglist.h>
#include <wx/splitter.h>
#include <wx/filename.h>
#include <wx/statline.h>
#include <wx/html/htmlwin.h>
#include <wx/tokenzr.h>
#include <wx/busyinfo.h>

#include <wx/stc/stc.h>

#include <wex/lkscript.h>
#include <wex/dview/dvplotctrl.h>
#include <wex/dview/dvtimeseriesdataset.h>

#include <lk/lex.h>
#include <lk/parse.h>
#include <lk/eval.h>
#include <lk/invoke.h>
#include <lk/stdlib.h>

#include "sscdev.h"
#include "dataview.h"
#include "scripting.h"

void Output( const wxString &text )
{
	app_frame->Log( text, false );
}

void Output( const char *fmt, ... )
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
	Output( wxString(buf) );	
}

void ClearOutput()
{
	app_frame->ClearLog();
}


static bool sscvar_to_lkvar( lk::vardata_t &out, var_data *vv)
{
	if (!vv) return false;

	switch( vv->type )
	{
	case SSC_NUMBER:
		out.assign( (double) vv->num );
		break;
	case SSC_STRING:
		out.assign( vv->str.c_str() );
		break;
	case SSC_ARRAY:
		out.empty_vector();
		out.vec()->reserve( (size_t) vv->num.length() );
		for (size_t i=0;i<vv->num.length();i++)
			out.vec_append( vv->num[i] );
		break;
	case SSC_MATRIX:
		out.empty_vector();
		out.vec()->reserve( vv->num.nrows() );
		for (size_t i=0;i<vv->num.nrows();i++)
		{
			out.vec()->push_back( lk::vardata_t() );
			out.vec()->at(i).empty_vector();
			out.vec()->at(i).vec()->reserve( vv->num.ncols() );
			for (size_t j=0;j<vv->num.ncols();j++)
				out.vec()->at(i).vec_append( vv->num.at(i,j) );
		}
		break;
	case SSC_TABLE:
		{
			out.empty_hash();
			const char *key = vv->table.first();
			while (key != 0)
			{
				var_data *x = vv->table.lookup( key );
				lk::vardata_t &xvd = out.hash_item( lk_string(key) );
				sscvar_to_lkvar( xvd, x );
				key = vv->table.next();
			}
		}
		break;
	}

	return true;
}

static bool lkvar_to_sscvar( var_data *vv, lk::vardata_t &val )
{	
	if (!vv) return false;

	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		vv->type = SSC_NUMBER;
		vv->num = (ssc_number_t)val.as_number();
		break;
	case lk::vardata_t::STRING:
		vv->type = SSC_STRING;
		vv->str = std::string((const char*) val.as_string().c_str());
		break;
	case lk::vardata_t::VECTOR:
		{
			size_t dim1 = val.length(), dim2 = 0;
			for (size_t i=0;i<val.length();i++)
			{
				lk::vardata_t *row = val.index(i);
				if (row->type() == lk::vardata_t::VECTOR && row->length() > dim2 )
					dim2 = row->length();
			}

			if (dim2 == 0 && dim1 > 0)
			{
				vv->type = SSC_ARRAY;
				vv->num.resize( dim1 );
				for ( size_t i=0;i<dim1;i++)
					vv->num[i] = (ssc_number_t)val.index(i)->as_number();
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				vv->type = SSC_MATRIX;
				vv->num.resize( dim1, dim2 );
				for ( size_t i=0;i<dim1;i++)
				{
					for ( size_t j=0;j<dim2;j++ )
					{
						double x = 0;
						if ( val.index(i)->type() == lk::vardata_t::VECTOR
							&& j < val.index(i)->length() )
							x = (ssc_number_t)val.index(i)->index(j)->as_number();

						vv->num.at(i,j) = x;
					}
				}
			}
		}
		break;
	case lk::vardata_t::HASH:		
		{
			vv->type = SSC_TABLE;
			vv->table.clear();

			lk::varhash_t &hash = *val.hash();
			for ( lk::varhash_t::iterator it = hash.begin();
				it != hash.end();
				++it )
			{
				var_data *item = vv->table.assign( std::string( (const char*)(*it).first.c_str() ), var_data() );
				lkvar_to_sscvar( item, *(*it).second );
			}
		}
		break;
	}

	return true;
}

void fcall_var( lk::invoke_t &cxt )
{
	LK_DOC2( "var", "Sets or gets a variable value in the SSC data set.", 
		"Set a variable value.", "(string:name, variant:value):none", 
		"Get a variable value", "(string:name):variant" );
	
	var_table *vt = app_frame->GetVarTable();

	wxString name = cxt.arg(0).as_string();
	if (cxt.arg_count() == 1)
	{
		var_data *vv = vt->lookup( name.ToStdString() );
		if (vv)	sscvar_to_lkvar( cxt.result(), vv );
	}
	else if (cxt.arg_count() == 2)
	{
		lk::vardata_t &val = cxt.arg(1).deref();		
		var_data *vv = vt->assign( name.ToStdString(), var_data() ); // create empty variable
		if (vv)
		{
			lkvar_to_sscvar( vv, val );		
			app_frame->GetDataView()->UpdateView();
		}
	}
}


void fcall_clear( lk::invoke_t &cxt )
{
	LK_DOC( "clear", "Deletes variables from the SSC data set.  If no variable name(s) are specified, all are deleted.", "([string or array:variable name(s) to delete]):none");
	
	if (cxt.arg_count() > 0)
	{
		if (cxt.arg(0).type() == lk::vardata_t::VECTOR)
		{
			size_t len = cxt.arg(0).length();
			for (size_t i=0;i<len;i++)
				app_frame->GetVarTable()->unassign( 
					(const char*) cxt.arg(0).index(i)->as_string().c_str() );
		}
		else
			app_frame->GetVarTable()->unassign( (const char*) cxt.arg(0).as_string().c_str() );
	}
	else
		app_frame->GetVarTable()->clear();

	app_frame->GetDataView()->UpdateView();
}

void fcall_save( lk::invoke_t &cxt )
{
	LK_DOC( "save", "Save the current variable data set to disk in the SSCdev binary data (*.bdat) format.", "(string:filename):boolean" );
	cxt.result().assign( app_frame->WriteBdatToDisk( cxt.arg(0).as_string() ) );
}

void fcall_load( lk::invoke_t &cxt )
{
	LK_DOC( "load", "Load a variable data set from an SSCdev binary data (*.bdat) file.", "(string:filename):boolean" );
	cxt.result().assign( app_frame->LoadBdat( cxt.arg(0).as_string() ) );
}

void fcall_run( lk::invoke_t &cxt )
{
	LK_DOC( "run", 
		"Starts the computation sequence defined.  If no parameter is given, it runs the currently defined list of compute modules. "
		"Passing a comma-separated list of compute module names changes the list.", 
		"([string:compute modules list]):array of booleans");

	if (cxt.arg_count() > 0)
		app_frame->SetCurrentCM( cxt.arg(0).as_string() );

	std::vector<bool> ok = app_frame->Start();
	cxt.result().empty_vector();
	for (size_t i=0;i<ok.size();i++)
		cxt.result().vec_append( ok[i] ? 1 : 0 );
}

void fcall_tsview( lk::invoke_t &cxt )
{
	LK_DOC( "tsview", "Show a timeseries viewer for the variables given in the comma-separated list, or plots the name-data pairs sent as arguments.  Variable must have 8760 values.", "(string:comma-separated variable name list -or- string:name1, array:values1,...):none");
	
	wxFrame *frm = new wxFrame(app_frame, -1, "Timeseries Viewer", wxDefaultPosition, wxSize(900,600));
	wxDVPlotCtrl *dv = new wxDVPlotCtrl( frm );
	var_table *vt = app_frame->GetVarTable();	
	int iadded = 0;	
	std::vector<double> da(8760);

	if (cxt.arg_count() == 1)
	{
		wxArrayString selections = wxStringTokenize(cxt.arg(0).as_string(), ",");

		for (size_t i=0;i<selections.Count();i++)
		{
			var_data *v = vt->lookup( (const char*) selections[i].c_str() );
			if ( v != 0
				&& v->type == SSC_ARRAY
				&& v->num.length() == 8760)
			{
				for (int k=0;k<8760;k++)
					da[k] = v->num[k];

				dv->AddDataSet(  new wxDVArrayDataSet( selections[i], da ) );
				iadded++;
			}
		}
	}
	else
	{
		for (size_t i=1;i<cxt.arg_count();i+=2)
		{
			wxString name = cxt.arg(i-1).as_string();
			wxString units;

			wxString::size_type lpos = name.Find('(');
			wxString::size_type rpos = name.Find(')');
			if (lpos != wxString::npos
				&& rpos != wxString::npos
				&& rpos > lpos)
			{
				units = name.Mid( lpos+1, rpos-lpos-1 );
				name.Truncate(lpos);
				name.Trim();
			}

			if (cxt.arg(i).type() == lk::vardata_t::VECTOR
				&& cxt.arg(i).length() == 8760)
			{
				for (int k=0;k<8760;k++)
					da[k] = cxt.arg(i).index(k)->as_number();
				
				dv->AddDataSet(  new wxDVArrayDataSet( name, units, 1.0, da ) );
				iadded++;
			}
		}
	}
			
	if (iadded == 0)
	{
		frm->Destroy();
		cxt.result().assign( 0.0 );
	}
	else
	{
		dv->SelectDataOnBlankTabs();
		frm->Show();
		cxt.result().assign( (double)iadded );
	}
}

void fcall_freeze( lk::invoke_t &cxt )
{
	LK_DOC( "freeze", "Freeze the data view for improved processing speed", "(none):none");
	app_frame->GetDataView()->Freeze();
}

void fcall_thaw( lk::invoke_t &cxt )
{
	LK_DOC( "thaw", "Thaw the data view for to restore interactivity", "(none):none");
	app_frame->GetDataView()->Thaw();
}
lk::fcall_t* ssc_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_var,
		fcall_clear,
		fcall_run,
		fcall_save,
		fcall_load,
		fcall_tsview,
		fcall_freeze,
		fcall_thaw,
		0 };
		
	return (lk::fcall_t*)vec;
}


enum { ID_CODEEDITOR = wxID_HIGHEST+1, ID_RUN, ID_HELP };

class MyScriptCtrl : public wxLKScriptCtrl
{
public:
	MyScriptCtrl( wxWindow *parent, int id = wxID_ANY )
		: wxLKScriptCtrl( parent, id, wxDefaultPosition, wxDefaultSize, wxLK_STDLIB_ALL|wxLK_STDLIB_SOUT )
	{
	}
	
	virtual void OnOutput( const wxString &tt )
	{
		Output( tt );
	}
	virtual void OnSyntaxCheck(int, const wxString &err)
	{
		ClearOutput();
		Output( err );
	}
};

EditorWindow::EditorWindow( wxWindow *parent )
		: wxPanel( parent )
{
			
	wxBoxSizer *szdoc = new wxBoxSizer( wxHORIZONTAL );
	szdoc->Add( new wxButton( this, wxID_NEW, "New" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_OPEN, "Open" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_SAVE, "Save" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_SAVEAS, "Save as" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_FIND, "Find" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, wxID_FORWARD, "Find next" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, ID_HELP, "Help" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( new wxButton( this, ID_RUN, "Run" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	szdoc->Add( m_stopButton = new wxButton( this, wxID_STOP, "Stop" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );	
	szdoc->AddStretchSpacer();
	m_stopButton->SetForegroundColour( *wxRED );
	m_stopButton->Hide();
					
	m_editor = new MyScriptCtrl(this, ID_CODEEDITOR );
	
	m_editor->RegisterLibrary( ssc_funcs(), "SSC Functions", this );	

	wxBoxSizer *szedit = new wxBoxSizer(wxVERTICAL);
	szedit->Add(szdoc, 0, wxALL | wxEXPAND, 2);
	szedit->Add( m_editor, 1, wxALL|wxEXPAND, 0 );
	szedit->Add( m_statusLabel = new wxStaticText( this, wxID_ANY, wxEmptyString ), 0, wxALL|wxEXPAND, 0 );

	SetSizer(szedit);
		
	m_editor->SetFocus();
}

EditorWindow::~EditorWindow()
{
}

void EditorWindow::OnCommand( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_NEW:
		CloseDoc();
		break;
	case wxID_OPEN:
		Open();
		break;
	case wxID_SAVE:
		Save();
		break;
	case wxID_SAVEAS:
		SaveAs();
		break;
	case wxID_UNDO: m_editor->Undo(); break;
	case wxID_REDO: m_editor->Redo(); break;
	case wxID_CUT: m_editor->Cut(); break;
	case wxID_COPY: m_editor->Copy(); break;
	case wxID_PASTE: m_editor->Paste(); break;
	case wxID_SELECTALL: m_editor->SelectAll(); break;		
	case wxID_FIND:
		m_editor->ShowFindReplaceDialog(); break;
	case wxID_FORWARD:
		m_editor->FindNext(); break;
	case ID_HELP:
		m_editor->ShowHelpDialog( this );
		break;
	case ID_RUN:
		Exec();
		break;
	case wxID_STOP:
		m_editor->Stop();
		m_stopButton->Hide();
		Layout();
		break;
	}
}
	
void EditorWindow::Open()
{
	CloseDoc();
	wxFileDialog dlg(this, "Open", wxEmptyString, wxEmptyString,
							"LK Script Files (*.lk)|*.lk",
							wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_CHANGE_DIR);

	if (dlg.ShowModal() == wxID_OK)
		if (!Load( dlg.GetPath() ))
			wxMessageBox("Could not load file:\n\n" + dlg.GetPath());
}

bool EditorWindow::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool EditorWindow::SaveAs()
{
	wxFileDialog dlg( this, "Save as...",
		wxPathOnly(m_fileName),
		wxFileNameFromPath(m_fileName),
		"LK Script Files (*.lk)|*.lk", wxFD_SAVE|wxFD_OVERWRITE_PROMPT );
	if (dlg.ShowModal() == wxID_OK)
		return Write( dlg.GetPath() );
	else
		return false;
}

	
bool EditorWindow::CloseDoc()
{
	if ( m_editor->IsScriptRunning() )
	{
		if (wxYES==wxMessageBox("A script is running. Cancel it?", "Query", wxYES_NO))
			m_editor->Stop();

		return false;
	}

	if (m_editor->GetModify())
	{
		Raise();
		wxString id = m_fileName.IsEmpty() ? "untitled" : m_fileName;
		int result = wxMessageBox("Script modified. Save it?\n\n" + id, "Query", wxYES_NO|wxCANCEL);
		if ( result == wxCANCEL 
			|| (result == wxYES && !Save()) )
			return false;
	}

	m_editor->SetText( wxEmptyString );
	m_editor->EmptyUndoBuffer();
	m_editor->SetSavePoint();
	m_fileName.Clear();
	m_statusLabel->SetLabel( m_fileName );
	return true;
}

bool EditorWindow::Write( const wxString &file )
{
	wxBusyInfo info( "Saving script file...");
	wxMilliSleep( 120 );

	if ( ((wxStyledTextCtrl*)m_editor)->SaveFile( file ) )
	{
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
	else return false;
}


bool EditorWindow::Load( const wxString &file )
{
	FILE *fp = fopen(file.c_str(), "r");
	if ( fp )
	{
		wxString str;
		char buf[1024];
		while(fgets( buf, 1023, fp ) != 0)
			str += buf;

		fclose(fp);
		m_editor->SetText(str);
		m_editor->EmptyUndoBuffer();
		m_editor->SetSavePoint();
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
	else return false;
}
	
void EditorWindow::Exec()
{
	ClearOutput();
	m_stopButton->Show();
	Layout();
	wxGetApp().Yield(true);
	
	wxLKSetToplevelParentForPlots( app_frame );
	wxLKSetPlotTarget( NULL );

	
	wxString work_dir;
	if( !m_fileName.IsEmpty() )
		work_dir = wxPathOnly(m_fileName);

	m_editor->SetWorkDir( work_dir );

	m_editor->Execute( );

	if ( m_stopButton->IsShown() )
	{
		m_stopButton->Hide();
		Layout();
	}
}

BEGIN_EVENT_TABLE( EditorWindow, wxPanel )
	EVT_BUTTON( wxID_NEW, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_OPEN, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_SAVE, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_SAVEAS, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_HELP, EditorWindow::OnCommand )

	EVT_BUTTON( wxID_FIND, EditorWindow::OnCommand )
	EVT_BUTTON( wxID_FORWARD, EditorWindow::OnCommand )

	
	EVT_BUTTON( ID_RUN, EditorWindow::OnCommand )

	EVT_BUTTON( wxID_STOP, EditorWindow::OnCommand )
	EVT_BUTTON( ID_RUN, EditorWindow::OnCommand )
	EVT_BUTTON( ID_HELP, EditorWindow::OnCommand )

END_EVENT_TABLE()


