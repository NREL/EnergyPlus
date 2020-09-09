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
#include <wx/statline.h>
#include <wx/html/htmlwin.h>

#include <wex/lkscript.h>
#include <wex/utils.h>

#include "tcslayout.h"
#include "tcsscript.h"
#include "tcsmain.h"

void fcall_clear(  lk::invoke_t &cxt )
{
	LK_DOC("clear", "Clear all units and connections.", "(none):string");
	tcFrame::Instance()->GetKernel()->clear_units();

}

void fcall_add_unit(  lk::invoke_t &cxt )
{
	LK_DOC("add_unit", "Add a unit to the simulator.", "(string:type name, [string:description]):integer");	
	wxString name = cxt.arg(0).as_string(), desc;
	if ( cxt.arg_count() > 1 ) desc = cxt.arg(1).as_string();
	int unit = tcFrame::Instance()->GetKernel()->add_unit( (const char*)name.c_str(), (const char*)desc.c_str() );
	cxt.result().assign( unit );	
}

void fcall_set_value( lk::invoke_t &cxt )
{
	LK_DOC("set_value", "Set a variable value for a unit.  Values can be a number, string, array, or 2D array.", "(integer:unit, string:variable name, any:value):void" );

	int u = cxt.arg(0).as_integer();
	wxString var = cxt.arg(1).as_string();

	tcskernel *kern = tcFrame::Instance()->GetKernel();

	lk::vardata_t &val = cxt.arg(2).deref();
	switch (val.type())
	{
	case lk::vardata_t::NUMBER:
		kern->set_unit_value( u, var.c_str(), (double) val.as_number() );
		break;
	case lk::vardata_t::STRING:
		kern->set_unit_value( u, var.c_str(), (const char*) val.as_string().c_str() );
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
				double *p = new double[ dim1 ];
				for ( size_t i=0;i<dim1;i++)
					p[i] = val.index(i)->as_number();

				kern->set_unit_value( u, var.c_str(), p, dim1 );
				delete [] p;
			}
			else if ( dim1 > 0 && dim2 > 0 )
			{
				double *p = new double[ dim1 * dim2 ];
				for ( size_t i=0;i<dim1;i++)
				{
					for ( size_t j=0;j<dim2;j++ )
					{
						double x = 0;
						if ( val.index(i)->type() == lk::vardata_t::VECTOR
							&& j < val.index(i)->length() )
							x = val.index(i)->index(j)->as_number();

						p[ dim2*i +j ] = x;
					}
				}
				kern->set_unit_value( u, var.c_str(), p, dim1, dim2 );
				delete [] p;
			}
		}
		break;
	}
}

void fcall_get_result( lk::invoke_t &cxt )
{
	LK_DOC("get_result", "Obtain the values of for a particular variable at all timesteps after a simulation.", "(integer:unit, string:variable name):array");

	int unit = cxt.arg(0).as_integer();
	wxString var( cxt.arg(1).as_string().c_str() );
	
	tcKernel *kern = tcFrame::Instance()->GetKernel();
	tcKernel::dataset *ds = 0;
	int idx = 0;
	while ( (ds = kern->get_results( idx++ )) != 0 )
	{
		if (ds->uidx == unit && wxString(ds->name.c_str()) == var)
		{
			cxt.result().empty_vector();
			cxt.result().vec()->reserve( ds->values.size() );

			if ( ds->type == TCS_NUMBER )
			{
				for (size_t j=0;j<ds->values.size();j++)
					cxt.result().vec_append( ds->values[j].dval );
			}
			else
			{
				for (size_t j=0;j<ds->values.size();j++)
					cxt.result().vec_append( lk_string(ds->values[j].sval.c_str()) );
			}

			return;
		}
	}

}

void fcall_connect( lk::invoke_t &cxt )
{
	LK_DOC("connect", "Make a connection from an output to an input.", "(integer:unit1, string:output name, integer:unit2, string:input name, [number:ftol=0.1], [number:arridx=-1]):boolean" );
	int u1 = cxt.arg(0).as_integer();
	wxString out = cxt.arg(1).as_string();
	int u2 = cxt.arg(2).as_integer();
	wxString in = cxt.arg(3).as_string();
	double ftol = 0.1;
	int arridx = -1;
	if (cxt.arg_count() > 4) ftol = cxt.arg(4).as_number();
	if (cxt.arg_count() > 5) arridx = cxt.arg(5).as_number();

	bool ok = tcFrame::Instance()->GetKernel()->connect( u1, out.c_str(), u2, in.c_str(), ftol, arridx );
	cxt.result().assign( ok );
}

void fcall_simulate(  lk::invoke_t &cxt )
{
	LK_DOC("simulate", "Run a simulation in the given time range specified in hours.", "(number:start, number:end, number:step, [number:maxiter=100], [boolean:store array,matrix data vars], [boolean:proceed even if max iter reached]):number");
	double start = cxt.arg(0).as_number() * 3600.0;
	double end = cxt.arg(1).as_number() * 3600.0;
	double step = cxt.arg(2).as_number() * 3600.0;
	int maxiter = 100;
	bool store_arrmat = false;
	bool proceed_anyway = true;

	if ( cxt.arg_count() > 3 ) maxiter = cxt.arg(3).as_integer();
	if ( cxt.arg_count() > 4 ) store_arrmat = cxt.arg(4).as_boolean();
	if ( cxt.arg_count() > 5 ) proceed_anyway = cxt.arg(5).as_boolean();

	int code = tcFrame::Instance()->Simulate( start, end, step, maxiter, store_arrmat, proceed_anyway );

	cxt.result().assign( code );	
}

void fcall_netlist( lk::invoke_t &cxt )
{
	LK_DOC("netlist", "Generate a netlist description of the current configuration.", "(void):string");
	cxt.result().assign( std::string(tcFrame::Instance()->GetKernel()->netlist()).c_str() );
}

void fcall_open_visual( lk::invoke_t &cxt )
{
	LK_DOC("open_visual", "Open a .tcs file in the visual editor.  Does not check for existing modifications before overwriting the visual editors current contents.", "(string:file name):boolean");
	cxt.result().assign( tcFrame::Instance()->GetVisualEditor()->LoadFile( wxString(cxt.arg(0).as_string().c_str()) ) );
}

void fcall_load_visual( lk::invoke_t &cxt )
{
	LK_DOC("load_visual", "Loads the current system in the visual editor into the kernel.", "(none):boolean");
	cxt.result().assign( tcFrame::Instance()->GetVisualEditor()->GetLayout()->LoadSystemInKernel(
		tcFrame::Instance()->GetKernel() ) );
}

void fcall_timevec( lk::invoke_t &cxt )
{
	LK_DOC("timevec", "Returns a time vector array specified by the parameters.", "(real:start, real:stop, real:step):array");
	double start = cxt.arg(0).as_number();
	double end = cxt.arg(1).as_number();
	double step = 1;
	if ( cxt.arg_count() > 2 ) step = cxt.arg(2).as_number();

	if (step <= 0) return;
	if (end <= start) return;

	cxt.result().empty_vector();
	
	for ( double time = start; time <= end; time += step )
		cxt.result().vec_append( time );
}


void fcall_datatablevariable( lk::invoke_t &cxt )
{
	LK_DOC("datatablevariable", "Add specified variable name to the data table.", "(string:varname):void");

	if ( cxt.arg_count() > 0)
	{
		tcFrame::Instance()->AddVariableToDataTable(cxt.arg(0).as_string());
	}
}

void fcall_cleardatatableselections( lk::invoke_t &cxt )
{
	LK_DOC("cleardatatableselections", "Clear the selected variables in the data table.", "(string:varname):void");

	tcFrame::Instance()->ClearDataTableSelections();
}

#include <wx/thread.h>


class ThreadProgressDialog : public wxDialog
{
	DECLARE_EVENT_TABLE()

	bool m_canceled;
	std::vector<wxGauge*> m_progbars;
	std::vector<wxTextCtrl*> m_percents;

	wxTextCtrl *m_log;

public:
	bool IsCanceled()
	{		
		return m_canceled;
	}

	void Log( const wxArrayString &list )
	{
		for (size_t i=0;i<list.Count();i++)
			Log(list[i]);
	}


	void Log( const wxString &text )
	{
		m_log->AppendText( text + "\n" );
	}

	void Update(int ThreadNum, float percent)
	{
		if (ThreadNum >= 0 && ThreadNum < (int)m_progbars.size())
		{
			m_progbars[ThreadNum]->SetValue( (int)percent );
			m_percents[ThreadNum]->SetValue( wxString::Format("%.1f %%", percent) );
		}
	}

	ThreadProgressDialog(wxWindow *parent, int nthreads)
		: wxDialog( parent, wxID_ANY, wxString("Thread Progress"), wxDefaultPosition, 
		wxSize(700, 600), wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
	{
		m_canceled = false;
		wxButton *btnCancel = new wxButton(this, wxID_CANCEL, "Cancel", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);

		wxBoxSizer *szv = new wxBoxSizer(wxVERTICAL);

		for (int i=0;i<nthreads;i++)
		{
			wxBoxSizer *sizer = new wxBoxSizer( wxHORIZONTAL );
			sizer->Add( new wxStaticText(this, wxID_ANY, wxString::Format("thread %d", i)), 0, wxALL|wxALIGN_CENTER_VERTICAL, 3 );
			
			wxGauge *gauge = new wxGauge(this, wxID_ANY, 100);
			wxTextCtrl *text = new wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_READONLY);

			sizer->Add( gauge, 1, wxALL|wxEXPAND, 3 );
			sizer->Add( text, 0, wxALL|wxEXPAND, 3 );

			m_progbars.push_back(gauge);
			m_percents.push_back(text);
			
			szv->Add( sizer, 0, wxEXPAND|wxALL, 5 );
		}

		szv->Add( new wxStaticLine(this), 0, wxEXPAND|wxALL, 4 );


		m_log = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE );
		szv->Add( m_log, 1, wxALL|wxEXPAND, 3 );

		wxBoxSizer *szh = new wxBoxSizer( wxHORIZONTAL );
		szh->AddStretchSpacer();
		szh->Add( btnCancel, 0, wxALIGN_CENTER_VERTICAL|wxALL, 2);
		szv->Add( szh, 0, wxEXPAND|wxALL, 4 );

		SetSizer(szv);
	}
	
	void OnCancel(wxCommandEvent &)
	{
		m_canceled = true;
	}

	void OnDialogClose(wxCloseEvent &)
	{
		m_canceled = true;
	}
};

BEGIN_EVENT_TABLE( ThreadProgressDialog, wxDialog )
	EVT_BUTTON( wxID_CANCEL, ThreadProgressDialog::OnCancel )
	EVT_CLOSE( ThreadProgressDialog::OnDialogClose )
END_EVENT_TABLE( )

class ThreadedKernel : public wxThread, public tcskernel
{
	int m_threadId;
	double m_start, m_end, m_step;
	bool m_proceedAnyways;
	int m_maxIter, m_simResult;
	bool m_ok;


	wxMutex m_runLock, m_cancelLock, m_percentLock, m_logLock;
	bool m_running, m_canceled;
	float m_percent;
	wxArrayString m_messages;
public:
	ThreadedKernel( tcskernel *setup, tcstypeprovider *prov, int thread_id, 
		double start, double end, double step, bool, bool proceed, int maxiter )
		: wxThread( ::wxTHREAD_JOINABLE ), tcskernel( prov ),
		m_threadId(thread_id), m_start(start), m_end(end), m_step(step),
		m_proceedAnyways(proceed), m_maxIter(maxiter), m_simResult(0)
	{
		m_canceled = false;
		m_percent = 0.0;
		m_running = false;
		m_ok = false;
		if ( copy( *setup ) == 0 )
			m_ok = true;

	}

	virtual ~ThreadedKernel()
	{
	}

	float GetPercent()
	{
		return m_percent;
	}

	int GetId() { return m_threadId; }

	void Cancel()
	{
		wxMutexLocker _lock(m_cancelLock);
		m_canceled = true;
	}
	

	virtual void log( const std::string &text )
	{
		wxMutexLocker _lock(m_logLock);
		m_messages.Add( wxString::Format("thread %d: ", m_threadId) + wxString(text) );
	}

	virtual bool converged( double time )
	{
		if (m_step != 0.0 )
		{
			int istep = (int) (time-m_start)/m_step;
			int nstep = (int) (m_end-m_start)/m_step;
			int nnsteps = nstep/400;
			if ( nnsteps == 0 ) nnsteps = 1;		
			if (istep % nnsteps == 0)
			{
				wxMutexLocker _lock(m_percentLock);
				m_percent = (float)(100 * ((double)istep) / ((double)nstep) );
			}
		}
		
		return !m_canceled;
	}

	wxArrayString GetNewMessages()
	{
		wxMutexLocker _lock(m_logLock);
		wxArrayString list = m_messages;
		m_messages.Clear();
		return list;
	}

	bool IsOk() { return m_ok; }
	bool IsRunning() { wxMutexLocker _lock(m_runLock); return m_running; }

	virtual void *Entry()
	{
		if ( !m_ok ) return NULL;
		m_canceled = false;
		m_running = true;
		set_max_iterations( m_maxIter, m_proceedAnyways );		
		m_simResult = simulate( m_start, m_end, m_step );
		m_running = false;
		return NULL;
	}

};


void fcall_parallel( lk::invoke_t &cxt )
{
	LK_DOC("parallel", "Runs parallel simulations as defined by a parametric table.", "(array:parametric tables, string:outputs desired, number:start, number:end, number:step, [number:maxiter=100], [boolean:store array,matrix data vars], [boolean:proceed even if max iter reached], [number:maxcpus]):array");
	
	double start = cxt.arg(2).as_number() * 3600.0;
	double end = cxt.arg(3).as_number() * 3600.0;
	double step = cxt.arg(4).as_number() * 3600.0;
	int maxiter = 100;
	bool store_arrmat = false;
	bool proceed_anyway = true;
	int nthread = -1;

	if ( cxt.arg_count() > 5 ) maxiter = cxt.arg(5).as_integer();
	if ( cxt.arg_count() > 6 ) store_arrmat = cxt.arg(6).as_boolean();
	if ( cxt.arg_count() > 7 ) proceed_anyway = cxt.arg(7).as_boolean();
	if ( cxt.arg_count() > 8 ) nthread = cxt.arg(8).as_integer();

	if ( nthread < 1 )
		nthread = wxThread::GetCPUCount();

	ThreadProgressDialog tpd( tcFrame::Instance(), nthread );
	tpd.Show();

	wxStopWatch sw;

	std::vector<ThreadedKernel*> threads;
	for( int i=0;i<nthread;i++)
	{
		ThreadedKernel *t = new ThreadedKernel( tcFrame::Instance()->GetKernel(), tcFrame::Instance()->GetTypeProvider(),
			i, start, end, step, store_arrmat, proceed_anyway, maxiter );
		threads.push_back( t );
		t->Create();
	}

	// now change the parametrics values
	lk::vardata_t &parlist = cxt.arg(0);
	if ( parlist.type() == lk::vardata_t::VECTOR )
	{
		for ( int i=0;i<(int)parlist.length() && i < nthread;i++ )
		{
			lk::vardata_t &vallist = *parlist.index(i);

			if ( vallist.type() == lk::vardata_t::VECTOR )
			{
				for ( size_t j=0;j<vallist.length();j++ )
				{
					lk::vardata_t &tab = *vallist.index(j);

					lk::vardata_t *unit = tab.lookup( "unit" );
					lk::vardata_t *var = tab.lookup( "variable" );
					lk::vardata_t *val = tab.lookup( "value" );

					if (  unit != 0 && var != 0 && val != 0 /*
						&& unit->type() == lk::vardata_t::NUMBER
						&& var->type() == lk::vardata_t::STRING */ )
					{
						int uid = unit->as_integer();
						lk_string varid = var->as_string();

						switch ( val->type() )
						{
						case lk::vardata_t::NUMBER:
							threads[i]->set_unit_value( uid, (const char*)varid.c_str(), val->as_number() );
							tcFrame::Instance()->Log( wxString::Format("thread %d unit %d variable '%s' = %d\n",
								i, uid, (const char*)varid.c_str(), val->as_number() ) );
							break;
						case lk::vardata_t::STRING:
							threads[i]->set_unit_value( uid, (const char*)varid.c_str(), (const char*)val->as_string().c_str() );							
							tcFrame::Instance()->Log( wxString::Format("thread %d unit %d variable '%s' = '%s'\n",
								i, uid, (const char*)varid.c_str(), (const char*)val->as_string().c_str() ) );
							break;
						}
					}
					else
						tcFrame::Instance()->Log( wxString::Format("par %d list %d: did not find 'unit' 'variable' and 'value' fields\n", i, j ));
				}
			}
			else
				tcFrame::Instance()->Log(wxString::Format("par %d: each parametric structure must be an array of tables\n", i ));
		}
	}
	else
	{
		tcFrame::Instance()->Log("invalid parametric run structure\n");
	}

	tcFrame::Instance()->Log( wxString::Format("thread creation time: %d ms\n", sw.Time() ) );

	sw.Start();
	// start the threads
	for ( int i=0;i<nthread;i++ )
		threads[i]->Run();

	tcFrame::Instance()->Log( wxString::Format("thread start time: %d ms\n", sw.Time() ) );
	
	
	sw.Start();
	while (1)
	{
		size_t i, num_finished = 0;
		for (i=0;i<threads.size();i++)
			if (!threads[i]->IsRunning())
				num_finished++;

		if (num_finished == threads.size())
			break;

		// threads still running so update interface
		for (i=0;i<threads.size();i++)
		{
			float per = threads[i]->GetPercent();
			tpd.Update(i, per);
			wxArrayString msgs = threads[i]->GetNewMessages();
			tpd.Log( msgs );
		}

		wxGetApp().Yield();

		// if dialog's cancel button was pressed, send cancel signal to all threads
		if (tpd.IsCanceled())
		{
			for (i=0;i<threads.size();i++)
				threads[i]->Cancel();
		}

		::wxMilliSleep( 100 );
	}

	
	// wait on the joinable threads
	for (size_t i=0;i<threads.size();i++)
		threads[i]->Wait();
	
	tcFrame::Instance()->Log( wxString::Format("thread total run time: %d ms\n", sw.Time() ) );

	// obtain results here


	// delete all the thread objects
	for (size_t i=0;i<threads.size();i++)
		delete threads[i];

	threads.clear();
}

lk::fcall_t* tcs_funcs()
{
	static const lk::fcall_t vec[] = {
		fcall_clear,
		fcall_add_unit,
		fcall_set_value,
		fcall_connect,
		fcall_simulate,
		fcall_parallel,
		fcall_netlist,
		fcall_get_result,
		fcall_open_visual,
		fcall_load_visual,
		fcall_timevec,
		fcall_datatablevariable,
		fcall_cleardatatableselections,
		0 };
		
	return (lk::fcall_t*)vec;
}


enum { ID_FIND_NEXT = wxID_HIGHEST + 124 };

BEGIN_EVENT_TABLE( tcScriptEditor, wxPanel )
	EVT_BUTTON( wxID_NEW, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_OPEN, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_SAVE, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_SAVEAS, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_FIND, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_FORWARD, tcScriptEditor::OnAction )
	EVT_BUTTON( wxID_HELP, tcScriptEditor::OnHelp )
	EVT_BUTTON( ID_FIND_NEXT, tcScriptEditor::OnAction )
END_EVENT_TABLE()

class MyScriptCtrl : public wxLKScriptCtrl
{
public:
	MyScriptCtrl( wxWindow *parent, int id = wxID_ANY )
		: wxLKScriptCtrl( parent, id, wxDefaultPosition, wxDefaultSize, wxLK_STDLIB_ALL|wxLK_STDLIB_SOUT )
	{
	}

	virtual bool OnEval( int  )
	{
		wxGetApp().Yield( true );
		return true;
	}

	virtual void OnOutput( const wxString &tt )
	{
		Log( tt );
	}
};

tcScriptEditor::tcScriptEditor( wxWindow *p )
	: wxPanel( p, wxID_ANY, wxDefaultPosition, wxDefaultSize )
{
	//SetBackgroundColour( *wxWHITE );

	wxBoxSizer *sztools = new wxBoxSizer( wxHORIZONTAL );
	sztools->Add( new wxButton( this, wxID_NEW, "New" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2 );
	sztools->Add( new wxButton( this, wxID_OPEN, "Open" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( new wxButton( this, wxID_SAVE, "Save", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT ), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( new wxButton( this, wxID_SAVEAS, "Save as", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( new wxButton( this, wxID_FORWARD, "Run" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );

	m_stopButton = new wxButton( this, wxID_STOP, "Stop" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
	m_stopButton->SetForegroundColour( *wxRED );
	sztools->Add( m_stopButton, 0, wxALL|wxEXPAND, 2 );
	m_stopButton->Show( false );

	sztools->Add( new wxButton( this, wxID_FIND, "Find" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( new wxButton( this, ID_FIND_NEXT, "Find next" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( new wxButton( this, wxID_HELP, "Help" , wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT), 0, wxALL|wxEXPAND, 2  );
	sztools->Add( m_statusLabel = new wxStaticText(this,wxID_ANY,wxEmptyString), 0, wxALL|wxALIGN_CENTER_VERTICAL, 2 );

	m_editor = new MyScriptCtrl( this, wxID_ANY );
	m_editor->RegisterLibrary( tcs_funcs(), "TCS Functions", this );	

	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( sztools, 0, wxALL|wxEXPAND, 2 );
	szmain->Add( new wxStaticLine( this ), 0, wxALL|wxEXPAND, 0 );
	szmain->Add( m_editor, 1, wxALL|wxEXPAND );
	SetSizer(szmain);
}

bool tcScriptEditor::IsModified()
{
	return m_editor->GetModify();
}

bool tcScriptEditor::Save()
{
	if ( m_fileName.IsEmpty() )
		return SaveAs();
	else
		return Write( m_fileName );
}

bool tcScriptEditor::SaveAs()
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

void tcScriptEditor::OnHelp( wxCommandEvent & )
{
	m_editor->ShowHelpDialog( this );
}

bool tcScriptEditor::CloseDoc()
{
	if (m_editor->GetModify())
	{
		int result = wxMessageBox("Script modified. Save it?", "Query", wxYES_NO|wxCANCEL);
		if ( result == wxCANCEL 
			|| (result == wxYES && !Save()) )
			return false;
	}

	m_editor->SetText("");
	m_editor->EmptyUndoBuffer();
	m_editor->SetSavePoint();
	m_fileName = "";
	m_statusLabel->SetLabel( wxEmptyString );
	return true;
}

bool tcScriptEditor::Write( const wxString &file )
{
	if ( ((wxStyledTextCtrl*)m_editor)->SaveFile( file ) )
	{
		m_fileName = file;
		m_statusLabel->SetLabel( m_fileName );
		return true;
	}
	else return false;
}

bool tcScriptEditor::Load( const wxString &file )
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
		m_statusLabel->SetLabel(file);
		m_fileName = file;
		return true;
	}
	else return false;
}

void tcScriptEditor::OnAction( wxCommandEvent &evt )
{
	switch( evt.GetId() )
	{
	case wxID_OPEN:
		{
			
			wxFileDialog dlg(this, "Open", wxEmptyString, wxEmptyString,
								  "LK Script Files (*.lk)|*.lk",
								  wxFD_OPEN | wxFD_FILE_MUST_EXIST | wxFD_CHANGE_DIR);

			if (dlg.ShowModal() == wxID_OK)
				if (!Load( dlg.GetPath() ))
					wxMessageBox("Could not load file:\n\n" + dlg.GetPath());
		}
		break;
	case wxID_SAVE: Save();	break;
	case wxID_SAVEAS: SaveAs(); break;
	case wxID_NEW: CloseDoc(); break;
	case wxID_FIND:
		m_editor->ShowFindReplaceDialog();
		break;
	case ID_FIND_NEXT:
		m_editor->FindNext();
		break;
	case wxID_FORWARD: Exec(); break;
	case wxID_STOP: m_editor->Stop(); break;
	default:
		break;
	}
}

void tcScriptEditor::Exec()
{
	if (!m_fileName.IsEmpty())
		((wxStyledTextCtrl*)m_editor)->SaveFile( m_fileName + "~" );

	ClearLog();
	Log("Start: " + wxNow()  + "\n");
	m_stopButton->Show();
	Layout();
	wxGetApp().Yield(true);

	m_editor->Execute( );
	
	if ( m_stopButton->IsShown() )
	{
		m_stopButton->Hide();
		Layout();
	}
}
