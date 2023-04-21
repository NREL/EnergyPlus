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

#ifndef __tcsmain_h
#define __tcsmain_h


#include <wx/wx.h>
#include <wx/grid.h>
#include <wx/notebook.h>
#include <string>
#include <tcskernel.h>

// wxwidgets
class wxNotebook;
class wxAuiNotebook;
class wxProgressDialog;
class wxTextCtrl;

// wex
class wxDVPlotCtrl;
class wxExtGridCtrl;
class wxNumericCtrl;
class wxMetroNotebook;

// tcs
class tcskernel;

// tcsmain
class tcFrame;
class tcLayoutCtrl;
class tcVisualEditor;
class tcScriptEditor;

void Log( const wxString &text );
void Log( const char *fmt, ... );
void ClearLog();

class tcKernel : public tcskernel
{
public:
	tcKernel( tcFrame *frm, tcstypeprovider *prov );
	virtual ~tcKernel();

	virtual void log( const std::string & );
	virtual bool converged( double time );

	void set_store_array_matrix_data( bool b ) { m_storeArrMatData = b; }

	virtual int simulate( double start, double end, double step, wxProgressDialog *pd, double *time_sec );
	
	struct dataitem {
		dataitem( const char *s ) : sval(s) { }
		dataitem( const std::string &s ) : sval(s) { }
		dataitem( double d ) : dval(d) { }
		std::string sval;
		double dval;
	};

	struct dataset {
		unit *u;
		int uidx;
		int idx;
		std::string name;
		std::string units;
		std::string group;
		int type;
		std::vector<dataitem> values;
	};

	dataset *get_results(int idx);

private:
	bool m_storeArrMatData;
	wxProgressDialog *m_progressDialog;
	tcFrame *m_frame;
	std::vector< dataset > m_results;
	double m_start, m_end, m_step;
	size_t m_dataIndex;
	wxStopWatch m_watch;
};

class ResultsTable : public wxGridTableBase
{
public:
	ResultsTable( );
	void AddResult( tcKernel::dataset *d );

    virtual int GetNumberRows();
    virtual int GetNumberCols();
	virtual bool IsEmptyCell( int, int ) { return false; }
	virtual void SetValue( int, int, const wxString &) { };
    virtual wxString GetValue( int row, int col );
    virtual wxString GetColLabelValue( int col );
	void ReleasePointers();
private:
	std::vector< tcKernel::dataset* > m_results;
};

class tcFrame : public wxFrame
{
private:
	wxDVPlotCtrl *m_plot;
	wxExtGridCtrl *m_grid;
	wxTextCtrl *m_textOut;
	wxCheckListBox *m_varSelector;
	tcVisualEditor *m_visualEditor;
	tcScriptEditor *m_scriptEditor;
	wxMetroNotebook *m_notebook;
	
	tcKernel *m_kernel;
	tcstypeprovider m_provider;

public:
	tcFrame();
	virtual ~tcFrame();

	tcstypeprovider *GetTypeProvider() { return &m_provider; }
	tcKernel *GetKernel() { return m_kernel; }
	int Simulate(double start, double end, double step, int iter, bool store_arrmat, bool proceed_anyway );
	void AddVariableToDataTable( const wxString &varname );
	void ClearDataTableSelections();

	void Log( const wxString & );
	void ClearLog();
	void OnSelectVar( wxCommandEvent & );
	void OnCloseFrame( wxCloseEvent & );

	wxArrayString GetTypes();
	void ShowTypeDataDialog( const wxString &type );


	tcVisualEditor *GetVisualEditor() { return m_visualEditor; }
	tcScriptEditor *GetScriptEditor() { return m_scriptEditor; }

	void UpdateGrid();

	static tcFrame *Instance();

	DECLARE_EVENT_TABLE()
};


class tcVisualEditor : public wxPanel
{
private:
	wxNumericCtrl *m_startTime;
	wxNumericCtrl *m_endTime;
	wxNumericCtrl *m_timeStep;
	wxNumericCtrl *m_maxIter;
	tcLayoutCtrl *m_layout;
	wxString m_fileName;
	wxStaticText *m_statusLabel;
	wxComboBox *m_typeChoice;
	wxCheckBox *m_storeArrMat;
	wxCheckBox *m_proceedAnyway;

public:
	tcVisualEditor( wxWindow *parent );	
	void OnAction( wxCommandEvent & );
	bool IsModified();
	void UpdateTypes();
	wxString GetFileName() { return m_fileName; }
	tcLayoutCtrl *GetLayout() { return m_layout; }
	bool WriteToDisk( const wxString &file );
	bool LoadFile( const wxString &file );

	DECLARE_EVENT_TABLE();
};


class tcApp : public wxApp
{
public:
	virtual bool OnInit();
};

DECLARE_APP( tcApp );

#endif
