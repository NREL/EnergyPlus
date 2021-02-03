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


#include <wx/statline.h>

#include <wex/numeric.h>
#include <wex/extgrid.h>

#include "editvariable.h"


enum {  ID_TYPE_STRING=wxID_HIGHEST+333,
		ID_TYPE_NUMBER,
		ID_TYPE_ARRAY,
		ID_TYPE_MATRIX,
		ID_FOCUS_STRING,
		ID_FOCUS_NUMBER,
		ID_numCols,
		ID_rbgVarType,
		ID_grdArrMat,
		ID_btnCancel,
		ID_btnAccept,
		ID_btnChooseFile,
		ID_txtValue,
		ID_numValue,
		ID_numRows };

BEGIN_EVENT_TABLE( EditVariableDialog, wxDialog )
	EVT_MENU( ID_TYPE_STRING, EditVariableDialog::OnShortcut )
	EVT_MENU( ID_TYPE_NUMBER, EditVariableDialog::OnShortcut )
	EVT_MENU( ID_TYPE_ARRAY, EditVariableDialog::OnShortcut )
	EVT_MENU( ID_TYPE_MATRIX, EditVariableDialog::OnShortcut )
	EVT_MENU( ID_FOCUS_STRING, EditVariableDialog::OnShortcut )
	EVT_MENU( ID_FOCUS_NUMBER, EditVariableDialog::OnShortcut )

	EVT_NUMERIC( ID_numRows, EditVariableDialog::OnRowsColsChange )
	EVT_NUMERIC( ID_numCols, EditVariableDialog::OnRowsColsChange )
	EVT_TEXT( ID_txtValue, EditVariableDialog::OnTextChange )
	EVT_BUTTON( ID_btnChooseFile, EditVariableDialog::OnChooseFile )
	EVT_NUMERIC( ID_numValue, EditVariableDialog::OnNumChange )
	EVT_GRID_CMD_CELL_CHANGED( ID_grdArrMat, EditVariableDialog::OnGridCellChange )
	EVT_RADIOBOX( ID_rbgVarType, EditVariableDialog::OnTypeChange )
END_EVENT_TABLE()

EditVariableDialog::EditVariableDialog(wxWindow *parent, const wxString &title )
	 : wxDialog( parent, wxID_ANY, title, wxDefaultPosition, wxSize(800,600), 
		wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER )
{

	numRows = new wxNumericCtrl(this, ID_numRows, 3, wxNUMERIC_INTEGER );
	numCols = new wxNumericCtrl(this, ID_numCols, 4, wxNUMERIC_INTEGER );
		
	numValue = new wxNumericCtrl(this, ID_numValue );
	txtValue = new wxTextCtrl(this, ID_txtValue );

	btnChooseFile = new wxButton(this, ID_btnChooseFile, "file..", wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);

	wxArrayString type_choices;
	type_choices.Add("SSC_STRING");
	type_choices.Add("SSC_NUMBER");
	type_choices.Add("SSC_ARRAY");
	type_choices.Add("SSC_MATRIX");
	type_choices.Add("SSC_TABLE");
	rbgVarType = new wxRadioBox(this, ID_rbgVarType, "Data type", wxDefaultPosition, wxDefaultSize, type_choices);

	grdArrMat = new wxExtGridCtrl(this, ID_grdArrMat);
	grdArrMat->CreateGrid(2,2);
	grdArrMat->EnableEditing(true);
	grdArrMat->DisableDragCell();
	grdArrMat->DisableDragColSize();
	grdArrMat->DisableDragRowSize();
	grdArrMat->DisableDragColMove();
	grdArrMat->DisableDragGridSize();
	grdArrMat->SetRowLabelSize(23);
	grdArrMat->SetColLabelSize(23);

	
	wxBoxSizer *sz_htop = new wxBoxSizer(wxHORIZONTAL);
	sz_htop->Add( new wxStaticText(this, wxID_ANY, "Numeric value:"), 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 2);
	sz_htop->Add( numValue, 0, wxALL|wxEXPAND, 2 );
	sz_htop->Add( new wxStaticText(this, wxID_ANY, "String value:" ), 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 2);
	sz_htop->Add( txtValue, 0, wxALL|wxEXPAND, 2 );	
	sz_htop->Add( btnChooseFile );
	sz_htop->Add( new wxStaticText(this, wxID_ANY, "# rows:" ), 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 2);
	sz_htop->Add( numRows, 0, wxALL|wxEXPAND, 2 );	
	sz_htop->Add( new wxStaticText(this, wxID_ANY, "# cols:"), 0, wxALL|wxEXPAND|wxALIGN_CENTER_VERTICAL, 2);
	sz_htop->Add( numCols, 0, wxALL|wxEXPAND, 2 );

	wxBoxSizer *sz_main = new wxBoxSizer(wxVERTICAL);
	sz_main->Add( rbgVarType, 0, wxALL|wxEXPAND, 10 );
	sz_main->Add( sz_htop, 0, wxALL|wxEXPAND, 10 );
	sz_main->Add( grdArrMat, 1, wxEXPAND|wxALL, 10 );
	sz_main->Add( new wxStaticText(this, wxID_ANY, "Shortcuts: F1=SSC_STRING, F2=SSC_NUMBER, F3=SSC_ARRAY, F4=SSC_MATRIX,\n"
		"F5=Change string value, F6=Change number value, F10=Accept changes, Esc=Cancel dialog"), 0, wxALL|wxEXPAND, 10	 );
	sz_main->Add( new wxStaticLine( this, wxID_ANY ), 0, wxALL|wxEXPAND, 3 );
	sz_main->Add( CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );

	SetSizer( sz_main );

	wxAcceleratorEntry entries[10];
	entries[0].Set(::wxACCEL_NORMAL, WXK_F1, ID_TYPE_STRING);
	entries[1].Set(::wxACCEL_NORMAL, WXK_F2, ID_TYPE_NUMBER);
	entries[2].Set(::wxACCEL_NORMAL, WXK_F3, ID_TYPE_ARRAY);
	entries[3].Set(::wxACCEL_NORMAL, WXK_F4, ID_TYPE_MATRIX);
	entries[4].Set(::wxACCEL_NORMAL, WXK_F5, ID_FOCUS_STRING);
	entries[5].Set(::wxACCEL_NORMAL, WXK_F6, ID_FOCUS_NUMBER);
	entries[6].Set(::wxACCEL_NORMAL, WXK_F10, wxID_OK);
	wxAcceleratorTable acceltab(7,entries);
	SetAcceleratorTable(acceltab);
}

void EditVariableDialog::UpdateForm()	
{
	rbgVarType->SetSelection( m_var.type-1 );
	if (m_var.type == SSC_STRING)txtValue->ChangeValue( m_var.str );

	if (m_var.type == SSC_NUMBER)numValue->SetValue( m_var.num );

	if (m_var.type == SSC_ARRAY)
	{
		grdArrMat->Freeze();
		grdArrMat->ResizeGrid( m_var.num.length(), 1 );
		numRows->SetValue( m_var.num.length() );
		numCols->SetValue( 1 );
		for (size_t i=0;i<m_var.num.length();i++)
			grdArrMat->SetCellValue( i, 0, wxString::Format("%lg", (double) m_var.num[i]) );

		grdArrMat->Thaw();
	}

	if (m_var.type == SSC_MATRIX)
	{
		grdArrMat->Freeze();
		grdArrMat->ResizeGrid( m_var.num.nrows(), m_var.num.ncols() );
		numRows->SetValue( m_var.num.nrows() );
		numCols->SetValue( m_var.num.ncols() );

		for (size_t r=0;r<m_var.num.nrows();r++)
			for (size_t c=0;c<m_var.num.ncols();c++)
				grdArrMat->SetCellValue( r, c, wxString::Format("%lg", (double) m_var.num.at(r,c)) );

		grdArrMat->Thaw();
	}

	txtValue->Enable( m_var.type == SSC_STRING );
	numValue->Enable( m_var.type == SSC_NUMBER );
	grdArrMat->Enable( m_var.type == SSC_ARRAY || m_var.type == SSC_MATRIX );
	numRows->Enable( m_var.type == SSC_ARRAY || m_var.type == SSC_MATRIX );
	numCols->Enable( m_var.type == SSC_MATRIX );

	if (m_var.type == SSC_NUMBER)
	{
		numValue->SelectAll();
		numValue->SetFocus();
	}
}

void EditVariableDialog::OnTypeChange( wxCommandEvent & )
{
	m_var.type = rbgVarType->GetSelection()+1;
	UpdateForm();	
}

void EditVariableDialog::OnShortcut( wxCommandEvent &evt)
{
	switch(evt.GetId())
	{
	case ID_TYPE_STRING: m_var.type = SSC_STRING; UpdateForm(); break;
	case ID_TYPE_NUMBER: m_var.type = SSC_NUMBER; UpdateForm(); break;
	case ID_TYPE_ARRAY:  m_var.type = SSC_ARRAY; UpdateForm(); break;
	case ID_TYPE_MATRIX: m_var.type = SSC_MATRIX; UpdateForm(); break;
	case ID_FOCUS_STRING: txtValue->SetFocus(); txtValue->SelectAll(); break;
	case ID_FOCUS_NUMBER: numValue->SetFocus(); numValue->SelectAll(); break;
	}
}

void EditVariableDialog::OnTextChange( wxCommandEvent & )
{
	m_var.str = txtValue->GetValue();
}

void EditVariableDialog::OnNumChange( wxCommandEvent &)
{
	m_var.num = (double) numValue->AsDouble();
}

void EditVariableDialog::OnGridCellChange( wxGridEvent &evt )
{
	int r, c;
	r = evt.GetRow();
	c = evt.GetCol();

	if (r < 0 || c < 0) return;

	double val = wxAtof( grdArrMat->GetCellValue(r,c) );

	if (m_var.type == SSC_MATRIX)
	{
		if (r < (int)m_var.num.nrows() && c < (int)m_var.num.ncols())
			m_var.num.at(r,c) = val;
	}
	else if (m_var.type == SSC_ARRAY)
	{
		if (r < (int)m_var.num.length())
			m_var.num[r] = val;
	}

	grdArrMat->SetCellValue( r, c, wxString::Format("%lg",val) );
}

void EditVariableDialog::OnChooseFile( wxCommandEvent & )
{
	wxFileDialog fd( this, "Choose a file");
	if (fd.ShowModal() == wxID_OK)
	{
		wxString file = fd.GetPath();
		file.Replace("\\","/");
		txtValue->ChangeValue( file );
		m_var.str = file;
	}
}

void EditVariableDialog::OnRowsColsChange( wxCommandEvent & )
{
	size_t nr, nc;

	nr = (size_t) numRows->AsInteger();
	nc = (size_t) numCols->AsInteger();

	if (m_var.type == SSC_ARRAY)
	{
		util::matrix_t<ssc_number_t> old;
		old.copy(m_var.num);
		m_var.num.resize_fill( nr, 0.0 );
		for (size_t i=0;i<m_var.num.length();i++)
			m_var.num[i] = (i<old.length()) ? old[i] : 0.0;
	}
	else
	{
		util::matrix_t<ssc_number_t> old;
		old.copy( m_var.num );

		m_var.num.resize_fill( nr, nc, 0.0 );	
		for (size_t r=0;r<nr;r++)
			for (size_t c=0;c<nc;c++)
				m_var.num.at(r,c) = (r<old.nrows()&&c<old.ncols()) ? old.at(r,c) : 0.0;
	}
	UpdateForm();
}

