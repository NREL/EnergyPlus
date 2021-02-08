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


#ifndef __dataview_h
#define __dataview_h

#include <vector>

#include <wx/panel.h>
#include <wx/checklst.h>
#include <wx/treebase.h>
#include <wx/grid.h>

#include "dllinvoke.h"

class wxExtGridCtrl;

class DataView : public wxPanel
{
public:

	class Table; // forward

	DataView( wxWindow *parent );
	virtual ~DataView() { m_vt = NULL; }

	void SetDataObject( var_table *vt ) { m_vt = vt; UpdateView(); }
	ssc_data_t GetDataObject() { return m_vt; }

	void UpdateView();	
	void UpdateGrid();
	virtual void Freeze();
	virtual void Thaw();


	std::vector<int> GetColumnWidths();
	void SetColumnWidths( const std::vector<int> &cwl );
	wxArrayString GetSelections();
	void SetSelections(const wxArrayString &sel);

	wxString GetSelection();

	void AddVariable();
	void EditVariable(wxString name=wxEmptyString);
	void DeleteVariable(wxString name=wxEmptyString);
	void ShowStats( wxString name=wxEmptyString );

private:
	void OnCommand(wxCommandEvent &evt);
	void OnVarListCheck(wxCommandEvent &evt);
	void OnVarListDClick(wxCommandEvent &evt);
	void OnPopup( wxCommandEvent &evt);

	void OnGridLabelRightClick(wxGridEvent &evt);
	void OnGridLabelDoubleClick(wxGridEvent &evt);

	bool m_frozen;
	wxExtGridCtrl *m_grid;
	Table *m_grid_table;
	wxCheckListBox *m_varlist;

	wxTreeItemId m_root_item;
	std::vector<wxTreeItemId> m_tree_items;
	wxArrayString m_names;
	wxArrayString m_selections;

	wxString m_popup_var_name;

	var_table *m_vt;

	DECLARE_EVENT_TABLE();
};


class wxExtGridCtrl;
class wxNumericCtrl;

class StatDialog: public wxDialog
{
public:
	StatDialog(wxWindow *parent, const wxString &title);

	void Compute( util::matrix_t<ssc_number_t> &val );

private:
	wxExtGridCtrl *grdMonthly;
	wxNumericCtrl *numSumOver1000;
	wxNumericCtrl *numSum;
	wxNumericCtrl *numMax;
	wxNumericCtrl *numMean;
	wxNumericCtrl *numMin;
};


#endif
