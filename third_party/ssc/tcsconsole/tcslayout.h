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

#ifndef __tcsedit_h
#define __tcsedit_h

#include <wx/wx.h>
#include <tcskernel.h>

#ifdef __WXOSX__
#define TC_USE_OVERLAY 1
#include <wx/overlay.h>
#endif


class wxGrid;

class tcLayoutCtrl : public wxWindow
{
public:
	tcLayoutCtrl( wxWindow *parent, int id, const wxPoint &pos=wxDefaultPosition, const wxSize &size = wxDefaultSize);
	virtual ~tcLayoutCtrl();
	
	void AddType( const wxString &name, tcstypeinfo *ti, const wxString &meta = wxEmptyString );

	// deletes units and connections
	void Clear();

	wxString GetError() { return m_error; }
	bool Read( wxInputStream &is );
	bool Write( wxOutputStream &os );
	void SetModified( bool b );
	bool IsModified();
	
	void CreatePopupMenu();
	
	wxString GetStatusText();
	bool CanFinishConnection();
	
	void GetViewExtent( int &minx, int &maxx, int &miny, int &maxy );
	void Draw( wxDC &dc, wxSize &client, bool with_status = true, bool with_back_grid = true );
	wxBitmap GetBitmap();

	wxString GetNetlist();
	wxString GetLKScript();
	bool LoadSystemInKernel( tcskernel *k );

	
	static wxGrid *CreateUnitDataGrid( wxWindow *parent, tcstypeinfo *ti );

public:
	
	void OnPaint( wxPaintEvent &evt );
	void OnSize( wxSizeEvent &evt );
	void OnLeftDown( wxMouseEvent &evt );
	void OnLeftDouble( wxMouseEvent &evt );
	void OnLeftUp( wxMouseEvent &evt );
	void OnRightDown( wxMouseEvent &evt );
	void OnMouseMove( wxMouseEvent &evt );
	void OnChar( wxKeyEvent & );
	void OnPopup( wxCommandEvent &evt );

private:
	class tcUnit;

	class tcType
	{
	public:
		tcType( const wxString &n, tcstypeinfo *t, const wxString &m )
			: name(n), type(t), meta(m) {  }

		wxString name;
		tcstypeinfo *type;
		wxString meta;
	};
	
	struct tcConnPt
	{
		tcConnPt( tcsvarinfo *t, tcUnit *u, 
			int vi, const wxPoint &p,
			bool in ) : ti(t), unit(u), idx(vi), pt(p), isinput(in) {  }
		tcsvarinfo *ti;
		tcUnit *unit;
		int idx;
		wxPoint pt;
		bool isinput;
	};
	
	struct SPair { wxString key, value; };

	class tcUnit
	{
	public:
		tcUnit() {
			type = 0;
			x = y = 20;
			rdr.width = rdr.height = 100;
		}

		tcstypeinfo *type;
		int x, y;
		wxString description;
		std::vector< SPair > values;

			
		struct {
			int width, height;
			std::vector< tcConnPt > inputs;
			std::vector< tcConnPt > outputs;
		} rdr;
	};

	
	class tcConn
	{
	public:
		tcConn() {
			start = end = 0;
			start_pt = end_pt = 0;
			ftol = 0.1;
			index = -1;
		}

		tcUnit *start, *end;
		tcConnPt *start_pt, *end_pt;
		double ftol;
		int index;
		std::vector< wxPoint > waypoints;
	};

	float Distance(int x1, int y1, int x2, int y2);

	std::vector< tcUnit* > m_units;
	std::vector< tcConn* > m_conns;
	std::vector< tcType > m_types;

	int m_offsetX, m_offsetY;
	int m_snapSpacing;
	int m_mouseLastX, m_mouseLastY;
	int m_popupX, m_popupY;
	int m_statusHeight;

	bool m_modified;

	tcUnit *m_currentUnit;
	int m_currentUnitIndex;
	tcConnPt *m_currentConnPt;
	tcConn *m_currentConn;
	tcConn *m_currentWaypointConn;
	int m_currentWaypointIndex;

	bool m_moveModeErase;
	tcUnit *m_movingUnit;
	int m_origX, m_origY, m_diffX, m_diffY;

	int m_lastLineX, m_lastLineY;
	bool m_lineModeErase;

	int UnitIndex( tcUnit *u );

	tcConn *m_movingWaypointConn;
	int m_movingWaypointIndex;
	bool m_moveWaypointErase;
	wxPoint m_moveWaypointLastXY;

	wxString m_error;

	wxMenu *m_popupMenu;

	void EditUnit( tcUnit *u );
	bool IsAssignedInputValue( tcConnPt *pt );
	
	
	void Modify() { m_modified = true; }
	void Snap(int *x, int *y);
	int Snap( int v );
	
	void DrawMoveOutline();
	void DrawWaypointMoveOutline();
	void DrawSegmentOutline( const wxPoint &pt );
	void EscapeAction();

	void UpdateStatus();

	wxTextCtrl *m_statusText;

	tcConnPt *LocateConnection( int mx, int my );
	tcUnit *LocateUnit( int mx, int my );
	bool FindWayPoint( int mx, int my, tcConn **conn, int *wpidx );

	void SizeNewUnit( tcUnit *u );
	tcConnPt *FindConnPt( tcUnit *u, int varindex );
	tcstypeinfo *FindType( const wxString &name );
	bool GetNumericValue( tcUnit *u, const wxString &name, double *val );
	tcConn *FindConnFromPt( tcConnPt * );
	
#ifdef TC_USE_OVERLAY
	wxOverlay m_overlay;
#endif

	DECLARE_EVENT_TABLE()
};


#endif
