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

#include <wx/valtext.h>
#include <wx/dcbuffer.h>
#include <wx/grid.h>
#include <wx/imaglist.h>
#include <wx/gdicmn.h>
#include <wx/paper.h>
#include <wx/tokenzr.h>
#include <wx/datstrm.h>
#include <wx/busyinfo.h>
#include <wx/wfstream.h>
#include <wx/clipbrd.h>

#include "tcslayout.h"

enum { ID_popup_first, 
	ID_CREATE = 2134, ID_CREATE_LAST = 3215, 
	ID_MOVE_UP, ID_MOVE_DOWN,
	ID_DELETE_UNIT,
	ID_DELETE_CONNECTION,
	ID_DELETE_ALL_CONNECTIONS,
	ID_ADD_WAYPOINT,
	ID_DELETE_WAYPOINT,
	ID_EDIT_CONNECTION,
	ID_EDIT_UNIT,
	ID_COPY_VIEW,
	ID_popup_last };

BEGIN_EVENT_TABLE(tcLayoutCtrl, wxWindow)

	EVT_MENU_RANGE( ID_popup_first, ID_popup_last, tcLayoutCtrl::OnPopup )
	
	EVT_PAINT(tcLayoutCtrl::OnPaint)
	EVT_SIZE(tcLayoutCtrl::OnSize)
	EVT_LEFT_DOWN( tcLayoutCtrl::OnLeftDown )
	EVT_LEFT_DCLICK( tcLayoutCtrl::OnLeftDouble )
	EVT_LEFT_UP( tcLayoutCtrl::OnLeftUp )
	EVT_RIGHT_DOWN( tcLayoutCtrl::OnRightDown )
	EVT_MOTION( tcLayoutCtrl::OnMouseMove )
	EVT_CHAR( tcLayoutCtrl::OnChar )

END_EVENT_TABLE()

tcLayoutCtrl::tcLayoutCtrl( wxWindow *parent, int id, const wxPoint &pos, const wxSize &size )
	: wxWindow( parent,  id, pos, size, wxWANTS_CHARS|wxCLIP_CHILDREN )
{
	SetBackgroundStyle( wxBG_STYLE_CUSTOM );

	m_statusText = new wxTextCtrl(this, wxID_ANY, "ready", wxPoint(0,0), wxSize( size.GetWidth(), 23 ), wxBORDER_NONE|wxTE_READONLY );
	m_statusText->SetBackgroundColour( *wxWHITE );
	m_statusText->SetForegroundColour( *wxBLUE );
	
	int fontSize = 10;
#ifdef __WXMAC__
	fontSize = 12;
#endif
	m_statusText->SetFont( wxFont( fontSize, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas" ) );

	m_modified = false;

	m_snapSpacing = 10;
	m_mouseLastX = m_mouseLastY = 0;
	m_popupX = m_popupY = 0;
	m_popupMenu = 0;
	m_offsetX = m_offsetY = 0;

	m_currentUnit = 0;
	m_currentUnitIndex = -1;
	m_currentConnPt = 0;
	m_currentConn = 0;
	m_currentWaypointConn = 0;
	m_currentWaypointIndex = -1;

	m_movingUnit = 0;
	m_moveModeErase = false;
	m_origX = m_origY = m_diffX = m_diffY = 0;

	m_lineModeErase = false;
	m_lastLineX = m_lastLineY = 0;

	m_movingWaypointConn = 0;
	m_movingWaypointIndex = -1;
	m_moveWaypointErase = false;
	m_moveWaypointLastXY = wxPoint(0,0);

	m_statusHeight = 20;

	CreatePopupMenu();
	SetCursor( wxCursor( ::wxCURSOR_ARROW ) );


}

tcLayoutCtrl::~tcLayoutCtrl()
{
	Clear();
}

void tcLayoutCtrl::AddType( const wxString &name, tcstypeinfo *ti, const wxString &meta )
{
	for (size_t i=0;i<m_types.size();i++)
		if (m_types[i].name == name)
			return;

	m_types.push_back( tcType( name, ti, meta ) );
}

void tcLayoutCtrl::Clear()
{
	for (size_t i=0;i<m_units.size();i++)
		delete m_units[i];
	m_units.clear();

	for (size_t i=0;i<m_conns.size();i++)
		delete m_conns[i];
	m_conns.clear();
	
	Modify();
}

void tcLayoutCtrl::CreatePopupMenu()
{
	if (m_popupMenu != 0) delete m_popupMenu;

	m_popupMenu = new wxMenu;

	for (size_t i=0;i<m_types.size();i++)
		m_popupMenu->Append( ID_CREATE+i, "Create " + m_types[i].name );

	if (m_types.size() > 1)
		m_popupMenu->AppendSeparator();


	m_popupMenu->Append( ID_DELETE_UNIT, "Delete unit");
	m_popupMenu->Append( ID_DELETE_CONNECTION, "Delete connection" );
	m_popupMenu->Append( ID_DELETE_ALL_CONNECTIONS, "Delete all connections at this point" );
	m_popupMenu->Append( ID_ADD_WAYPOINT, "Add waypoint nearby");
	m_popupMenu->Append( ID_DELETE_WAYPOINT, "Delete waypoint");

	m_popupMenu->AppendSeparator();
	
	m_popupMenu->Append( ID_EDIT_CONNECTION, "Edit connection..." );
	m_popupMenu->Append( ID_EDIT_UNIT, "Edit unit values..." );
	m_popupMenu->Append( ID_MOVE_UP, "Move up (order)" );
	m_popupMenu->Append( ID_MOVE_DOWN, "Move down (order)" );

	m_popupMenu->AppendSeparator();
	m_popupMenu->Append( ID_COPY_VIEW, "Copy view to clipboard (image)");
}


	
bool tcLayoutCtrl::Read( wxInputStream &is )
{
	std::vector< tcUnit* > units;
	std::vector< tcConn* > conns;

	if ( !is.IsOk() ) return false;

	wxDataInputStream in(is);

	unsigned short code = in.Read16();
	if (code != 0xab)
	{
		m_error = "invalid start code, 0xab required";
		return false;
	}

	unsigned char ver = in.Read8();
	if (ver < 1)
	{
		m_error = "invalid data format version";
		return false;
	}

	int nunits = in.Read32();
	for (int i=0;i<nunits;i++)
	{
		wxString type = in.ReadString();
		tcstypeinfo *ti = FindType( type );
		if (!ti)
		{
			for (size_t k=0;k<units.size();k++)
				delete units[k];
			m_error.Printf("unit %d could not find referenced type: %s", i, type.c_str() );
			return false;
		}

		tcUnit *u = new tcUnit;
		u->type = ti;
		u->x = in.Read32()-10000;
		u->y = in.Read32()-10000;
		int nvals = in.Read32();
		for (int j=0;j<nvals;j++)
		{
			SPair p;
			p.key = in.ReadString();
			p.value = in.ReadString();
			u->values.push_back( p );
		}
		u->description = in.ReadString();

		SizeNewUnit( u );
		units.push_back( u );
	}

	int nconn = in.Read32();
	for (int i=0;i<nconn;i++)
	{
		size_t ustart = in.Read32();
		size_t uend = in.Read32();
		size_t startidx = in.Read32();
		size_t endidx = in.Read32();
		double tol = in.ReadDouble();
		int index = ((int)in.Read32()) - 1;
		int nway = in.Read32();
		std::vector<wxPoint> way;
		for (int j=0;j<nway;j++)
		{
			wxPoint p;
			p.x = in.Read32() - 10000;
			p.y = in.Read32() - 10000;
			way.push_back(p);
		}

		tcConnPt *p1, *p2;
		if ( ustart < units.size()
			&& uend < units.size()
			&& ((p1 = FindConnPt( units[ustart], startidx ))!=0)
			&& ((p2 = FindConnPt( units[uend], endidx ))!=0) )
		{
			tcConn *c = new tcConn;
			c->start = units[ustart];
			c->end = units[uend];
			c->start_pt = p1;
			c->end_pt = p2;
			c->ftol = tol;
			c->index = index;
			c->waypoints = way;
			conns.push_back( c );
		}
		else
		{
			for (size_t k=0;k<units.size();k++)
				delete units[k];
			for (size_t k=0;k<conns.size();k++)
				delete conns[k];

			m_error.Printf("failed to instantiate connection [%d:%d] to [%d:%d], check type definitions or versions",
				(int)ustart, (int)startidx, (int)uend, (int)endidx );

			return false;
		}
	}

	if (in.Read16() != 0xab)
	{
		for (size_t k=0;k<units.size();k++)
			delete units[k];
		m_error = "did not encounter end-of-data marker 0xab";
		return false;
	}

	Clear();
	m_units = units;
	m_conns = conns;
	m_currentUnit = 0;
	m_currentConn = 0;
	m_currentConnPt = 0;
	m_currentUnitIndex = -1;

	Refresh();
	UpdateStatus();
	m_modified = false;
	return true;
}

tcLayoutCtrl::tcConnPt *tcLayoutCtrl::FindConnPt( tcUnit *u, int varindex )
{
	for (size_t i=0;i<u->rdr.inputs.size();i++)
		if (u->rdr.inputs[i].idx == varindex)
			return &u->rdr.inputs[i];

	for (size_t i=0;i<u->rdr.outputs.size();i++)
		if (u->rdr.outputs[i].idx == varindex)
			return &u->rdr.outputs[i];

	return 0;

}

tcstypeinfo *tcLayoutCtrl::FindType( const wxString &name )
{
	for (size_t i=0;i<m_types.size();i++)
		if (m_types[i].name == name )
			return m_types[i].type;
	return 0;
}

bool tcLayoutCtrl::Write( wxOutputStream &os )
{
	if ( !os.IsOk() ) return false;
	wxDataOutputStream out(os);
	out.Write16( 0xab ); // start code
	out.Write8( 1 ); // format version

	// write units
	out.Write32( m_units.size() );
	for (size_t i=0;i<m_units.size();i++)
	{
		out.WriteString( m_units[i]->type->name );
		out.Write32( m_units[i]->x + 10000 );
		out.Write32( m_units[i]->y + 10000 );
		out.Write32( m_units[i]->values.size() );
		for ( size_t j=0;j< m_units[i]->values.size();j++ )
		{
			out.WriteString(  m_units[i]->values[j].key );
			out.WriteString(  m_units[i]->values[j].value );
		}
		out.WriteString( m_units[i]->description );
	}

	// write connections
	out.Write32( m_conns.size() );
	for (size_t i=0;i<m_conns.size();i++)
	{
		out.Write32( UnitIndex(m_conns[i]->start) );
		out.Write32( UnitIndex(m_conns[i]->end ) );
		out.Write32( m_conns[i]->start_pt->idx );
		out.Write32( m_conns[i]->end_pt->idx );
		out.WriteDouble( m_conns[i]->ftol );
		out.Write32( m_conns[i]->index + 1 );
		out.Write32( m_conns[i]->waypoints.size() );
		for (size_t j=0;j<m_conns[i]->waypoints.size();j++)
		{
			out.Write32( m_conns[i]->waypoints[j].x+10000 );
			out.Write32( m_conns[i]->waypoints[j].y+10000 );
		}
	}

	// finish code
	out.Write16( 0xab );
	return true;
}


int tcLayoutCtrl::UnitIndex( tcUnit *u )
{
	for (size_t i=0;i<m_units.size();i++)
		if (m_units[i] == u ) return i;

	return -1;
}

void tcLayoutCtrl::SetModified( bool b )
{
	m_modified = b;
}

bool tcLayoutCtrl::IsModified()
{
	return m_modified;
}

void tcLayoutCtrl::OnSize( wxSizeEvent & )
{
	m_statusText->SetSize(0,0,GetClientSize().GetWidth(), 23 );
}

static void extend( int x, int y, 
	int &minx, int &maxx, int &miny, int &maxy )
{	
	if (x < minx) minx = x;
	if (x > maxx) maxx = x;
	if (y < miny) miny = y;
	if (y > maxy) maxy = y;
}

void tcLayoutCtrl::GetViewExtent( int &minx, int &maxx, int &miny, int &maxy )
{
	minx = 1e9;
	maxx = -1e9;
	miny = 1e9;
	maxy = -1e9;

	for ( size_t i=0;i<m_units.size();i++ )
	{
		extend( m_units[i]->x, m_units[i]->y, 
			minx, maxx, miny, maxy );
		extend( m_units[i]->x + m_units[i]->rdr.width, 
			m_units[i]->y + m_units[i]->rdr.height, 
			minx, maxx, miny, maxy );
	}

	for ( size_t i=0;i<m_conns.size();i++ )
		for ( size_t j=0;j<m_conns[i]->waypoints.size();j++)
			extend( m_conns[i]->waypoints[j].x, 
				m_conns[i]->waypoints[j].y, 
				minx, maxx, miny, maxy );
}

wxBitmap tcLayoutCtrl::GetBitmap()
{
	int minx, maxx, miny, maxy;
	GetViewExtent( minx, maxx, miny, maxy );
	
	int width = maxx-minx;
	int height = maxy-miny;
	if ( width <= 0 || height <= 0 )
		return wxNullBitmap;

	int ox = m_offsetX;
	int oy = m_offsetY;

	m_offsetX = -minx;
	m_offsetY = -miny;

	wxBitmap bmp( width+20, height+20 );
	wxMemoryDC dc(bmp);

	dc.SetDeviceOrigin( 10, 10 );
	wxSize sz(width, height);
	Draw( dc, sz, false, false);

	m_offsetX = ox;
	m_offsetY = oy;

	return bmp;
}



void tcLayoutCtrl::Draw( wxDC &dc, wxSize &client, bool with_status, bool with_back_grid )
{
	wxRect windowRect( wxPoint(0,0), client );

	dc.SetBackground( *wxWHITE_BRUSH );
	dc.Clear();
	dc.SetFont( *wxNORMAL_FONT );
	
	if (with_back_grid)
	{
		dc.SetPen( wxPen( *wxLIGHT_GREY ,1) );
		if ( m_snapSpacing > 0 )
			for ( int i=0;i<windowRect.width;i+=m_snapSpacing )
				for (int j=0;j<windowRect.height;j+=m_snapSpacing )
					dc.DrawPoint(i, j);
	}

	for (size_t i=0;i<m_units.size();i++)
	{
		dc.SetPen( wxPen( *wxBLACK, 1 ) );
		dc.SetBrush( wxBrush( *wxWHITE ) );
		dc.SetTextForeground( *wxBLACK );

		wxRect r( m_units[i]->x + m_offsetX,
			m_units[i]->y + m_offsetY,
			m_units[i]->rdr.width,
			m_units[i]->rdr.height );

		dc.SetClippingRegion( r );
		dc.DrawRectangle( r );
		dc.SetTextForeground(*wxBLACK);
		dc.DrawText( wxString::Format("Unit %d", (int)i),
			r.x+2, r.y+2);
		dc.SetTextForeground( *wxLIGHT_GREY );
		dc.DrawText( "'" + wxString(m_units[i]->type->name) + "'", 
			r.x+2, dc.GetCharHeight()+r.y+2 );
		dc.SetTextForeground( "magenta" );
		dc.DrawText( m_units[i]->description, 
			r.x+2, dc.GetCharHeight()*2+r.y+4 );
		
		if ( m_units[i]->type->visual 
			&& m_units[i]->type->visual->pixmap )
		{
			wxBitmap xpm( m_units[i]->type->visual->pixmap );
			if ( xpm.IsOk() )
				dc.DrawBitmap( xpm, r.x + r.width - xpm.GetWidth() - 2, r.y+2, true );
		}

		dc.SetBrush( wxBrush( *wxBLACK ) );
		dc.SetPen( wxPen( *wxBLACK, 1 ) );

		for (size_t j=0;j<m_units[i]->rdr.inputs.size();j++ )
		{
			tcConnPt &cp = m_units[i]->rdr.inputs[j];
			if ( IsAssignedInputValue( &cp )) dc.SetTextForeground( wxColour(0, 100, 0) );
			else dc.SetTextForeground( wxColour(220, 0, 0) );
			wxString t = cp.ti->name;
			double dval = 0.0;
			if ( cp.ti->data_type == TCS_NUMBER && GetNumericValue( m_units[i], cp.ti->name, &dval ))
				t += wxString::Format( " (%lg)", dval );
			dc.DrawText( t, r.x+5, r.y+cp.pt.y - dc.GetCharHeight()/2 );	
			
			dc.DrawRectangle( r.x+1, r.y + cp.pt.y, 3, 3 );
		}
		
		dc.SetTextForeground( wxColour(0, 0, 100) );
		for (size_t j=0;j<m_units[i]->rdr.outputs.size();j++ )
		{
			tcConnPt &cp = m_units[i]->rdr.outputs[j];
			dc.DrawText( cp.ti->name, r.x+r.width-dc.GetTextExtent(cp.ti->name).GetWidth()-5, r.y + cp.pt.y - dc.GetCharHeight()/2 );

			dc.DrawRectangle( r.x+r.width-4, r.y + cp.pt.y, 3, 3 );
		}

		dc.DestroyClippingRegion();
	}

	dc.SetBrush( wxBrush( *wxBLACK ) );
	for ( size_t i=0;i<m_conns.size();i++ )
	{
		if ( strcmp( m_conns[i]->start_pt->ti->units, 
			m_conns[i]->end_pt->ti->units ) != 0 )
			dc.SetPen( wxPen( "orange", 2 ) );
		else
			dc.SetPen( wxPen( *wxBLACK, 2 ) );

		wxPoint curpt( m_conns[i]->start->x + m_conns[i]->start_pt->pt.x + m_offsetX,
			m_conns[i]->start->y + m_conns[i]->start_pt->pt.y + m_offsetY );

		for ( size_t j=0;j<m_conns[i]->waypoints.size();j++ )
		{
			wxPoint waypt( m_conns[i]->waypoints[j].x + m_offsetX,
				m_conns[i]->waypoints[j].y + m_offsetY );
			dc.DrawLine( curpt, waypt );
			curpt = waypt;
		}

		wxPoint endpt( m_conns[i]->end->x + m_conns[i]->end_pt->pt.x + m_offsetX,
			m_conns[i]->end->y + m_conns[i]->end_pt->pt.y + m_offsetY );

		endpt.x -= 9;
		dc.DrawLine( curpt, endpt );
		
		dc.SetPen( wxPen( *wxBLACK, 2 ) );
		wxPoint tri[3];
		tri[0].x = endpt.x+9;
		tri[0].y = endpt.y;
		tri[1].x = endpt.x;
		tri[1].y = endpt.y - 4;
		tri[2].x = endpt.x;
		tri[2].y = endpt.y + 4;
		dc.DrawPolygon( 3, tri );
	}
	
	if (with_status)
	{
		dc.SetPen( wxPen( *wxWHITE, 1 ));
		dc.SetBrush( wxBrush( *wxWHITE ));
		dc.DrawRectangle( 0, 0, windowRect.width, m_statusHeight);	
		dc.SetTextForeground( *wxBLACK );
		dc.DrawText( GetStatusText(), 2, 2 );
	}


}

void tcLayoutCtrl::OnPaint( wxPaintEvent & )
{
	wxAutoBufferedPaintDC dc(this);
	wxSize sz(GetClientSize());
	Draw( dc, sz, true, true );
}

bool tcLayoutCtrl::CanFinishConnection()
{
	bool already_connected = false;
		for (size_t i=0;i<m_conns.size();i++)
			if (m_conns[i]->end_pt == m_currentConnPt)
				already_connected = true;

	if ( m_currentConnPt != m_currentConn->start_pt
		&& m_currentUnit != 0
		&& m_currentConnPt != 0
		&& m_currentConnPt->isinput
		&& !already_connected )
		return true;
	return false;
}

void tcLayoutCtrl::OnLeftDown( wxMouseEvent &evt )
{
	SetFocus();

	if ( m_currentConn )
	{
		if (CanFinishConnection())
		{
			m_currentConn->end = m_currentUnit;
			m_currentConn->end_pt = m_currentConnPt;

			m_conns.push_back( m_currentConn );
			m_currentConn = 0;
			Modify();
			Refresh();
			UpdateStatus();
#ifdef TC_USE_OVERLAY
			wxClientDC dc( this );
			wxDCOverlay overlaydc( m_overlay, &dc );
			overlaydc.Clear();
			m_overlay.Reset();
#endif
		}
		else
		{
			int x = evt.GetX() - m_offsetX;
			int y = evt.GetY() - m_offsetY;
			Snap(&x,&y);
			m_currentConn->waypoints.push_back( wxPoint(x,y) );
			Modify();
			m_lastLineX = m_lastLineY = -1000;
			m_lineModeErase = false;
		}
	}
	else if ( m_currentUnit != 0 && m_currentConnPt == 0 )
	{
		m_origX = evt.GetX();
		m_origY = evt.GetY();
	
		ClientToScreen( &m_origX, &m_origY );

		m_diffX = m_diffY = 0;

		m_moveModeErase = false;
		m_movingUnit = m_currentUnit;
	}
	else if ( m_currentConnPt != 0 && m_currentConn == 0  
		&& m_currentUnit != 0 && !m_currentConnPt->isinput)
	{
		m_currentConn = new tcConn;
		m_currentConn->start = m_currentUnit;
		m_currentConn->start_pt = m_currentConnPt;
		m_lastLineX = m_lastLineY = 0;
		m_lineModeErase = false;
	}	
	else if ( m_movingWaypointConn == 0
		&& m_currentWaypointConn != 0
		&& m_currentWaypointIndex >= 0 )
	{
		m_movingWaypointConn = m_currentWaypointConn;
		m_movingWaypointIndex = m_currentWaypointIndex;
		m_moveWaypointErase = false;
	}
	else
		m_movingUnit = 0;

}

void tcLayoutCtrl::OnLeftDouble( wxMouseEvent & )
{
	if (m_currentUnit != 0 )
		EditUnit( m_currentUnit );
}

void tcLayoutCtrl::OnLeftUp( wxMouseEvent &evt )
{
	if ( m_movingUnit != 0 )
	{
		m_movingUnit->x += m_diffX;
		m_movingUnit->y += m_diffY;
		Snap(&m_movingUnit->x, &m_movingUnit->y);
		m_movingUnit = 0;
#ifdef TC_USE_OVERLAY
		wxClientDC dc( this );
		wxDCOverlay overlaydc( m_overlay, &dc );
		overlaydc.Clear();
		m_overlay.Reset();
#endif
		Modify();
		Refresh();
		UpdateStatus();
	}	
	else if ( m_movingWaypointConn )
	{
		int x = evt.GetX() - m_offsetX;
		int y = evt.GetY() - m_offsetY;
		Snap(&x,&y);
		m_movingWaypointConn->waypoints[ m_movingWaypointIndex ] = wxPoint(x,y);
		m_movingWaypointConn = 0;
		m_movingWaypointIndex = -1;
		m_moveWaypointErase = false;		
#ifdef TC_USE_OVERLAY
		wxClientDC dc( this );
		wxDCOverlay overlaydc( m_overlay, &dc );
		overlaydc.Clear();
		m_overlay.Reset();
#endif
		Modify();
		Refresh();
		UpdateStatus();
	}
}

void tcLayoutCtrl::OnRightDown( wxMouseEvent &evt )
{
	SetFocus();
	m_popupX = evt.GetX();
	m_popupY = evt.GetY();
	
	m_popupMenu->Enable( ID_DELETE_UNIT, m_currentUnit != 0 );
	m_popupMenu->Enable( ID_EDIT_CONNECTION, m_currentWaypointConn != 0 || (m_currentConnPt != 0 && m_currentConnPt->isinput));
	m_popupMenu->Enable( ID_DELETE_ALL_CONNECTIONS, m_currentConnPt != 0 && m_currentConn == 0);
	m_popupMenu->Enable( ID_DELETE_CONNECTION, m_currentWaypointConn != 0 );
	m_popupMenu->Enable( ID_ADD_WAYPOINT,  m_currentWaypointConn != 0 && m_currentWaypointIndex >= 0 );
	m_popupMenu->Enable( ID_DELETE_WAYPOINT, m_currentWaypointConn != 0 && m_currentWaypointIndex >= 0 );
	m_popupMenu->Enable( ID_EDIT_UNIT, m_currentUnit != 0 );
	m_popupMenu->Enable( ID_MOVE_UP, m_currentUnit != 0 );
	m_popupMenu->Enable( ID_MOVE_DOWN, m_currentUnit != 0 );
	PopupMenu( m_popupMenu, m_popupX, m_popupY );
}

void tcLayoutCtrl::DrawWaypointMoveOutline()
{
	wxClientDC dc(this);

#ifdef TC_USE_OVERLAY
	wxDCOverlay overlaydc( m_overlay, &dc );
	overlaydc.Clear();
#else
	dc.SetLogicalFunction( wxINVERT );
#endif

	dc.SetPen( wxPen( *wxBLACK, 1, wxPENSTYLE_DOT ) );
	wxSize cr = GetClientSize();
	int x = m_moveWaypointLastXY.x + m_offsetX;
	int y = m_moveWaypointLastXY.y + m_offsetY;
	dc.DrawLine( 0, y, cr.x, y );
	dc.DrawLine( x, 0, x, cr.y );
	dc.DrawCircle( m_moveWaypointLastXY.x + m_offsetX,
		m_moveWaypointLastXY.y + m_offsetY, 5 );
}

void tcLayoutCtrl::DrawMoveOutline()
{
	if (m_movingUnit)
	{
		wxClientDC dc(this);

#ifdef TC_USE_OVERLAY
		wxDCOverlay overlaydc( m_overlay, &dc );
		overlaydc.Clear();
		dc.SetPen( wxColour( 100, 100, 100 ) );
		dc.SetBrush( wxColour( 150, 150, 150, 150 ) );
#else
		dc.SetLogicalFunction( wxINVERT );
		dc.SetPen( wxPen( *wxBLACK, 3 ) );
		dc.SetBrush( *wxTRANSPARENT_BRUSH );
#endif
		int x = m_movingUnit->x + m_offsetX;
		int y = m_movingUnit->y + m_offsetY;

		x += m_diffX;
		y += m_diffY;

		Snap( &x, &y );

		dc.DrawRectangle( x, y, m_movingUnit->rdr.width, m_movingUnit->rdr.height );
	}
	
}

void tcLayoutCtrl::DrawSegmentOutline( const wxPoint &pt )
{
	wxClientDC dc(this);
	
#ifdef TC_USE_OVERLAY
	wxDCOverlay overlaydc( m_overlay, &dc );
	overlaydc.Clear();
#else
	dc.SetLogicalFunction( wxINVERT );
#endif

	dc.SetPen( wxPen( *wxBLACK, 2 ) );
	dc.DrawLine( pt, wxPoint(m_lastLineX, m_lastLineY) );

#ifdef TC_USE_OVERLAY
	if (!m_currentConn) return;

	wxPoint p1( m_offsetX + m_currentConn->start->x + m_currentConn->start_pt->pt.x,
		m_offsetY + m_currentConn->start->y + m_currentConn->start_pt->pt.y );


	for (size_t j = 0;j<m_currentConn->waypoints.size();j++)
	{
		wxPoint p0 = p1;
		p1 = m_currentConn->waypoints[j];			
		p1.x += m_offsetX;
		p1.y += m_offsetY;

		dc.DrawLine( p0, p1 );
	}
#endif

}

void tcLayoutCtrl::OnMouseMove( wxMouseEvent &evt )
{
	if (m_movingUnit != 0)
	{
#ifndef TC_USE_OVERLAY
		if (m_moveModeErase )
			DrawMoveOutline();
#endif

		int xroot = evt.GetX();
		int yroot = evt.GetY();
		ClientToScreen( &xroot, &yroot );
		
		m_diffX = xroot - m_origX;
		m_diffY = yroot - m_origY;

		DrawMoveOutline();
		m_moveModeErase = true;
		UpdateStatus();
		return;
	}

	if ( m_movingWaypointConn )
	{
#ifndef TC_USE_OVERLAY
		if (m_moveWaypointErase )
			DrawWaypointMoveOutline();
#endif
		
		m_moveWaypointLastXY.x = evt.GetX() - m_offsetX;
		m_moveWaypointLastXY.y = evt.GetY() - m_offsetY;
		Snap(&m_moveWaypointLastXY.x,&m_moveWaypointLastXY.y);
		DrawWaypointMoveOutline();

		m_moveWaypointErase = true;
	}

	if (m_currentConn != 0)
	{
		wxPoint pt( m_currentConn->start->x + m_currentConn->start_pt->pt.x,
			m_currentConn->start->y + m_currentConn->start_pt->pt.y );

		for (size_t j = 0;j<m_currentConn->waypoints.size();j++)
			pt = m_currentConn->waypoints[j];			
		
		pt.x += m_offsetX;
		pt.y += m_offsetY;

#ifndef TC_USE_OVERLAY
		if (m_lineModeErase)
			DrawSegmentOutline( pt );
#endif		
		m_lastLineX = evt.GetX();
		m_lastLineY = evt.GetY();
		Snap(&m_lastLineX,&m_lastLineY);
		DrawSegmentOutline( pt );

		m_lineModeErase = true;

	}
		

	int mx = evt.GetX();
	int my = evt.GetY();

	m_currentUnit = LocateUnit( mx, my );
	m_currentUnitIndex = -1;
	if ( m_currentUnit )
	{
		for (size_t i=0;i<m_units.size();i++)
			if (m_currentUnit == m_units[i])
				m_currentUnitIndex = i;
		
		m_currentConnPt = LocateConnection( mx, my );
	}
	else
	{
		m_currentConnPt = 0;
		FindWayPoint( mx, my, &m_currentWaypointConn, &m_currentWaypointIndex);
	}

	if ( (m_currentConnPt &&  !m_currentConn)
		|| ( m_currentConnPt &&  m_currentConn && CanFinishConnection()) ) SetCursor( wxCursor( ::wxCURSOR_BULLSEYE ) );
	else if (m_currentConn ) SetCursor( wxCursor( ::wxCURSOR_CROSS ));
	else if (m_currentWaypointConn ) SetCursor( wxCursor(::wxCURSOR_PENCIL));
	else SetCursor( wxCursor( ::wxCURSOR_ARROW ) );

		
	m_mouseLastX = mx;
	m_mouseLastY = my;

	UpdateStatus();
}

void tcLayoutCtrl::UpdateStatus()
{
	m_statusText->ChangeValue( GetStatusText() );
}

void tcLayoutCtrl::OnChar( wxKeyEvent &evt )
{
	switch( evt.GetKeyCode() )
	{
	case WXK_LEFT:
		m_offsetX += m_snapSpacing*5;
		m_lastLineX += m_snapSpacing*5;
		Refresh();
		break;
	case WXK_RIGHT:
		m_offsetX -= m_snapSpacing*5;
		m_lastLineX -= m_snapSpacing*5;
		Refresh();
		break;
	case WXK_UP:
		m_offsetY += m_snapSpacing*5;
		m_lastLineY += m_snapSpacing*5;
		Refresh();
		break;
	case WXK_DOWN:
		m_offsetY -= m_snapSpacing*5;
		m_lastLineY -= m_snapSpacing*5;
		Refresh();
		break;
	case WXK_ESCAPE:
		EscapeAction();
		break;
	}
	
	UpdateStatus();
}

void tcLayoutCtrl::EscapeAction()
{
	if (m_movingUnit)
	{
		m_movingUnit = 0;
		Refresh();
	}

	if (m_currentConn)
	{
		delete m_currentConn;
		m_currentConn = 0;
		Refresh();
	}

	if (m_movingWaypointConn)
	{
		m_movingWaypointConn = 0;
		m_movingWaypointIndex = -1;
	}	
}

void tcLayoutCtrl::SizeNewUnit( tcUnit *u )
{	
	wxClientDC dc(this);	
	dc.SetFont(*wxNORMAL_FONT);

	int ii = 0;
	int oo = 0;
	int wi = 0, wo = 0;

	int connh = m_snapSpacing*2;
	int ystart = connh * 4;
	
	// handle inputs on left side
	if ( !u->type->visual || u->type->visual->conn_left == 0)
	{
		int idx = 0;
		while( u->type->variables[idx].var_type != TCS_INVALID )
		{
			int size = dc.GetTextExtent( u->type->variables[idx].name ).GetWidth();
			if (u->type->variables[idx].var_type == TCS_INPUT )
			{
				u->rdr.inputs.push_back( tcConnPt( &u->type->variables[idx], u, idx, wxPoint(0, ystart+ii*connh ), true));
				ii++;
				if (size > wi)
					wi = size;
			}
			idx++;
		}
	}
	else if ( u->type->visual && u->type->visual->conn_left != 0)
	{
		wxArrayString in = wxStringTokenize( u->type->visual->conn_left, "," );
		for (size_t i=0;i<in.Count();i++)
		{
			wxString variable = in[i];
			int idx = 0;
			while ( u->type->variables[idx].var_type != TCS_INVALID )
			{
				if ( u->type->variables[idx].name == in[i] 
					&& (u->type->variables[idx].var_type == TCS_INPUT
						|| u->type->variables[idx].var_type == TCS_PARAM ) )
				{
					int size = dc.GetTextExtent( in[i] ).GetWidth();					
					u->rdr.inputs.push_back( tcConnPt( &u->type->variables[idx], u, idx, wxPoint(0, ystart+ii*connh ), true));
					ii++;
					if (size > wi)
						wi = size;

					break;
				}
				idx++;
			}
		}
	}


	// handle outputs on right side
	if ( !u->type->visual || u->type->visual->conn_right == 0)
	{
		int idx = 0;
		while( u->type->variables[idx].var_type != TCS_INVALID )
		{
			int size = dc.GetTextExtent( u->type->variables[idx].name ).GetWidth();
			if ( u->type->variables[idx].var_type == TCS_OUTPUT )
			{
				u->rdr.outputs.push_back( tcConnPt( &u->type->variables[idx], u, idx, wxPoint(0, ystart+oo*connh ), false));
				oo++;
				if (size > wo)
					wo = size;
			}	
			idx++;
		}
	}
	else if ( u->type->visual && u->type->visual->conn_right != 0 )
	{
		wxArrayString out = wxStringTokenize( u->type->visual->conn_right, "," );
		for (size_t i=0;i<out.Count();i++)
		{
			wxString variable = out[i];
			int idx = 0;
			while ( u->type->variables[idx].var_type != TCS_INVALID )
			{
				if ( u->type->variables[idx].name == out[i] 
					&& (u->type->variables[idx].var_type == TCS_OUTPUT
						|| u->type->variables[idx].var_type == TCS_DEBUG ) )
				{
					int size = dc.GetTextExtent( out[i] ).GetWidth();
					u->rdr.outputs.push_back( tcConnPt( &u->type->variables[idx], u, idx, wxPoint(0, ystart+oo*connh ), false));
					oo++;
					if (size > wo)
						wo = size;

					break;
				}
				idx++;
			}
		}
	}



	u->rdr.width = Snap(wi + wo + 100);
	u->rdr.height = Snap(( ii>oo?ii:oo ) * connh + ystart );

	if ( u->type->visual && u->type->visual->min_width > 0 )
	{
		int units = u->type->visual->min_width * m_snapSpacing;
		if ( u->rdr.width < units )
			u->rdr.width = Snap( units );
	}

	for (size_t i=0;i<u->rdr.outputs.size();i++)
		u->rdr.outputs[i].pt.x = u->rdr.width;

}

void tcLayoutCtrl::OnPopup( wxCommandEvent &evt )
{
	tcUnit *u = LocateUnit( m_popupX, m_popupY );
	if (evt.GetId() >= ID_CREATE && evt.GetId() < ID_CREATE_LAST)
	{
		tcType &t = m_types[evt.GetId() - ID_CREATE];

		int x = m_popupX;
		int y = m_popupY;
		Snap(&x,&y);

		u = new tcUnit;
		u->type = t.type;
		u->x = x - m_offsetX;
		u->y = y - m_offsetY;
		u->description = "no description";

		SizeNewUnit( u );

		m_units.push_back( u );
		Modify();

		Refresh();
		UpdateStatus();
	}
	else if ( evt.GetId() == ID_DELETE_UNIT && u != 0 )
	{
		std::vector< tcUnit* >::iterator it = std::find( m_units.begin(), m_units.end(), u );
		if ( it != m_units.end() )
		{
			size_t i = 0;
			while ( i < m_conns.size() )
			{
				if (m_conns[i]->start == u || m_conns[i]->end == u)
				{
					if ( m_currentConn == m_conns[i] )
						m_currentConn = 0;

					delete m_conns[i];
					m_conns.erase( m_conns.begin() + i );
				}
				else
					i++;
			}
			
			m_currentUnit = 0;
			m_currentUnitIndex = -1;
			m_currentConnPt = 0;

			delete u;
			m_units.erase( it );

			Modify();
			Refresh();
			UpdateStatus();
		}
	}
	else if ( evt.GetId() == ID_ADD_WAYPOINT
		&& m_currentWaypointConn != 0 
		&& m_currentWaypointIndex >= 0 )
	{
		int x = m_mouseLastX - m_offsetX;
		int y = m_mouseLastY - m_offsetY;
		Snap(&x,&y);
		x -= m_snapSpacing*2;
		y -= m_snapSpacing*2;

		m_currentWaypointConn->waypoints.insert( 
			m_currentWaypointConn->waypoints.begin() + m_currentWaypointIndex,
			wxPoint( x, y ) );
		
		Modify();
		Refresh();
		UpdateStatus();
	}
	else if ( evt.GetId() == ID_DELETE_WAYPOINT 
		&& m_currentWaypointConn != 0 
		&& m_currentWaypointIndex >= 0 )
	{
		m_currentWaypointConn->waypoints.erase( 
			m_currentWaypointConn->waypoints.begin() + m_currentWaypointIndex );
		m_currentWaypointConn = 0;
		m_currentWaypointIndex = -1;
		Modify();
		Refresh();
		UpdateStatus();
	}
	else if ( evt.GetId() == ID_DELETE_CONNECTION && m_currentWaypointConn != 0 )
	{
		std::vector< tcConn* >::iterator it = std::find( m_conns.begin(), m_conns.end(), m_currentWaypointConn );
		if ( it != m_conns.end() )
		{
			delete *it;
			m_conns.erase( it );
			m_currentWaypointConn = 0;
			m_currentWaypointIndex = -1;
			m_currentConn = 0;
			m_currentConnPt = 0;
			Modify();
			Refresh();
			UpdateStatus();
		}
	}
	else if ( evt.GetId() == ID_EDIT_CONNECTION && 
		( m_currentWaypointConn != 0 || (m_currentConnPt != 0 && m_currentConnPt->isinput)) )
	{
		tcConn *cp = 0;
		if (m_currentWaypointConn)
			cp = m_currentWaypointConn;
		else
			cp = FindConnFromPt( m_currentConnPt );

		if (!cp)
		{
			wxMessageBox("Could not find connection to edit");
			return;
		}
		wxString title = wxString::Format("Connection %d.%s --> %d.%s", UnitIndex(cp->start),
			cp->start_pt->ti->name,
			UnitIndex(cp->end),
			cp->end_pt->ti->name );
		
		wxDialog dlg( this, wxID_ANY, title, wxDefaultPosition,
			wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);
		
		wxTextCtrl *tol = new wxTextCtrl( &dlg, wxID_ANY, wxString::Format("%lg", cp->ftol));
		wxTextCtrl *index = new wxTextCtrl( &dlg, wxID_ANY, wxString::Format("%d", cp->index));

		
		wxGridSizer *grid = new wxGridSizer( 2,3,3 );
		grid->Add( new wxStaticText(&dlg, wxID_ANY, "Tolerance (+:%, -:absolute)"));
		grid->Add( tol );
		grid->Add( new wxStaticText(&dlg, wxID_ANY, "Array index (for array->number connections)"));
		grid->Add( index );

		wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
		szmain->Add( grid, 1, wxALL|wxEXPAND, 5 );
		szmain->Add( dlg.CreateButtonSizer(wxOK|wxCANCEL), 0, wxALL|wxEXPAND, 10 );

		dlg.SetSizerAndFit( szmain );

		if (dlg.ShowModal() == wxID_OK)
		{
			cp->ftol = atof( tol->GetValue().c_str() );
			cp->index = atoi( index->GetValue().c_str() );
		}
	}
	else if ( evt.GetId() == ID_DELETE_ALL_CONNECTIONS && m_currentConnPt != 0 && m_currentConn == 0 )
	{
		int ndel = 0;
		size_t idx = 0;
		while ( idx < m_conns.size() )
		{
			if (m_conns[idx]->start_pt == m_currentConnPt
				|| m_conns[idx]->end_pt == m_currentConnPt )
			{
				ndel++;
				delete m_conns[idx];
				m_conns.erase( m_conns.begin() + idx );
			}
			else
				idx++;
		}

		if (ndel > 0)
		{
			m_currentConn = 0;
			m_currentConnPt = 0;
			Modify();
			Refresh();
			UpdateStatus();
		}
	}
	else if ( (evt.GetId() == ID_MOVE_UP || evt.GetId() == ID_MOVE_DOWN ) && u != 0 )
	{
		size_t i;
		for (i=0;i<m_units.size();i++)
			if (u == m_units[i]) break;

		if (evt.GetId() == ID_MOVE_UP && i > 0 && i < m_units.size() )
		{
			tcUnit *temp = m_units[i-1];
			m_units[i-1] = u;
			m_units[i] = temp;
			Modify();
			Refresh();
			UpdateStatus();
		}
		else if (evt.GetId() == ID_MOVE_DOWN && i < m_units.size()-1 )
		{
			tcUnit *temp = m_units[i+1];
			m_units[i+1] = u;
			m_units[i] = temp;
			Modify();
			Refresh();
			UpdateStatus();
		}
	}
	else if ( evt.GetId() == ID_COPY_VIEW )
	{
		if (wxTheClipboard->Open())
		{
			wxTheClipboard->SetData(new wxBitmapDataObject( GetBitmap() ));
			wxTheClipboard->Close();
		}
	}
	else if ( evt.GetId() == ID_EDIT_UNIT && m_currentUnit != 0)
	{
		EditUnit( m_currentUnit );
	}
}


tcLayoutCtrl::tcUnit *tcLayoutCtrl::LocateUnit( int x, int y )
{
	x -= m_offsetX;
	y -= m_offsetY;
	for ( size_t i=0;i<m_units.size();i++ )
		if ( x >= m_units[i]->x 
			&& y >= m_units[i]->y
			&& x <= m_units[i]->x+m_units[i]->rdr.width
			&& y <= m_units[i]->y+m_units[i]->rdr.height )
			return m_units[i];

	return 0;
}


tcLayoutCtrl::tcConnPt *tcLayoutCtrl::LocateConnection( int x, int y )
{
	tcUnit *u = LocateUnit(x,y);
	if ( !u ) return 0;
	
	x -= m_offsetX;
	y -= m_offsetY;

	if ( x <= u->x + m_snapSpacing )
	{
		for (size_t i=0;i<u->rdr.inputs.size();i++)
			if ( abs(u->y + u->rdr.inputs[i].pt.y - y) <= m_snapSpacing )
				return &u->rdr.inputs[i];
	}
	else if ( x >= u->x + u->rdr.width - m_snapSpacing )
	{
		for (size_t i=0;i<u->rdr.outputs.size();i++)
			if ( abs(u->y + u->rdr.outputs[i].pt.y - y) <= m_snapSpacing )
				return &u->rdr.outputs[i];
	}
	return 0;
}

bool tcLayoutCtrl::FindWayPoint( int mx, int my, tcConn **conn, int *wpidx )
{
	int x = mx-m_offsetX;
	int y = my-m_offsetY;
	for (size_t i=0;i<m_conns.size();i++)
	{
		for (size_t j=0;j<m_conns[i]->waypoints.size();j++)
		{
			wxPoint wp = m_conns[i]->waypoints[j];
			if (Distance(x,y,wp.x,wp.y)<5)
			{
				*conn = m_conns[i];
				*wpidx = j;
				return true;
			}
		}
	}
	*conn = 0;
	*wpidx = -1;
	return false;
}

float tcLayoutCtrl::Distance(int x1, int y1, int x2, int y2)
{
	return (float)sqrt( (double)(x2-x1)*(x2-x1) + (double)(y2-y1)*(y2-y1) );
}

void tcLayoutCtrl::Snap(int *x, int *y)
{
	*x = Snap(*x);
	*y = Snap(*y);
}

int tcLayoutCtrl::Snap( int v )
{
	int multiples = (int)(v / m_snapSpacing);
	float dist1 = (float)abs(m_snapSpacing*multiples - v);
	float dist2 = (float)abs(m_snapSpacing*(multiples+1) - v);
	if (dist1 < dist2) return m_snapSpacing*multiples;
	else return m_snapSpacing*(multiples+1);
}

wxString tcLayoutCtrl::GetStatusText()
{	
	int x = m_mouseLastX-m_offsetX;
	int y = m_mouseLastY-m_offsetY;
	Snap(&x,&y);

	wxString status = wxString::Format( "{%d %d %d %d}", x, y, m_offsetX, m_offsetY );

	if (m_currentUnit != 0)
		status << "  Unit " << m_currentUnitIndex << " '" << m_currentUnit->type->name << "' ";

	if (m_currentConnPt != 0)
	{
		wxString datatype = "string";
		switch(m_currentConnPt->ti->data_type)
		{
		case TCS_STRING: datatype = "<string>"; break;
		case TCS_NUMBER: datatype = "<number>"; break;
		case TCS_ARRAY: datatype = "<array>"; break;
		case TCS_MATRIX: datatype = "<matrix>"; break;
		default: datatype = "<invalid>"; break;
		}
		status << "  [ " << m_currentConnPt->idx << ": " << m_currentConnPt->ti->name 
			<< "  (" << m_currentConnPt->ti->units <<  ")  " 
			<< datatype <<  "  " << m_currentConnPt->ti->label << " ]";
	}

	if ( m_currentConn != 0 )
	{
		if ( CanFinishConnection() )
			status << "  finish connection here?";
		else
			status << "    connecting";
	}
	else if (m_movingUnit != 0)
		status << "    moving unit";
	else if (m_movingWaypointConn != 0)
		status << "    moving waypoint";
	else
		status << "    ready";
	return status;
}

wxGrid *tcLayoutCtrl::CreateUnitDataGrid( wxWindow *parent, tcstypeinfo *type )
{
	wxGrid *grid = new wxGrid( parent, wxID_ANY );	
	tcsvarinfo *vl = type->variables;
	int idx=0;
	while( vl[idx].var_type != TCS_INVALID )
		idx++;
	
	grid->CreateGrid( idx, 7 );
	grid->SetColLabelValue( 0, "Type" );
	grid->SetColLabelValue( 1, "Data" );
	grid->SetColLabelValue( 2, "Name" );
	grid->SetColLabelValue( 3, "Label" );
	grid->SetColLabelValue( 4, "Units" );
	grid->SetColLabelValue( 5, "Group" );
	grid->SetColLabelValue( 6, "Meta" );

	idx = 0;
	while ( vl[idx].var_type != TCS_INVALID )
	{
		wxColour rowc = wxColour( 225, 255, 255 );
		wxString stype = "input";
		if ( vl[idx].var_type == TCS_OUTPUT )
		{
			stype = "output";
			rowc = wxColour( 255, 225, 255 );
		}
		else if (vl[idx].var_type == TCS_DEBUG )
		{
			stype = "debug";
			rowc = wxColour( 225, 225, 255 );
		}
		else if (vl[idx].var_type == TCS_PARAM )
		{
			stype = "param";
			rowc = wxColour( 255, 255, 225 );
		}

		wxString sdata = "number";
		if (vl[idx].data_type == TCS_STRING ) sdata = "string";
		else if (vl[idx].data_type == TCS_ARRAY ) sdata = "array";
		else if (vl[idx].data_type == TCS_MATRIX ) sdata = "matrix";

		grid->SetCellValue( idx, 0, stype );
		grid->SetCellValue( idx, 1, sdata );
		grid->SetCellValue( idx, 2, vl[idx].name );
		grid->SetCellValue( idx, 3, vl[idx].label );
		grid->SetCellValue( idx, 4, vl[idx].units );
		grid->SetCellValue( idx, 5, vl[idx].group );
		grid->SetCellValue( idx, 6, vl[idx].meta );

		for (int j=0;j<7;j++)
			grid->SetCellBackgroundColour( idx, j, rowc );

		idx++;
	}

	grid->AutoSizeColumns(false);

	return grid;
}

void tcLayoutCtrl::EditUnit( tcUnit *u )
{
	wxDialog dlg( this, wxID_ANY, wxString::Format("Edit Properties of unit %d", UnitIndex(u)), wxDefaultPosition,
		wxDefaultSize, wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER);

	wxString str;
	for (size_t i=0;i<u->values.size();i++)
		str += u->values[i].key + "=" + u->values[i].value + "\n";

	wxStaticText *type = new wxStaticText( &dlg, wxID_ANY, wxString::Format("Unit %d Type %s", UnitIndex(u), u->type->name) );
	wxFont font = *wxNORMAL_FONT;
	font.SetWeight( wxFONTWEIGHT_BOLD );
	type->SetFont( font );

	wxTextCtrl *desc = new wxTextCtrl( &dlg, wxID_ANY, u->description );
	wxTextCtrl *vals = new wxTextCtrl( &dlg, wxID_ANY, str, 
		wxDefaultPosition, wxSize(600, 300), wxTE_MULTILINE|wxTE_DONTWRAP );

	wxGrid *grid = CreateUnitDataGrid( &dlg, u->type );	
	grid->SetInitialSize( wxSize( 250, 150 ) );

	int fontSize = 12;
#ifdef __WXMAC__
	fontSize = 14;
#endif
	vals->SetFont(wxFont(fontSize, wxFONTFAMILY_MODERN, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false, "Consolas"));
			
	wxBoxSizer *szmain = new wxBoxSizer( wxVERTICAL );
	szmain->Add( type, 0, wxALL|wxEXPAND, 5 );
	szmain->Add( desc, 0, wxALL|wxEXPAND, 5 );
	szmain->Add( vals, 2, wxALL|wxEXPAND, 5 );
	szmain->Add( grid, 1, wxALL|wxEXPAND, 5 );
	szmain->Add( dlg.CreateButtonSizer( wxOK|wxCANCEL ), 0, wxALL|wxEXPAND, 10 );
	dlg.SetSizerAndFit( szmain );
	dlg.CenterOnScreen();
	vals->SetFocus();
	if (dlg.ShowModal() == wxID_OK)
	{
		u->description = desc->GetValue();
		u->values.clear();
		wxArrayString lines = wxStringTokenize( vals->GetValue(), "\n" );
		for (size_t i=0;i<lines.Count();i++)
		{
			int eqpos = lines[i].Index('=');
			if (eqpos > 0)
			{
				SPair p;
				p.key = lines[i].Mid(0, eqpos);
				p.value = lines[i].Mid(eqpos+1);
				u->values.push_back( p );
			}
		}

		Refresh();
		UpdateStatus();
	}
}

bool tcLayoutCtrl::IsAssignedInputValue( tcConnPt *pt )
{
	for (size_t i=0;i<m_conns.size();i++)
		if (m_conns[i]->end_pt == pt)
			return true;

	for (size_t i=0;i<pt->unit->values.size();i++)
		if ( pt->unit->values[i].key == pt->ti->name )
			return true;
	
	return false;
}

bool tcLayoutCtrl::GetNumericValue( tcUnit *u, const wxString &name, double *val )
{
	for (size_t i=0;i<u->values.size();i++)
		if (u->values[i].key == name)
			return u->values[i].value.ToDouble(val);
	return false;
}

wxString tcLayoutCtrl::GetNetlist()
{
	wxString buf;	
	for ( size_t i=0;i<m_units.size(); i++ )
	{
		buf << "unit " << i << " " << m_units[i]->type->name << " '" << m_units[i]->description << "'\n";
		for (size_t j=0;j<m_units[i]->values.size();j++)
			buf << "\t" << m_units[i]->values[j].key << "=" << m_units[i]->values[j].value << "\n";
	}

	for ( size_t i=0;i<m_conns.size();i++ )
	{
		int u1 = UnitIndex(m_conns[i]->start);
		int u2 = UnitIndex( m_conns[i]->end );
		buf << u1 << ":" << m_conns[i]->start_pt->idx << " " << m_conns[i]->start_pt->ti->name
			<< " --> " << u2 << ":" << m_conns[i]->end_pt->idx << " " << m_conns[i]->end_pt->ti->name << "\n";
	}
	return buf;
}

wxString tcLayoutCtrl::GetLKScript()
{
	wxString buf = "setup_system = define() {\n\tclear( );\n";
	for ( size_t i=0;i<m_units.size(); i++ )
	{
		buf << "\n\t u" << i << " = add_unit( \"" << m_units[i]->type->name << "\", \"" << m_units[i]->description << "\" );\n";
		for ( size_t j=0;j<m_units[i]->values.size(); j++ )
		{
			const char *key = m_units[i]->values[j].key.c_str();
			const char *val = m_units[i]->values[j].value.c_str();

			// find variable type
			int data_type = TCS_INVALID;
			tcsvarinfo *vl = m_units[i]->type->variables;
			unsigned int idx=0;
			while( vl[idx].var_type != TCS_INVALID )
			{
				if ( strcmp(vl[idx].name, key) == 0 )
				{
					data_type = vl[idx].data_type;
					break;
				}
				idx++;
			}

			if ( data_type == TCS_INVALID ) continue;
			tcsvalue tv;
			tv.type = TCS_INVALID;
			if ( tcskernel::parse_unit_value( &tv, data_type, val ) )
			{
				switch( data_type )
				{
				case TCS_NUMBER:
					buf << "\tset_value( u" << i << ", \"" << key << "\", " << tv.data.value << " );\n";
					break;
				case TCS_STRING:
					buf << "\tset_value( u" << i <<", \"" << key << "\", \"" << tv.data.cstr << "\" );\n";
					break;
				case TCS_ARRAY:
					buf << "\tset_value( u" << i << ", \"" << key << "\", [ ";
					for (idx=0;idx<tv.data.array.length;idx++ )
					{
						buf << tv.data.array.values[idx];
						if ( idx < tv.data.array.length-1 )
							buf << ", ";
					}
					buf << "] );\n";
					break;
				case TCS_MATRIX:
					buf << "\tset_value( u" << i << ", \"" << key << "\", [ ";
					for ( idx=0;idx<tv.data.matrix.nrows;idx++ )
					{
						buf << "[";
						for (unsigned int col=0;col<tv.data.matrix.ncols;col++ )
						{
							buf << TCS_MATRIX_INDEX(&tv, idx, col);
							if ( col < tv.data.matrix.ncols - 1 )
								buf << ", ";
						}
						buf << "]";
						if ( idx < tv.data.matrix.nrows - 1 )
							buf << ", ";
					}
					buf << "] );\n";
					break;
				}
			}
		}
	}

	buf << "\n";

	for ( size_t i=0;i<m_conns.size(); i++ )
	{
		int u1 = UnitIndex(m_conns[i]->start);
		int u2 = UnitIndex( m_conns[i]->end );

		buf << "\tconnect( u" << u1 << ", \"" << m_conns[i]->start_pt->ti->name << "\", u" 
			<< u2 << ", \"" << m_conns[i]->end_pt->ti->name << "\", " << m_conns[i]->ftol << ", " << m_conns[i]->index << " );\n";
	}

	buf << "};\n";
	return buf;
}

bool tcLayoutCtrl::LoadSystemInKernel( tcskernel *kern )
{
	kern->clear_units();
	
	for ( size_t i=0;i<m_units.size(); i++ )
	{
		int u = kern->add_unit( m_units[i]->type->name, (const char*)m_units[i]->description.c_str() );
		for ( size_t j=0;j<m_units[i]->values.size(); j++ )
		{
			const char *key = m_units[i]->values[j].key.c_str();
			const char *val = m_units[i]->values[j].value.c_str();
			if (!kern->parse_unit_value( u, key, val ))
			{
				m_error.Printf( "failed to parse value on unit %d, %s=%s", u, key, val );
				return false;
			}
		}
	}

	for ( size_t i=0;i<m_conns.size();i++ )
	{
		int u1 = UnitIndex(m_conns[i]->start);
		int u2 = UnitIndex( m_conns[i]->end );
		if (!kern->connect( u1,
			m_conns[i]->start_pt->idx,
			u2,
			m_conns[i]->end_pt->idx,
			m_conns[i]->ftol,
			m_conns[i]->index ))
		{
			m_error.Printf( "failed to connect [%d:%d] --> [%d:%d]  (tol %lg, idx %d)",u1,
				m_conns[i]->start_pt->idx,
				u2,
				m_conns[i]->end_pt->idx,
				m_conns[i]->ftol,
				m_conns[i]->index );
			return false;
		}
	}

	return true;
}

tcLayoutCtrl::tcConn *tcLayoutCtrl::FindConnFromPt( tcConnPt *pt )
{
	tcUnit *u = pt->unit;
	for (size_t i=0;i<m_conns.size();i++)
		if ( m_conns[i]->end == u
			&& m_conns[i]->end_pt == pt )
			return m_conns[i];
	return 0;
}
