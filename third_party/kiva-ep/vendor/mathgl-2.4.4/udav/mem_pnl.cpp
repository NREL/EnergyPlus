/***************************************************************************
 *   Copyright (C) 2008 by Alexey Balakin                                  *
 *   mathgl.abalakin@gmail.com                                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <QLayout>
#include <QTableWidget>
#include <QToolBar>
#include <QInputDialog>
#include <QMessageBox>
#include <mgl2/mgl.h>
//-----------------------------------------------------------------------------
#include "mem_pnl.h"
#include "info_dlg.h"
#undef sprintf	// fix libintl bug of defining sprintf
//-----------------------------------------------------------------------------
#include "xpm/table.xpm"
#include "xpm/preview.xpm"
//-----------------------------------------------------------------------------
extern bool mglAutoSave;
extern mglParse parser;
QWidget *newDataWnd(InfoDialog *inf, QWidget *wnd, mglDataA *v);
void refreshData(QWidget *w);
//-----------------------------------------------------------------------------
QWidget *createMemPanel(QWidget *p)	// NOTE: parent should be MainWindow
{
	MemPanel *m = new MemPanel(p);
	m->wnd = p;	return m;
}
//-----------------------------------------------------------------------------
void refreshMemPanel(QWidget *p)
{
	MemPanel *m = dynamic_cast<MemPanel *>(p);
	if(m)	m->refresh();
}
//-----------------------------------------------------------------------------
MemPanel::MemPanel(QWidget *parent) : QWidget(parent)
{
	infoDlg = new InfoDialog(this);
	infoDlg->setModal(true);	infoDlg->allowRefresh=false;

	QToolBar *t = new QToolBar(this);	t->setMovable(false);
	QVBoxLayout *v = new QVBoxLayout(this);	v->addWidget(t);
	t->addAction(QPixmap(":/png/document-new.png"), _("Create new data array"), this, SLOT(newTable()));
	t->addAction(QPixmap(table_xpm), _("Edit selected data array"), this, SLOT(editData()));
	t->addAction(QPixmap(":/png/edit-delete.png"), _("Delete selected data array"), this, SLOT(delData()));
	t->addAction(QPixmap(preview_xpm), _("Properties of selected data array"), this, SLOT(infoData()));
	t->addAction(QPixmap(":/png/view-refresh.png"), _("Update list of data arrays"), this, SLOT(refresh()));
	t->addSeparator();
	t->addAction(QPixmap(":/png/edit-clear.png"), _("Delete ALL data arrays"), this, SLOT(delAllData()));

	colSort = 0;
	tab = new QTableWidget(this);	tab->setColumnCount(3);	v->addWidget(tab);
	QStringList sl;	sl<<_("Name")<<_("Sizes")<<_("Memory");
	tab->setHorizontalHeaderLabels(sl);
	connect(tab, SIGNAL(cellClicked(int,int)), this, SLOT(tableClicked(int,int)));
	connect(tab, SIGNAL(cellDoubleClicked(int,int)), this, SLOT(tableDClicked(int,int)));

	setWindowTitle(_("Memory"));
}
//-----------------------------------------------------------------------------
void MemPanel::tableClicked(int, int col)
{	colSort = col;	tab->sortItems(col);	}
//-----------------------------------------------------------------------------
void MemPanel::tableDClicked(int row, int)	{	editData(row);	}
//-----------------------------------------------------------------------------
void MemPanel::newTable()
{
	bool ok;
	QString name = QInputDialog::getText(this, _("UDAV - New variable"),
				_("Enter name for new variable"), QLineEdit::Normal, "", &ok);
	if(!ok || name.isEmpty())	return;
	mglDataA *v = parser.AddVar(name.toLocal8Bit().constData());
	QWidget *t;
	if(v->o)	t = (QWidget *)v->o;
	else		t = newDataWnd(infoDlg,wnd,v);
	t->showMaximized();	t->activateWindow();
	refresh();
}
//-----------------------------------------------------------------------------
void MemPanel::editData(int n)
{
	if(tab->rowCount()<1)	return;
	if(n<0)	n = tab->currentRow();
	if(n<0)	n = 0;
	mglDataA *v = parser.FindVar(tab->item(n,0)->text().toLocal8Bit().constData());
	if(!v)	return;
	QWidget *t;
	if(v->o)	t = (QWidget *)v->o;
	else		t = newDataWnd(infoDlg,wnd,v);
	t->showMaximized();	t->activateWindow();
}
//-----------------------------------------------------------------------------
void MemPanel::delData()
{
	if(tab->rowCount()<1)	return;
	int	n = tab->currentRow();
	if(n<0)	n = 0;
	mglDataA *v = parser.FindVar(tab->item(n,0)->text().toLocal8Bit().constData());
	if(v && v->o)	((QWidget *)v->o)->close();
	parser.DeleteVar(tab->item(n,0)->text().toLocal8Bit().constData());
	refresh();
}
//-----------------------------------------------------------------------------
void MemPanel::delAllData()
{
	if(QMessageBox::information(this, _("UDAV - delete all data"),
			_("Do you want to delete all data?"), QMessageBox::No,
			QMessageBox::Yes)!=QMessageBox::Yes)	return;
	parser.DeleteAll();	refresh();
}
//-----------------------------------------------------------------------------
void MemPanel::infoData()
{
	if(tab->rowCount()<1)	return;
	int	n = tab->currentRow();
	if(n<0)	n = 0;
	mglDataA *v = parser.FindVar(tab->item(n,0)->text().toLocal8Bit().constData());
	if(!v)	return;
	infoDlg->setVar(v);
	QString s = QString::fromWCharArray(v->Name());
	infoDlg->setWindowTitle(s + _(" - UDAV preview"));
	infoDlg->refresh();
	infoDlg->show();
}
//-----------------------------------------------------------------------------
void MemPanel::refresh()
{
	long n = parser.GetNumVar(), m=0;
	for(long i=0;i<n;i++)	if(parser.GetVar(i))	m++;
	tab->setRowCount(m);
	QString s;
	QTableWidgetItem *it;
	Qt::ItemFlags flags=Qt::ItemIsSelectable|Qt::ItemIsEnabled;
	for(long i=m=0;i<n;i++)
	{
		mglDataA *v = parser.GetVar(i);
		if(!v)	continue;
		s = QString::fromWCharArray(v->Name());
		it = new QTableWidgetItem(s);
		tab->setItem(m,0,it);	it->setFlags(flags);
		s.sprintf("%ld * %ld * %ld", v->GetNx(), v->GetNy(), v->GetNz());
		it = new QTableWidgetItem(s);
		tab->setItem(m,1,it);	it->setFlags(flags);
		it->setTextAlignment(Qt::AlignHCenter|Qt::AlignVCenter);
		long sv = 0;
		if(dynamic_cast<mglData*>(v))	sv = v->GetNN()*sizeof(mreal)+sizeof(mglData);
		else if(dynamic_cast<mglDataC*>(v))	sv = v->GetNN()*sizeof(dual)+sizeof(mglDataC);
		else if(dynamic_cast<mglDataV*>(v))	sv = sizeof(mglDataV);
		else if(dynamic_cast<mglDataW*>(v))	sv = sizeof(mglDataW);
		else if(dynamic_cast<mglDataF*>(v))	sv = sizeof(mglDataF);
		else if(dynamic_cast<mglDataR*>(v))	sv = sizeof(mglDataR);
		else if(dynamic_cast<mglDataT*>(v))	sv = sizeof(mglDataT);
		if(sv==0)	s = _("unknown");
#if MGL_SIZEOF_LONG>4
//		else if((sv>>80L)>0)	s.sprintf("%ld Yb",sv>>80L);
//		else if((sv>>70L)>0)	s.sprintf("%ld Zb",sv>>70L);
		else if((sv>>60L)>0)	s.sprintf("%ld Eb",sv>>60L);
		else if((sv>>50L)>0)	s.sprintf("%ld Pb",sv>>50L);
		else if((sv>>40L)>0)	s.sprintf("%ld Tb",sv>>40L);
#endif
		else if((sv>>30L)>0)	s.sprintf("%ld Gb",sv>>30L);
		else if((sv>>20L)>0)	s.sprintf("%ld Mb",sv>>20L);
		else if((sv>>10L)>0)	s.sprintf("%ld Kb",sv>>10L);
		else	s.sprintf("%ld b",sv);
		it = new QTableWidgetItem(s);
		tab->setItem(m,2,it);	it->setFlags(flags);
		it->setTextAlignment(Qt::AlignRight|Qt::AlignVCenter);
		if(v->o)	refreshData((QWidget *)v->o);
		m++;
	}
	tab->sortItems(colSort);
}
//-----------------------------------------------------------------------------
