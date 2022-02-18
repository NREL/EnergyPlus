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
#ifndef MEM_PNL_H
#define MEM_PNL_H
#include <QWidget>
class InfoDialog;
class QTableWidget;
//-----------------------------------------------------------------------------
class MemPanel : public QWidget
{
Q_OBJECT
public:
	QWidget *wnd;
	MemPanel(QWidget *parent = 0);
public slots:
	void refresh();		///< refresh list of variables
private slots:
	void newTable();
	void editData(int n=-1);
	void infoData();
	void delAllData();
	void delData();
	void tableClicked(int row, int col);
	void tableDClicked(int row, int col);
private:
	InfoDialog *infoDlg;	///< Data preview and information
	QTableWidget *tab;
	int colSort;
};
//-----------------------------------------------------------------------------
#endif // MEM_PNL_H
//-----------------------------------------------------------------------------
