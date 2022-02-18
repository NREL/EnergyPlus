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
#ifndef PROPDIALOG_H
#define PROPDIALOG_H
//-----------------------------------------------------------------------------
#include <qdialog.h>
class QLabel;
class QPushButton;
class QCheckBox;
class QLineEdit;
class QComboBox;
//-----------------------------------------------------------------------------
/// Set UDAV general properties
class PropDialog : public QDialog
{
Q_OBJECT
public:
	PropDialog(QWidget *parent = 0);
	~PropDialog();
signals:
	void sizeChanged(int w, int h);
	void propUpdated();
private slots:
	void applyChanges();
	void setC0()	{	setC(0);	}
	void setC1()	{	setC(1);	}
	void setC2()	{	setC(2);	}
	void setC3()	{	setC(3);	}
	void setC4()	{	setC(4);	}
	void setC5()	{	setC(5);	}
	void setC6()	{	setC(6);	}
	void setC7()	{	setC(7);	}
	void setC8()	{	setC(8);	}
	void setC9()	{	setC(9);	}
	void setF();
	void getPathH();
	void getPathF();
private:
	void setC(int k);
	QLabel *lbl;
	QPushButton *cb[10];
	QCheckBox *run, *edt, *load, *save, *cmpl, *high, *dots, *wheel;	//, *pure;
	QLineEdit *hlp, *defW, *defH;
	QFont defFont;
	QColor cc[10];
	QComboBox *lng, *fnt;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
