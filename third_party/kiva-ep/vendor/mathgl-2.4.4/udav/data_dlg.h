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
#ifndef DATADIALOG_H
#define DATADIALOG_H
//-----------------------------------------------------------------------------
#include <qdialog.h>
class QComboBox;
class QLineEdit;
class QSpinBox;
class QLabel;
class QShowEvent;
//-----------------------------------------------------------------------------
/// Selecting styles of command (like line style, color scheme, font style, axis style)
class DataDialog : public QDialog
{
	Q_OBJECT
public:
	QString getData()	{	return result;	};
	void updateNames();
	DataDialog(QWidget *parent = 0);
	~DataDialog(){};
protected:
	virtual void showEvent(QShowEvent *ev)
	{	updateNames();	QDialog::showEvent(ev);	}
private slots:
	void nameChanged();
	void updateRes();
	void userRes();
private:
	QString result;
	QComboBox *name, *oper, *dirs;
	QLineEdit *res;
	QSpinBox  *x1, *y1, *z1, *x2, *y2, *z2;
	QLabel *sizes;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
