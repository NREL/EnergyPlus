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
#ifndef OPTION_DLG_H
#define OPTION_DLG_H
//-----------------------------------------------------------------------------
#include <QDialog>
#if defined(_MSC_VER)
#include <mgl2/define.h>
#endif
class QLineEdit;
class QComboBox;
class QRadioButton;
//-----------------------------------------------------------------------------
/// Dialog for selecting command options
class OptionDialog : public QDialog
{
Q_OBJECT
public:
	OptionDialog(QWidget *parent=0);
	~OptionDialog();
	
	QString getOption()	{	return result;	}
private slots:
	void prepareResult();
private:
	QString result;
	QLineEdit *x1, *x2, *y1, *y2, *z1, *z2, *c1, *c2;
	QLineEdit *alpha, *amb, *dif, *val, *mn, *fs, *leg;
	QComboBox *cut, *lig;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
