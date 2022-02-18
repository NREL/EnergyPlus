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
#ifndef OPEN_DLG_H
#define OPEN_DLG_H
//-----------------------------------------------------------------------------
#include <QDialog>
class QLineEdit;
class QComboBox;
class QRadioButton;
//-----------------------------------------------------------------------------
/// Dialog for selecting command options
class DataOpenDialog : public QDialog
{
Q_OBJECT
public:
	DataOpenDialog(QWidget *parent=0);
	~DataOpenDialog();
	inline const QString getCode()	{	return code;	}
	inline const QString getName()	{	return data;	}
	void setFile(const QString &fname);
private slots:
	void prepareResult();
	void selectScr();
private:
	QString code, data, file;
	QLineEdit *nx, *ny, *nz, *name;
	QRadioButton *rA, *rM, *r2, *r3;
	QComboBox *scr;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
