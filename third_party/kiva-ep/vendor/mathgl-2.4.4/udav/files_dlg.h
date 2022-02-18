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
#ifndef FILES_DLG_H
#define FILES_DLG_H
//-----------------------------------------------------------------------------
#include <QDialog>
#if defined(_MSC_VER)
#include <mgl2/define.h>
#endif
class QLineEdit;
class QComboBox;
class QRadioButton;
//-----------------------------------------------------------------------------
/// Dialog for enetring script arguments $0...$9
class FilesDialog : public QDialog
{
Q_OBJECT
public:
	FilesDialog(QWidget *parent=0);
	~FilesDialog();
	QString putFiles(const QString &str);
	void setNumFiles(int n);

private slots:
	void putArguments();
private:
	QLineEdit *a[9];
	QString s[9];
	int narg;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
