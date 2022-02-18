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
#ifndef FIND_DLG_H
#define FIND_DLG_H
//-----------------------------------------------------------------------------
#include <QDialog>
class QCheckBox;
class QLineEdit;
class QPushButton;
#if defined(_MSC_VER)
#include <mgl2/define.h>
#endif
//-----------------------------------------------------------------------------
/// Dialog for finding something in text
class FindDialog : public QDialog
{
Q_OBJECT
public:
	FindDialog(QWidget *parent = 0);
	~FindDialog();
signals:
	void findText(const QString &str, bool cs, bool fw);
	void replText(const QString &str, const QString &txt, bool cs, bool fw);
protected:
	void closeEvent(QCloseEvent *event);
private slots:
	void findClicked();
	void replClicked();
	void enableFind(const QString &text);
private:
	QLineEdit *line;
	QLineEdit *text;
	QCheckBox *caseUse;
	QCheckBox *backward;
	QPushButton *find;
	QPushButton *repl;
	QPushButton *cancel;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
