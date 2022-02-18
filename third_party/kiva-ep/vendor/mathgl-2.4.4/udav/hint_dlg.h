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
#ifndef HINT_DLG_H
#define HINT_DLG_H
//-----------------------------------------------------------------------------
#include <QTextEdit>
#include <QDialog>
class QCheckBox;
//-----------------------------------------------------------------------------
/// Dialog for showing hints
class HintDialog : public QDialog
{
Q_OBJECT
public:
	HintDialog(QWidget *parent = 0);
	~HintDialog()	{}
protected:
	void closeEvent(QCloseEvent *event);
private slots:
	void nextClicked()
	{	cur = (cur+1)%numHints;	text->setText(hints[cur]);	}
	void prevClicked()
	{	cur = (cur+numHints-1)%numHints;	text->setText(hints[cur]);	}
private:
	int cur;
	int numHints;
	QVector<QString> hints;
	QTextEdit *text;
	QCheckBox *start;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
