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
#ifndef TEXT_PNL_H
#define TEXT_PNL_H
//-----------------------------------------------------------------------------
#include <QWidget>
#include <QStringList>
#include "textedit.h"
//-----------------------------------------------------------------------------
class QMenu;
class QPrinter;
class QBoxLayout;
class FindDialog;
class OptionDialog;
class StyleDialog;
class SetupDialog;
class NewCmdDialog;
class PlotPanel;
class SubplotDialog;
//-----------------------------------------------------------------------------
class TextPanel : public QWidget
{
Q_OBJECT
public:
	QMenu *menu;
	TextEdit *edit;		///< script itself
	PlotPanel *graph;	///< NOTE: have to be filled!!!
	NewCmdDialog *newCmdDlg;
	SubplotDialog *subplotDlg;

	TextPanel(QWidget *parent = 0);
	~TextPanel();
	void load(const QString &fileName);
	void save(const QString &fileName);
	inline bool isModified()	{	return edit->document()->isModified();	}
	inline void setModified(bool m)	{	edit->document()->setModified(m);	}
	inline void moveCursor(QTextCursor::MoveOperation c)
	{	edit->moveCursor(c);	}
	void setCompleter(bool en);
	QString selection();

signals:
	void setCurrentFile(const QString &s);
	void setStatus(const QString &s);

public slots:
	void setEditorFont(QFont *f=0);
//	void setEditPos(bool bottom);
	void animPutText(const QString &);
//	void putText(const QString &txt);
	void putLine(const QString &txt, bool replace=false);
	void setCursorPosition(int);

	void addOptions();
	void addStyle();
	void insNVal();
	void insFile();
	void insPath();
	void insFitF();
	void insPrim();
	void newCmd(int n=-1);
	void addSetup();

	void refreshData();
	void printText();
	void find();
	bool findText(const QString &str="", bool cs=false, bool fw=true);
	void replText(const QString &str, const QString &txt, bool cs=false, bool fw=true);

private:

	QStringList words, vars;
	QPrinter *printer;

	FindDialog *findDialog;
	OptionDialog *optDialog;
	StyleDialog *stlDialog;
	QWidget *dataOpenDlg;
	SetupDialog *setupDlg;

	void toolTop(QBoxLayout *l);
//	void toolLeft(QBoxLayout *l);

	void saveHDF5(const QString &fileName);
	void loadHDF5(const QString &fileName);
};
//-----------------------------------------------------------------------------
#endif // TEXT_PNL_H
//-----------------------------------------------------------------------------
