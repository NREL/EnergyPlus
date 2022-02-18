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
#ifndef TREE_PNL_H
#define TREE_PNL_H
//-----------------------------------------------------------------------------
#include <QWidget>
#include <QStringList>
//-----------------------------------------------------------------------------
class QMenu;
class QBoxLayout;
class OptionDialog;
class NewCmdDialog;
class PlotPanel;
class TextEdit;
//-----------------------------------------------------------------------------
class TreePanel : public QWidget
{
Q_OBJECT
public:
	// NOTE: these 3 pointers have to be filled!!!
	TextEdit *edit;		///< script itself
	PlotPanel *graph;	
	NewCmdDialog *newCmdDlg;

	TreePanel(QWidget *parent = 0);
	~TreePanel();

public slots:
	void refresh();			///< refresh tree according new script
	void setPosition(int);	///< select item for given line number
	void hidePlot(int);		///< hide plot for given item
	void annotatePlot(int);	///< add annotation for given item
	void newCmd();			///< add new command + refresh tree

private:
	void toolTop(QBoxLayout *l);
};
//-----------------------------------------------------------------------------
#endif // TREE_PNL_H
//-----------------------------------------------------------------------------
