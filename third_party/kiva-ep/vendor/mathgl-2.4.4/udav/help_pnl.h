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
#ifndef HELP_PNL_H
#define HELP_PNL_H
//-----------------------------------------------------------------------------
#include <QWidget>
#if defined(_MSC_VER)
#include <mgl2/define.h>
#endif
class QTextBrowser;
class QLineEdit;
//-----------------------------------------------------------------------------
class HelpPanel : public QWidget
{
Q_OBJECT
public:
	HelpPanel(QWidget *parent = 0);
public slots:
//	void showExamples();
	void showHelp(const QString &cmd=QString::null);
	void zoomIn();
	void zoomOut();
private:
	QTextBrowser *help;
	QLineEdit *entry;
};
//-----------------------------------------------------------------------------
#endif // HELP_PNL_H
//-----------------------------------------------------------------------------
