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
#include <QLayout>
#include <QLineEdit>
#include <QToolBar>
#include <QPushButton>
#include <QTextBrowser>
#include <QToolButton>
#include <stdio.h>
//-----------------------------------------------------------------------------
#include "mgl2/define.h"
#include "help_pnl.h"
extern QString pathHelp;
void raisePanel(QWidget *w);
//-----------------------------------------------------------------------------
QWidget *createHlpPanel(QWidget *p)		{	return new HelpPanel(p);	}
void showHelpMGL(QWidget *p,QString s)
{
	HelpPanel *hlp = dynamic_cast<HelpPanel *>(p);
	if(hlp)	hlp->showHelp(s);
}
//void showExMGL(QWidget *hlp)			{	((HelpPanel *)hlp)->showExamples();	}
//-----------------------------------------------------------------------------
HelpPanel::HelpPanel(QWidget *parent) : QWidget(parent)
{

	QToolBar *t = new QToolBar(this);	t->setMovable(false);
	QVBoxLayout *v = new QVBoxLayout(this);	v->addWidget(t);
	help = new QTextBrowser(this);	v->addWidget(help);	help->setOpenExternalLinks(false);

	t->addAction(QPixmap(":/png/go-previous.png"), _("Backward"), help, SLOT(backward()));
	entry = new QLineEdit(this);	t->addWidget(entry);
	connect(entry, SIGNAL(textChanged(const QString &)), this, SLOT(showHelp(const QString &)));
	connect(entry, SIGNAL(returnPressed()), this, SLOT(showHelp()));
	t->addAction(QPixmap(":/png/go-next.png"), _("Forward"), help, SLOT(forward()));
	t->addSeparator();
//	t->addAction(QPixmap(":/png/help-faq.png"), _("Examples"), this, SLOT(showExamples()));
	t->addAction(QPixmap(":/png/zoom-in.png"), _("Zoom in text"), this, SLOT(zoomIn()));
	t->addAction(QPixmap(":/png/zoom-out.png"), _("Zoom out text"), this, SLOT(zoomOut()));
	setWindowTitle(_("Help"));
}
//-----------------------------------------------------------------------------
// void HelpPanel::showExamples()
// {
// 	QStringList s;	s<<(pathHelp);
// 	help->setSearchPaths(s);
// 	setWindowTitle("Examples");	raisePanel(this);
// 	help->setSource("mgl_en"+"_2.html");
// }
//-----------------------------------------------------------------------------
void HelpPanel::showHelp(const QString &txt)
{
	QString cmd=txt;
	raisePanel(this);
	QStringList s;	s<<(pathHelp);
	help->setSearchPaths(s);
	if(cmd.isEmpty())	cmd = entry->text().trimmed();
	// NOTE disable other translations for help files due to Qt bug
	if(cmd.isEmpty())	help->setSource("mgl_en"+QString(".html"));
	else	help->setSource("mgl_en"+QString(".html#")+cmd);
	setWindowTitle("Help");
}
//-----------------------------------------------------------------------------
void HelpPanel::zoomIn()
{	QFont f(help->font());	f.setPointSize(f.pointSize()+1);	help->setFont(f);	}
//-----------------------------------------------------------------------------
void HelpPanel::zoomOut()
{	QFont f(help->font());	f.setPointSize(f.pointSize()-1);	help->setFont(f);	}
//-----------------------------------------------------------------------------
