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
#include <QLabel>
#include <QLayout>
#include <QLineEdit>
#include <QCheckBox>
#include <QPushButton>
#include <QCloseEvent>
#include "mgl2/define.h"
#include "find_dlg.h"
//-----------------------------------------------------------------------------
FindDialog::FindDialog(QWidget *parent) : QDialog(parent)
{
	QLabel *lbl;
	QHBoxLayout *a;
	setWindowTitle(_("UDAV - Find"));
	QVBoxLayout *o = new QVBoxLayout;
	a = new QHBoxLayout;	o->addLayout(a);
	lbl = new QLabel(_("Find what:"), this);			a->addWidget(lbl);
	line = new QLineEdit(this);	lbl->setBuddy(line);	a->addWidget(line);
	a = new QHBoxLayout;	o->addLayout(a);
	lbl = new QLabel(_("Replace by:"), this);			a->addWidget(lbl);
	text = new QLineEdit(this);	lbl->setBuddy(text);	a->addWidget(text);

	caseUse = new QCheckBox(_("Match case"), this);	o->addWidget(caseUse);
	backward = new QCheckBox(_("Search backward"), this);	o->addWidget(backward);

	a = new QHBoxLayout(this);	a->setMargin(11);
	a->setSpacing(6);			a->addLayout(o);
	o = new QVBoxLayout;		a->addLayout(o);
	find = new QPushButton(_("Find"), this);		o->addWidget(find);
	find->setDefault(true);	find->setEnabled(false);
	repl= new QPushButton(_("Replace"), this);	o->addWidget(repl);
	repl->setEnabled(false);
	cancel = new QPushButton(_("Close"), this);	o->addWidget(cancel);
	o->addStretch(1);
	connect(line, SIGNAL(textChanged(const QString &)), this, SLOT(enableFind(const QString &)));
	connect(find, SIGNAL(clicked()), this, SLOT(findClicked()));
	connect(repl, SIGNAL(clicked()), this, SLOT(replClicked()));
	connect(cancel, SIGNAL(clicked()),this, SLOT(close()));
}
//-----------------------------------------------------------------------------
FindDialog::~FindDialog()	{}
//-----------------------------------------------------------------------------
void FindDialog::findClicked()
{	emit findText(line->text(), caseUse->isChecked(), backward->isChecked());	}
//-----------------------------------------------------------------------------
void FindDialog::replClicked()
{	emit replText(line->text(), text->text(), caseUse->isChecked(), backward->isChecked());	}
//-----------------------------------------------------------------------------
void FindDialog::enableFind(const QString &txt)
{	find->setEnabled(!txt.isEmpty());	repl->setEnabled(!txt.isEmpty());	}
//-----------------------------------------------------------------------------
void FindDialog::closeEvent(QCloseEvent *event)
{
	emit replText("", "", false, false);
	event->accept();
}
//-----------------------------------------------------------------------------
