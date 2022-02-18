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
#include <QSettings>
#include <QCheckBox>
#include <QLayout>
#include <QPushButton>
#include "hint_dlg.h"
#include "mgl2/data_cf.h"
#include "mgl2/wnd.h"
//-----------------------------------------------------------------------------
//
//	Hint dialog
//
//-----------------------------------------------------------------------------
HintDialog::HintDialog(QWidget *parent) : QDialog(parent)
{
	for(int i=0;mgl_hints[i];i++)	hints.append(mgl_hints[i]);
	numHints=hints.size();
	cur = int(mgl_rnd()*numHints);
	setWindowTitle(_("UDAV - Hint"));
	QHBoxLayout *a;
	QPushButton *b;
	QVBoxLayout *o = new QVBoxLayout(this);
	text = new QTextEdit(this);	o->addWidget(text);
	text->setReadOnly(true);	text->setText(hints[cur]);

	start = new QCheckBox(_("Show at startup"), this);	o->addWidget(start);
	start->setChecked(true);

	a = new QHBoxLayout;	o->addLayout(a);
	b = new QPushButton(_("Prev"), this);		a->addWidget(b);
	connect(b, SIGNAL(clicked()), this, SLOT(prevClicked()));
	b = new QPushButton(_("Next"), this);		a->addWidget(b);
	connect(b, SIGNAL(clicked()), this, SLOT(nextClicked()));
	b = new QPushButton(_("Close"), this);	a->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(close()));
}
//-----------------------------------------------------------------------------
void HintDialog::closeEvent(QCloseEvent *)
{
	QSettings settings("udav","UDAV");
	settings.setPath(QSettings::IniFormat, QSettings::UserScope, "UDAV");
	settings.beginGroup("/UDAV");
	settings.setValue("/showHint", start->isChecked());
	settings.endGroup();
}
//-----------------------------------------------------------------------------
void udavShowHint(QWidget *p)
{	HintDialog *hd = new HintDialog(p);	hd->exec();	}
//-----------------------------------------------------------------------------
