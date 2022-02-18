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
#include <QPushButton>
#include <mgl2/mgl.h>
#include "args_dlg.h"
extern mglParse parser;
//-----------------------------------------------------------------------------
QDialog *createArgsDlg(QWidget *p)	{	return new ArgsDialog(p);	}
//-----------------------------------------------------------------------------
ArgsDialog::ArgsDialog(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Set script arguments"));
	QHBoxLayout *h;
	QVBoxLayout *v;
	QGridLayout *g;
	QLabel *l;
	QPushButton *b;

	v = new QVBoxLayout(this);
	g = new QGridLayout();		v->addLayout(g);
	l = new QLabel(_("String for $1"), this);	g->addWidget(l, 0, 0);
	a[1] = new QLineEdit(this);					g->addWidget(a[1], 1, 0);
	l = new QLabel(_("String for $2"), this);	g->addWidget(l, 0, 1);
	a[2] = new QLineEdit(this);					g->addWidget(a[2], 1, 1);
	l = new QLabel(_("String for $3"), this);	g->addWidget(l, 2, 0);
	a[3] = new QLineEdit(this);					g->addWidget(a[3], 3, 0);
	l = new QLabel(_("String for $4"), this);	g->addWidget(l, 2, 1);
	a[4] = new QLineEdit(this);					g->addWidget(a[4], 3, 1);
	l = new QLabel(_("String for $5"), this);	g->addWidget(l, 4, 0);
	a[5] = new QLineEdit(this);					g->addWidget(a[5], 5, 0);
	l = new QLabel(_("String for $6"), this);	g->addWidget(l, 4, 1);
	a[6] = new QLineEdit(this);					g->addWidget(a[6], 5, 1);
	l = new QLabel(_("String for $7"), this);	g->addWidget(l, 6, 0);
	a[7] = new QLineEdit(this);					g->addWidget(a[7], 7, 0);
	l = new QLabel(_("String for $8"), this);	g->addWidget(l, 6, 1);
	a[8] = new QLineEdit(this);					g->addWidget(a[8], 7, 1);
	l = new QLabel(_("String for $9"), this);	g->addWidget(l, 8, 0);
	a[9] = new QLineEdit(this);					g->addWidget(a[9], 9, 0);
	l = new QLabel(_("String for $0"), this);	g->addWidget(l, 8, 1);
	a[0] = new QLineEdit(this);					g->addWidget(a[0], 9, 1);

	h = new QHBoxLayout();	h->addStretch(1);	v->addLayout(h);
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(putArguments()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
ArgsDialog::~ArgsDialog()	{}
//-----------------------------------------------------------------------------
void ArgsDialog::putArguments()
{
	int len=0;
	for(int i=0;i<10;i++)
			if(a[i]->text().length()>len)
				len = a[i]->text().length();
	wchar_t *str = new wchar_t[len+2];
	for(int i=0;i<10;i++)
	{
		QString s = a[i]->text();
		int j, n = s.length();
		for(j=0;j<n;j++)	str[j] = (s[j]).unicode();
		str[j] = 0;
		parser.AddParam(i, str);
	}
	delete []str;	accept();
}
//-----------------------------------------------------------------------------
