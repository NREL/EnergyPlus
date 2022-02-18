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
#include "files_dlg.h"
#include "mgl2/define.h"
//-----------------------------------------------------------------------------
FilesDialog::FilesDialog(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Set template parameters"));
	QHBoxLayout *h;
	QVBoxLayout *v;
	QGridLayout *g;
	QLabel *l;
	QPushButton *b;

	v = new QVBoxLayout(this);
	g = new QGridLayout();		v->addLayout(g);
	l = new QLabel(_("String for %1"), this);	g->addWidget(l, 0, 0);
	a[0] = new QLineEdit(this);					g->addWidget(a[0], 1, 0);
	l = new QLabel(_("String for %2"), this);	g->addWidget(l, 0, 1);
	a[1] = new QLineEdit(this);					g->addWidget(a[1], 1, 1);
	l = new QLabel(_("String for %3"), this);	g->addWidget(l, 2, 0);
	a[2] = new QLineEdit(this);					g->addWidget(a[2], 3, 0);
	l = new QLabel(_("String for %4"), this);	g->addWidget(l, 2, 1);
	a[3] = new QLineEdit(this);					g->addWidget(a[3], 3, 1);
	l = new QLabel(_("String for %5"), this);	g->addWidget(l, 4, 0);
	a[4] = new QLineEdit(this);					g->addWidget(a[4], 5, 0);
	l = new QLabel(_("String for %6"), this);	g->addWidget(l, 4, 1);
	a[5] = new QLineEdit(this);					g->addWidget(a[5], 5, 1);
	l = new QLabel(_("String for %7"), this);	g->addWidget(l, 6, 0);
	a[6] = new QLineEdit(this);					g->addWidget(a[6], 7, 0);
	l = new QLabel(_("String for %8"), this);	g->addWidget(l, 6, 1);
	a[7] = new QLineEdit(this);					g->addWidget(a[7], 7, 1);
	l = new QLabel(_("String for %9"), this);	g->addWidget(l, 8, 0);
	a[8] = new QLineEdit(this);					g->addWidget(a[8], 9, 0);

	h = new QHBoxLayout();	h->addStretch(1);	v->addLayout(h);
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(putArguments()));
	b->setDefault(true);	narg=0;
}
//-----------------------------------------------------------------------------
FilesDialog::~FilesDialog()	{}
//-----------------------------------------------------------------------------
void FilesDialog::putArguments()
{
	for(int i=0;i<9;i++)	s[i] = a[i]->text();
	accept();
}
//-----------------------------------------------------------------------------
void FilesDialog::setNumFiles(int n)
{
	for(int i=0;i<9;i++)	a[i]->setEnabled(i<n);
	narg = n;	if(narg>9)	narg=9;
}
//-----------------------------------------------------------------------------
QString FilesDialog::putFiles(const QString &str)
{
	QString res=str;
	switch(narg)
	{
	case 1:	res = str.arg(s[0]);	break;
	case 2:	res = str.arg(s[0],s[1]);	break;
	case 3:	res = str.arg(s[0],s[1],s[2]);	break;
	case 4:	res = str.arg(s[0],s[1],s[2],s[3]);	break;
	case 5:	res = str.arg(s[0],s[1],s[2],s[3],s[4]);	break;
	case 6:	res = str.arg(s[0],s[1],s[2],s[3],s[4],s[5]);	break;
	case 7:	res = str.arg(s[0],s[1],s[2],s[3],s[4],s[5],s[6]);	break;
	case 8:	res = str.arg(s[0],s[1],s[2],s[3],s[4],s[5],s[6],s[7]);	break;
	case 9:	res = str.arg(s[0],s[1],s[2],s[3],s[4],s[5],s[6],s[7],s[8]);	break;
	}
	return res;
}
//-----------------------------------------------------------------------------
