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
#include <QPushButton>
#include <QComboBox>
#include <QSpinBox>
#include <QLayout>
#include <QLabel>
#include <QLineEdit>
//-----------------------------------------------------------------------------
#include <mgl2/mgl.h>
extern mglParse parser;
#include "data_dlg.h"
//-----------------------------------------------------------------------------
DataDialog::DataDialog(QWidget* parent): QDialog(parent)
{
	setWindowTitle(_("UDAV - Insert style/scheme"));
	QHBoxLayout *h;
	QVBoxLayout *v;
	QGridLayout *g;
	QLabel *l;
	QPushButton *b;

	v = new QVBoxLayout(this);
	h = new QHBoxLayout();	v->addLayout(h);
	l = new QLabel(_("Data name"), this);	h->addWidget(l);
	name = new QComboBox(this);	h->addWidget(name);
	
	g = new QGridLayout();	v->addLayout(g);

	l = new QLabel("x1", this);	g->addWidget(l, 0, 0);
	x1 = new QSpinBox(this);	x1->setMinimum(-1);	g->addWidget(x1, 0, 1);
	l = new QLabel("x2", this);	g->addWidget(l, 0, 2);
	x2 = new QSpinBox(this);	x2->setMinimum(-1);	g->addWidget(x2, 0, 3);
	
	l = new QLabel("y1", this);	g->addWidget(l, 1, 0);
	y1 = new QSpinBox(this);	y1->setMinimum(-1);	g->addWidget(y1, 1, 1);
	l = new QLabel("y2", this);	g->addWidget(l, 1, 2);
	y2 = new QSpinBox(this);	y2->setMinimum(-1);	g->addWidget(y2, 1, 3);
	
	l = new QLabel("z1", this);	g->addWidget(l, 2, 0);
	z1 = new QSpinBox(this);	z1->setMinimum(-1);	g->addWidget(z1, 2, 1);
	l = new QLabel("z2", this);	g->addWidget(l, 2, 2);
	z2 = new QSpinBox(this);	z2->setMinimum(-1);	g->addWidget(z2, 2, 3);

	x1->setValue(-1);	y1->setValue(-1);	z1->setValue(-1);
	x2->setValue(-1);	y2->setValue(-1);	z2->setValue(-1);
	
	l = new QLabel(_("Operation"));	g->addWidget(l, 3, 0);
	oper = new QComboBox(this);		g->addWidget(oper, 3, 1);
	oper->addItem(_("none"));		oper->addItem(_("sum"));
	oper->addItem(_("min"));	oper->addItem(_("max"));
	l = new QLabel("along", this);	g->addWidget(l, 3, 2);
	dirs = new QComboBox(this);		g->addWidget(dirs, 3, 3);
	dirs->addItem("xyz");
	dirs->addItem("x");	dirs->addItem("y");	dirs->addItem("z");
	dirs->addItem("xy");	dirs->addItem("xz");	dirs->addItem("yz");

	connect(name, SIGNAL(currentIndexChanged(int)), this, SLOT(nameChanged()));
	connect(x1, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(x2, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(y1, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(y2, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(z1, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(z2, SIGNAL(valueChanged(int)), this, SLOT(updateRes()));
	connect(oper, SIGNAL(currentIndexChanged(int)), this, SLOT(updateRes()));
	connect(dirs, SIGNAL(currentIndexChanged(int)), this, SLOT(updateRes()));

	sizes = new QLabel(_("Result"));	v->addWidget(sizes);
	res = new QLineEdit(this);		v->addWidget(res);
	connect(res, SIGNAL(textChanged(QString)), this, SLOT(userRes()));
	
	h = new QHBoxLayout();	v->addLayout(h);	h->addStretch(1);
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(accept()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
void DataDialog::nameChanged()
{
	QString var = name->currentText();
	wchar_t *txt=new wchar_t[var.length()+1];
	var.toWCharArray(txt);	txt[var.length()]=0;
	mglData dat=parser.Calc(txt);	delete []txt;
	x1->setMaximum(dat.nx-1);	x1->setValue(-1);
	x2->setMaximum(dat.nx-1);	x2->setValue(-1);
	y1->setMaximum(dat.ny-1);	y1->setValue(-1);
	y2->setMaximum(dat.ny-1);	y2->setValue(-1);
	z1->setMaximum(dat.nz-1);	z1->setValue(-1);
	z2->setMaximum(dat.nz-1);	z2->setValue(-1);
}
//-----------------------------------------------------------------------------
void DataDialog::updateRes()
{
	result = name->currentText();
	int nx1 = x1->value(), nx2 = x2->value(), ny1 = y1->value(), ny2 = y2->value(), nz1 = z1->value(), nz2 = z2->value();
	if(nx1>=0 || ny1>=0 || nz1>=0 || nx2>=0 || ny2>=0 || nz2>=0)
		result += "(" + (nx1<0?"":QString::number(nx1)) + ":" + (nx2<0?"":QString::number(nx2)) + "," +
						(ny1<0?"":QString::number(ny1)) + ":" + (ny2<0?"":QString::number(ny2)) + "," +
						(nz1<0?"":QString::number(nz1)) + ":" + (nz2<0?"":QString::number(nz2)) + ")";
	if(oper->currentIndex()>0)
		result = "{" + oper->currentText() + " " + result + " '" + dirs->currentText() + "'}";
	wchar_t *txt=new wchar_t[result.length()+1];
	result.toWCharArray(txt);	txt[result.length()]=0;
	mglData dat=parser.Calc(txt);	delete []txt;
	sizes->setText(_("Result (will have sizes ") + QString::number(dat.nx)+"*"+QString::number(dat.ny)+"*"+QString::number(dat.nz)+")"	);
	res->setText(result);
}
//-----------------------------------------------------------------------------
void DataDialog::updateNames()
{
	name->clear();
	long i, n = parser.GetNumVar();
	for(i=0;i<n;i++)
	{
		const mglDataA *v = parser.GetVar(i);
		if(v)	name->addItem(QString::fromWCharArray(v->Name()));
	}
}
//-----------------------------------------------------------------------------
void DataDialog::userRes()
{
	QString txt = res->text();
	if(txt != result)
	{	result = txt;	sizes->setText(_("Result"));	}
}
//-----------------------------------------------------------------------------
