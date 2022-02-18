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
#include <QTextEdit>
#include <QPushButton>
#include <QRadioButton>
#include <QMessageBox>
#include <QCheckBox>
//-----------------------------------------------------------------------------
#include "anim_dlg.h"
#undef sprintf	// fix libintl bug of defining sprintf
extern int animDelay;
//-----------------------------------------------------------------------------
AnimParam::AnimParam(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Animation setup"));
	QHBoxLayout *a;
	QVBoxLayout *o, *g;
	QLabel *lbl;
	QPushButton *b;
	o = new QVBoxLayout(this);
	lbl = new QLabel(_("Redraw picture for $0 equal to"),this);
	o->addWidget(lbl);
	a = new QHBoxLayout();		o->addLayout(a);
	g = new QVBoxLayout();		a->addLayout(g);
	rbt = new QRadioButton(_("strings"),this);
	connect(rbt, SIGNAL(clicked()),this, SLOT(setRBT()));
	g->addWidget(rbt);
	text = new QTextEdit(this);	g->addWidget(text);
	connect(text,SIGNAL(textChanged()),this,SLOT(setRBT()));

	g = new QVBoxLayout();		a->addLayout(g);
	rbf = new QRadioButton(_("values"),this);
	connect(rbf, SIGNAL(clicked()),this, SLOT(setRBF()));
	g->addWidget(rbf);
	lbl = new QLabel(_("from"),this);
	g->addWidget(lbl, Qt::AlignLeft);
	p1 = new QLineEdit(this);	g->addWidget(p1);
	connect(p1,SIGNAL(textChanged(QString)),this,SLOT(setRBF()));
	lbl = new QLabel(_("to"),this);
	g->addWidget(lbl, Qt::AlignLeft);
	p2 = new QLineEdit(this);	g->addWidget(p2);
	connect(p2,SIGNAL(textChanged(QString)),this,SLOT(setRBF()));
	lbl = new QLabel(_("with step"),this);
	g->addWidget(lbl, Qt::AlignLeft);
	dp = new QLineEdit(this);	g->addWidget(dp);	dp->setText("1");
	connect(dp,SIGNAL(textChanged(QString)),this,SLOT(setRBF()));
	b = new QPushButton(_("Cancel"), this);	g->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);	g->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(fillRes()));
	b->setDefault(true);
	// general
	a = new QHBoxLayout();		o->addLayout(a);
	b = new QPushButton(_("Put to script"), this);	a->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(putTxt()));
	lbl = new QLabel(_("Delay (in ms)"),this);	a->addWidget(lbl);
	delay = new QLineEdit(this);	a->addWidget(delay);
	QString s;	s.sprintf("%d",animDelay);	delay->setText(s);
	// export to gif/jpeg
	a = new QHBoxLayout();		o->addLayout(a);
//	fname = new QLineEdit(this);	a->addWidget(fname);
	gif = new QCheckBox(_("Export to GIF"), this);		a->addWidget(gif);
	jpg = new QCheckBox(_("Save JPEG frames"), this);	a->addWidget(jpg);
	gifOn = jpgOn = false;
}
//-----------------------------------------------------------------------------
AnimParam::~AnimParam()	{}
//-----------------------------------------------------------------------------
void AnimParam::fillRes()
{
//	gifName = fname->text();
	gifOn = gif->isChecked();
	jpgOn = jpg->isChecked();
	animDelay = delay->text().toInt();
	if(rbt->isChecked())	{	res = text->toPlainText();	accept();	}
	else if(rbf->isChecked())
	{
		res = "";
		QString s;
		double x, x2=p2->text().toDouble(), dx=dp->text().toDouble();
		for(x=p1->text().toDouble();x<x2;x+=dx)
			res = res+QString::number(x,'g',4)+"\n";
		accept();
	}
	else	QMessageBox::warning(this,_("UDAV - animation"),
								 _("You should select one of case"));
}
//-----------------------------------------------------------------------------
void AnimParam::putTxt()
{
	QString s, t;
	if(!p1->text().isEmpty() && !p2->text().isEmpty() && !dp->text().isEmpty())
	{
		s = "##c "+p1->text()+" "+p2->text()+" "+dp->text()+"\n";
		emit putText(s);
	}
	if(text->toPlainText().isEmpty())	return;
	int i, n = text->toPlainText().count('\n')+1;
	for(i=0;i<n;i++)
	{
		t = text->toPlainText().section('\n',i,i);
		if(!t.isEmpty())	s = s+"##a "+t+"\n";
	}
	emit putText(s);
}
//-----------------------------------------------------------------------------
void AnimParam::setRBF()	{	rbt->setChecked(false);	rbf->setChecked(true);	}
//-----------------------------------------------------------------------------
void AnimParam::setRBT()	{	rbf->setChecked(false);	rbt->setChecked(true);	}
//-----------------------------------------------------------------------------
void AnimParam::setResult(const QString &s)
{	text->setText(s);	rbf->setChecked(false);	rbt->setChecked(true);	}
//-----------------------------------------------------------------------------
void AnimParam::setResult(double a1, double a2, double da)
{
	p1->setText(QString::number(a1));
	p2->setText(QString::number(a2));
	dp->setText(QString::number(da));
	rbt->setChecked(false);	rbf->setChecked(true);
}
//-----------------------------------------------------------------------------

