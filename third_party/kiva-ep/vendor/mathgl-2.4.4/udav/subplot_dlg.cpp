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
#include <QCheckBox>
#include <QRadioButton>
#include <QSpinBox>
#include <QLayout>
#include <QLabel>
#include <QLineEdit>
#include <mgl2/qmathgl.h>
#include "subplot_dlg.h"
#include "style_dlg.h"
#undef sprintf
//-----------------------------------------------------------------------------
void convertFromGraph(QPixmap &pic, mglGraph *gr, uchar **buf);
//-----------------------------------------------------------------------------
SubplotDialog::SubplotDialog(QWidget *parent) : QDialog(parent)
{
	grBuf = 0;
	setWindowTitle(_("UDAV - Setup inplot"));
	QLabel *l;
	QPushButton *b;
	QVBoxLayout *v = new QVBoxLayout(this), *u;
	QGridLayout *g = new QGridLayout;	v->addLayout(g);
	g->setColumnStretch(2, 1);	g->setColumnStretch(4, 1);
	g->setColumnStretch(6, 1);	g->setColumnStretch(8, 1);
	g->setColumnStretch(10, 1);	g->setAlignment(Qt::AlignTop);
	// SubPlot section
	cb = new QRadioButton("SubPlot",this);	g->addWidget(cb,0,0);
	cb->setToolTip(_("Set drawing area as cell of matrix nx*ny."));
	connect(cb,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("nx",this);	g->addWidget(l,0,1);
	bn = new QSpinBox(this);	g->addWidget(bn,0,2);
	bn->setMinimum(1);	bn->setToolTip(_("Horizontal size"));
	connect(bn,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ny",this);	g->addWidget(l,0,3);
	bm = new QSpinBox(this);	g->addWidget(bm,0,4);
	bm->setMinimum(1);	bm->setToolTip(_("Vertical size"));
	connect(bm,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ind",this);	g->addWidget(l,0,5);
	bk = new QSpinBox(this);	g->addWidget(bk,0,6);
	bk->setMinimum(0);	bk->setToolTip(_("Cell index"));
	connect(bk,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));

	// MultiPlot section
	cm = new QRadioButton("MultiPlot",this);	g->addWidget(cm,1,0);
	cm->setToolTip(_("Set drawing area as cells of matrix nx*ny."));
	connect(cm,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("nx",this);	g->addWidget(l,1,1);
	mn = new QSpinBox(this);	g->addWidget(mn,1,2);
	mn->setMinimum(1);	mn->setToolTip(_("Horizontal size"));
	connect(mn,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ny",this);	g->addWidget(l,1,3);
	mm = new QSpinBox(this);	g->addWidget(mm,1,4);
	mm->setMinimum(1);	mm->setToolTip(_("Vertical size"));
	connect(mm,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ind",this);	g->addWidget(l,1,5);
	mk = new QSpinBox(this);	g->addWidget(mk,1,6);
	mk->setMinimum(0);	mk->setToolTip(_("Starting cell index"));
	connect(mk,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("dx",this);	g->addWidget(l,1,7);
	mx = new QSpinBox(this);	g->addWidget(mx,1,8);
	mx->setMinimum(1);	mx->setToolTip(_("Width of selected cells"));
	connect(mx,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("dy",this);	g->addWidget(l,1,9);
	my = new QSpinBox(this);	g->addWidget(my,1,10);
	my->setMinimum(1);	my->setToolTip(_("Height of selected cells"));
	connect(my,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));

	// GridPlot section
	cg = new QRadioButton("GridPlot",this);	g->addWidget(cg,2,0);
	cg->setToolTip(_("Set drawing area as cell of matrix nx*ny."));
	connect(cg,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("nx",this);	g->addWidget(l,2,1);
	gn = new QSpinBox(this);	g->addWidget(gn,2,2);
	gn->setMinimum(1);	gn->setToolTip(_("Horizontal size"));
	connect(gn,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ny",this);	g->addWidget(l,2,3);
	gm = new QSpinBox(this);	g->addWidget(gm,2,4);
	gm->setMinimum(1);	gm->setToolTip(_("Vertical size"));
	connect(gm,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ind",this);	g->addWidget(l,2,5);
	gk = new QSpinBox(this);	g->addWidget(gk,2,6);
	gk->setMinimum(0);	gk->setToolTip(_("Cell index"));
	connect(gk,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("d",this);	g->addWidget(l,2,7);
	gd = new QLineEdit(this);	g->addWidget(gd,2,8);
	gd->setToolTip(_("Distance between cells"));
	connect(gd,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));

	// ColumnPlot section
	cc = new QRadioButton("ColumnPlot",this);	g->addWidget(cc,3,0);
	cc->setToolTip(_("Set drawing area as cells of column."));
	connect(cc,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("num",this);	g->addWidget(l,3,1);
	cn = new QSpinBox(this);	g->addWidget(cn,3,2);
	cn->setMinimum(1);	cn->setToolTip(_("Size of column"));
	connect(cn,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ind",this);	g->addWidget(l,3,5);
	ck = new QSpinBox(this);	g->addWidget(ck,3,6);
	ck->setMinimum(0);	ck->setToolTip(_("Cell index"));
	connect(ck,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("d",this);	g->addWidget(l,3,7);
	cd = new QLineEdit(this);	g->addWidget(cd,3,8);
	cd->setToolTip(_("Distance between cells"));
	connect(cd,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));

	// StickPlot section
	cs = new QRadioButton("StickPlot",this);	g->addWidget(cs,4,0);
	cs->setToolTip(_("Set drawing area as cells of stick."));
	connect(cc,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("num",this);	g->addWidget(l,4,1);
	sn = new QSpinBox(this);	g->addWidget(sn,4,2);
	sn->setMinimum(1);	sn->setToolTip(_("Size of stick"));
	connect(sn,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("ind",this);	g->addWidget(l,4,5);
	sk = new QSpinBox(this);	g->addWidget(sk,4,6);
	sk->setMinimum(0);	sk->setToolTip(_("Cell index"));
	connect(sk,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));

	// InPlot section
	ci = new QRadioButton("InPlot",this);	g->addWidget(ci,5,0);
	ci->setToolTip(_("Set drawing area as cells of matrix nx*ny."));
	connect(ci,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	l = new QLabel("x1",this);	g->addWidget(l,5,1);
	x1 = new QLineEdit(this);	g->addWidget(x1,5,2);
	x1->setText("0");	x1->setToolTip(_("Left bottom edge"));
	connect(x1,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("x2",this);	g->addWidget(l,5,3);
	x2 = new QLineEdit(this);	g->addWidget(x2,5,4);
	x2->setText("1");	x2->setToolTip(_("Right bottom edge"));
	connect(x2,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("y1",this);	g->addWidget(l,5,5);
	y1 = new QLineEdit(this);	g->addWidget(y1,5,6);
	y1->setText("0");	y1->setToolTip(_("Left top edge"));
	connect(y1,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel("y2",this);	g->addWidget(l,5,7);
	y2 = new QLineEdit(this);	g->addWidget(y2,5,8);
	y2->setText("1");	y2->setToolTip(_("Right top edge"));
	connect(y2,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));

	QHBoxLayout *h, *H;
	h = new QHBoxLayout;	v->addLayout(h);
	u = new QVBoxLayout;	h->addLayout(u);

	H = new QHBoxLayout;	u->addLayout(H);
	l = new QLabel(_("Rotate on"),this);	H->addWidget(l);
	l = new QLabel(QString::fromWCharArray(L"\u03b8"),this);	H->addWidget(l);
	tet = new QSpinBox(this);	H->addWidget(tet,1);	tet->setValue(0);	tet->setSingleStep(5);
	tet->setToolTip(_("Angle around x axis (in degrees)"));
	connect(tet,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel(QString::fromWCharArray(L"\u03c6"),this);	H->addWidget(l);
	phi = new QSpinBox(this);	H->addWidget(phi,1);	phi->setValue(0);	phi->setSingleStep(5);
	phi->setToolTip(_("Angle around z axis (in degrees)"));
	connect(phi,SIGNAL(valueChanged(QString)),this,SLOT(updatePic()));

	H = new QHBoxLayout;	u->addLayout(H);
	l = new QLabel(_("Aspect"),this);	H->addWidget(l);
	l = new QLabel(_("X/Z"),this);	H->addWidget(l);
	axz = new QLineEdit(this);	H->addWidget(axz);	axz->setText("1");
	axz->setToolTip(_("Aspect ratio of x-scale to z-scale"));
	connect(axz,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));
	l = new QLabel(_("Y/Z"),this);	H->addWidget(l);
	ayz = new QLineEdit(this);	H->addWidget(ayz);	ayz->setText("1");
	ayz->setToolTip(_("Aspect ratio of y-scale to z-scale"));
	connect(ayz,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));

	H = new QHBoxLayout;	u->addLayout(H);
	l = new QLabel(_("Reserve at"),this);	H->addWidget(l);
	rl = new QCheckBox(_("left"),this);	H->addWidget(rl);	rl->setChecked(true);
	rl->setToolTip(_("Reserve space for labels at left side (style '<')"));
	rb = new QCheckBox(_("bottom"),this);	H->addWidget(rb);	rb->setChecked(true);
	rb->setToolTip(_("Reserve space for labels at bottom side (style '_')"));
	rt = new QCheckBox(_("top"),this);		H->addWidget(rt);	rt->setChecked(true);
	rt->setToolTip(_("Reserve space for labels at top side (style '^')"));
	rr = new QCheckBox(_("right"),this);	H->addWidget(rr);	rr->setChecked(true);
	rr->setToolTip(_("Reserve space for labels at right side (style '>')"));
	rw = new QCheckBox(_("Whole area"),this);	H->addWidget(rw);
	rw->setToolTip(_("Set to use whole area (style '#')"));
	connect(rl,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	connect(rr,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	connect(rt,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	connect(rb,SIGNAL(toggled(bool)),this,SLOT(updatePic()));
	connect(rw,SIGNAL(toggled(bool)),this,SLOT(updatePic()));

	H = new QHBoxLayout;	u->addLayout(H);
	l = new QLabel(_("Title"),this);	H->addWidget(l);
	title = new QLineEdit(this);		H->addWidget(title);
	title->setToolTip(_("Title for plot. Can be used in SubPlot or MultiPlot only."));
	connect(title,SIGNAL(textChanged(QString)),this,SLOT(updatePic()));
	b = new QPushButton(_("Style"),this);	H->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(titleStl()));

	H = new QHBoxLayout;	u->addLayout(H);
	l = new QLabel(_("Result"),this);	H->addWidget(l);
	res = new QLineEdit(this);		H->addWidget(res);	res->setReadOnly(true);
	res->setToolTip(_("Resulting string"));

	pic = new QLabel(this);	h->addWidget(pic,1);

	h = new QHBoxLayout;	v->addLayout(h);	h->addStretch(1);
	b = new QPushButton(_("Cancel"),this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);	h->addWidget(b);	b->setDefault(true);
	connect(b, SIGNAL(clicked()),this, SLOT(finish()));
	
	stlDialog = new StyleDialog(this);
}
//-----------------------------------------------------------------------------
SubplotDialog::~SubplotDialog()	{	if(grBuf)	delete []grBuf;	}
//-----------------------------------------------------------------------------
void SubplotDialog::finish()	{	accept();	emit result(cmd);	}
//-----------------------------------------------------------------------------
void SubplotDialog::updatePic()
{
	static mglGraph gr;	gr.SetSize(pic->width(),pic->height());
	mglParse par;
	wchar_t *wcmd;

	setlocale(LC_NUMERIC, "C");
	QString stl="'";	// style for subplot
	if(rb->isChecked())	stl+='_';
	if(rl->isChecked())	stl+='<';
	if(rr->isChecked())	stl+='>';
	if(rt->isChecked())	stl+='^';
	if(rw->isChecked())	stl+='#';
	stl += '\'';
	if(stl=="'_<>^'")	stl="";
	int Tet = tet->value(), Phi = phi->value();
	int Ax = 1, Ay = 1;
	if(!axz->text().isEmpty())	Ax = axz->text().toDouble();
	if(!ayz->text().isEmpty())	Ay = ayz->text().toDouble();

	if(cb->isChecked())	// subplot
	{
		int n=bn->value(), m=bm->value(), k=bk->value();
		for(int i=0;i<n*m;i++)	if(i!=k)	{	gr.SubPlot(n,m,i);	gr.Box("h");	}
		cmd = "subplot "+QString::number(n)+" "+QString::number(m)+" "+QString::number(k)+" "+stl;
		if(!title->text().isEmpty())
		{	cmd += ":title '"+title->text()+"'";	if(!fmt.isEmpty())	cmd += fmt;	}
		if(Tet || Phi)	cmd += ":rotate "+QString::number(Tet)+" "+QString::number(Phi);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size()+1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	else if(cm->isChecked())	// multiplot
	{
		int n=mn->value(), m=mm->value(), k=mk->value(), dx=mx->value(), dy=my->value();
		for(int i=0;i<n*m;i++)	if(i!=k)	{	gr.SubPlot(n,m,i);	gr.Box("h");	}
		cmd = "multiplot "+QString::number(n)+" "+QString::number(m)+" "+QString::number(k)+" "+QString::number(dx)+" "+QString::number(dy)+" "+stl;
		if(!title->text().isEmpty())
		{	cmd += ":title '"+title->text()+"'";	if(!fmt.isEmpty())	cmd += fmt;	}
		if(Tet || Phi)	cmd += ":rotate "+QString::number(Tet)+" "+QString::number(Phi);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size() + 1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	else if(cg->isChecked())	// gridplot
	{
		int n=gn->value(), m=gm->value(), k=gk->value();
		double d = gd->text().isEmpty()?0:gd->text().toDouble();
		for(int i=0;i<n*m;i++)	if(i!=k)	{	gr.GridPlot(n,m,i,d);	gr.Box("h");	}
		cmd = "gridplot "+QString::number(n)+" "+QString::number(m)+" "+QString::number(k)+" "+QString::number(d);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size() + 1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	else if(cs->isChecked())	// stickplot
	{
		int n=sn->value(), k=sk->value();
		for(int i=0;i<n;i++)	if(i!=k)	{	gr.StickPlot(n,i,Tet,Phi);	gr.Box("h");	}
		cmd = "stickplot "+QString::number(n)+" "+QString::number(k)+" "+QString::number(Tet)+" "+QString::number(Phi);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size() + 1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	else if(cc->isChecked())	// columnplot	// TODO add angles
	{
		int n=cn->value(), k=ck->value();
		double d = cd->text().isEmpty()?0:cd->text().toDouble();
		for(int i=0;i<n;i++)	if(i!=k)	{	gr.ColumnPlot(n,i,d);	gr.Rotate(Tet,Phi);	gr.Box("h");	}
		cmd = "columnplot "+QString::number(n)+" "+QString::number(k)+" "+QString::number(d);
		if(Tet || Phi)	cmd += ":rotate "+QString::number(Tet)+" "+QString::number(Phi);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size() + 1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	else if(ci->isChecked())	// inplot
	{
		{	gr.SubPlot(1,1,0);	gr.Box("h");	}
		cmd = "inplot "+x1->text()+" "+x2->text()+" "+y1->text()+" "+y2->text();
		if(!title->text().isEmpty())
		{	cmd += ":title '"+title->text()+"'";	if(!fmt.isEmpty())	cmd += fmt;	}
		if(Tet || Phi)	cmd += ":rotate "+QString::number(Tet)+" "+QString::number(Phi);
		if(Ax!=1 || Ay!=1)	cmd += ":aspect "+QString::number(Ax)+" "+QString::number(Ay);
		wcmd = new wchar_t[cmd.size() + 1];
		cmd.toWCharArray(wcmd);
		wcmd[cmd.size()] = 0;
		par.Execute(&gr, wcmd);	gr.Box();
		delete[] wcmd;
		res->setText(cmd);
	}
	setlocale(LC_NUMERIC, "");

	
	QPixmap p;
	convertFromGraph(p, &gr, &grBuf);
	pic->setPixmap(p);
}
//-----------------------------------------------------------------------------
void SubplotDialog::titleStl()
{
	stlDialog->showFontPage();
	if(stlDialog->exec())	{	fmt = " "+stlDialog->getStyle();	updatePic();	}
}
//-----------------------------------------------------------------------------
void SubplotDialog::parseCmd(const QString& txt, bool final)
{

}
//-----------------------------------------------------------------------------
