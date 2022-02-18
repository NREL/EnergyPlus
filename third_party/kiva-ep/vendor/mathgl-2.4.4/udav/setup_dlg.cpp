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
#include <QMessageBox>
#include <QTabWidget>
#include <QComboBox>
#include <QCheckBox>
#include <QLineEdit>
#include <QSpinBox>
#include <QLayout>
#include <QLabel>
#include <QFile>
#include <QTextStream>
//-----------------------------------------------------------------------------
#include "setup_dlg.h"
#include "mgl2/define.h"
#undef sprintf	// fix libintl bug of defining sprintf
void fillColors(QComboBox *cb);
//-----------------------------------------------------------------------------
SetupDialog::SetupDialog(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Setup plot"));
	QWidget *p;
	QGridLayout *g;
	QLabel *l;
	QPushButton *b;
	QTabWidget *tab = new QTabWidget(this);
	// line style
	p = new QWidget(this);
	g = new QGridLayout(p);	g->setAlignment(Qt::AlignTop);
	l = new QLabel(_("X axis"), p);	g->addWidget(l,0,1);
	l = new QLabel(_("Y axis"), p);	g->addWidget(l,0,2);
	l = new QLabel(_("Z axis"), p);	g->addWidget(l,0,3);
	l = new QLabel(_("C axis"), p);	g->addWidget(l,0,4);
	l = new QLabel(_("Minimal"), p);	g->addWidget(l,1,0);
	xmin = new QLineEdit(p);	g->addWidget(xmin,1,1);
	ymin = new QLineEdit(p);	g->addWidget(ymin,1,2);
	zmin = new QLineEdit(p);	g->addWidget(zmin,1,3);
	cmin = new QLineEdit(p);	g->addWidget(cmin,1,4);
	l = new QLabel(_("Maximal"), p);	g->addWidget(l,2,0);
	xmax = new QLineEdit(p);	g->addWidget(xmax,2,1);
	ymax = new QLineEdit(p);	g->addWidget(ymax,2,2);
	zmax = new QLineEdit(p);	g->addWidget(zmax,2,3);
	cmax = new QLineEdit(p);	g->addWidget(cmax,2,4);
	l = new QLabel(_("Origin"), p);	g->addWidget(l,3,0);
	xorg = new QLineEdit(p);	g->addWidget(xorg,3,1);
	yorg = new QLineEdit(p);	g->addWidget(yorg,3,2);
	zorg = new QLineEdit(p);	g->addWidget(zorg,3,3);
	l = new QLabel(_("Label"), p);	g->addWidget(l,4,0);
	xlbl = new QLineEdit(p);	g->addWidget(xlbl,4,1);
	ylbl = new QLineEdit(p);	g->addWidget(ylbl,4,2);
	zlbl = new QLineEdit(p);	g->addWidget(zlbl,4,3);
	l = new QLabel(_("at position"), p);	g->addWidget(l,5,0);
	QStringList lpos;
	lpos.append(_("at minimum"));	lpos += _("at center");	lpos += _("at maximum");
	xpos = new QComboBox(p);	g->addWidget(xpos,5,1);	xpos->addItems(lpos);
	ypos = new QComboBox(p);	g->addWidget(ypos,5,2);	ypos->addItems(lpos);
	zpos = new QComboBox(p);	g->addWidget(zpos,5,3);	zpos->addItems(lpos);
	l = new QLabel(_("Ticks"), p);	g->addWidget(l,6,0);
	xtck = new QLineEdit(p);	g->addWidget(xtck,6,1);
	ytck = new QLineEdit(p);	g->addWidget(ytck,6,2);
	ztck = new QLineEdit(p);	g->addWidget(ztck,6,3);
	l = new QLabel(_("SubTicks"), p);	g->addWidget(l,7,0);
	xsub = new QLineEdit(p);	g->addWidget(xsub,7,1);
	ysub = new QLineEdit(p);	g->addWidget(ysub,7,2);
	zsub = new QLineEdit(p);	g->addWidget(zsub,7,3);
	l = new QLabel(_("Start"), p);	g->addWidget(l,8,0);
	xort = new QLineEdit(p);	g->addWidget(xort,8,1);
	yort = new QLineEdit(p);	g->addWidget(yort,8,2);
	zort = new QLineEdit(p);	g->addWidget(zort,8,3);
	l = new QLabel(_("Template"), p);	g->addWidget(l,9,0);
	xtt = new QLineEdit(p);	g->addWidget(xtt,9,1);
	ytt = new QLineEdit(p);	g->addWidget(ytt,9,2);
	ztt = new QLineEdit(p);	g->addWidget(ztt,9,3);
	ctt = new QLineEdit(p);	g->addWidget(ctt,9,4);
	l = new QLabel(_("AlphaDef"), p);	g->addWidget(l,10,0);
	aldef = new QLineEdit(p);	g->addWidget(aldef,11,0);
	l = new QLabel(_("Ambient"), p);	g->addWidget(l,10,1);
	amb = new QLineEdit(p);	g->addWidget(amb,11,1);
	l = new QLabel(_("BaseWidth"), p);	g->addWidget(l,10,2);
	basew = new QLineEdit(p);	g->addWidget(basew,11,2);
	l = new QLabel(_("MeshNum"), p);	g->addWidget(l,10,3);
	mesh = new QLineEdit(p);	g->addWidget(mesh,11,3);
	l = new QLabel(_("AxialDir"), p);	g->addWidget(l,10,4);
	axial = new QComboBox(p);	g->addWidget(axial,11,4);
	axial->addItem("none");	axial->addItem("x");
	axial->addItem("y");	axial->addItem("z");
	l = new QLabel(_("Font"), p);	g->addWidget(l,12,0);
	font = new QLineEdit(p);	g->addWidget(font,13,0);
	l = new QLabel(_("FontSize"), p);	g->addWidget(l,12,1);
	fsize = new QLineEdit(p);	g->addWidget(fsize,13,1);
	alpha = new QCheckBox(_("Alpha"),p);	g->addWidget(alpha, 13,2);
	light = new QCheckBox(_("Light"),p);	g->addWidget(light, 13,3);
	rotate = new QCheckBox(_("No text rotation"),p);	g->addWidget(rotate, 13,4);
	tab->addTab(p, _("General setup"));

	p = new QWidget(this);
	g = new QGridLayout(p);	g->setAlignment(Qt::AlignTop);
	l = new QLabel(_("ID"), p);	g->addWidget(l,0,0);
	l = new QLabel(_("State"), p);	g->addWidget(l,0,1);
	l = new QLabel(_("X pos"), p);	g->addWidget(l,0,2);
	l = new QLabel(_("Y pos"), p);	g->addWidget(l,0,3);
	l = new QLabel(_("Z pos"), p);	g->addWidget(l,0,4);
	l = new QLabel(_("Color"), p);	g->addWidget(l,0,5);
	l = new QLabel(_("Brightness"), p);	g->addWidget(l,0,6);
	char s[3] = "0:";
	for(int i=0;i<10;i++)
	{
		s[0] = char(i+'0');
		l = new QLabel(s, p);	g->addWidget(l,i+1,0);
		slight[i] = new QCheckBox(_("on"),p);	g->addWidget(slight[i], i+1, 1);
		xlight[i] = new QLineEdit(p);	g->addWidget(xlight[i], i+1, 2);
		ylight[i] = new QLineEdit(p);	g->addWidget(ylight[i], i+1, 3);
		zlight[i] = new QLineEdit(p);	g->addWidget(zlight[i], i+1, 4);
		clight[i] = new QComboBox(p);	g->addWidget(clight[i], i+1, 5);
		fillColors(clight[i]);
		blight[i] = new QLineEdit(p);	g->addWidget(blight[i], i+1, 6);
	}
	tab->addTab(p, _("Light settings"));

	QVBoxLayout *v = new QVBoxLayout(this);	v->addWidget(tab);
	QHBoxLayout *h = new QHBoxLayout();		v->addLayout(h);
	l = new QLabel(_("Template name"), this);		h->addWidget(l);
	fname = new QLineEdit("template.mgl", this);	h->addWidget(fname);
	h->addStretch(1);
	b = new QPushButton(_("Save"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(saveTmpl()));
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("To script"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(toScript()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
SetupDialog::~SetupDialog()	{}
//-----------------------------------------------------------------------------
void SetupDialog::saveTmpl()
{
	if(!convert())	return;
	QFile f(fname->text());
	if(!f.open(QIODevice::WriteOnly))
		QMessageBox::warning(this, _("UDAV - Save template"), _("Could not write to file"), QMessageBox::Ok, 0);
	else
	{
		QTextStream t(&f);	t << res;	f.close();
		QMessageBox::information(this, _("UDAV - Save template"), _("Template saved"), QMessageBox::Ok);
		emit putText(QString("#call %1").arg(fname->text()));
	}

}
//-----------------------------------------------------------------------------
void SetupDialog::toScript()
{
	if(!convert())	return;
	emit putText(res);
	accept();
}
//-----------------------------------------------------------------------------
bool SetupDialog::convert()
{
	// TODO: add error (missing values) parsing
	int i,j;
	double x1=0,y1=0,z1=0,x2=0,y2=0,z2=0;
	bool u1,v1,w1,u2,v2,w2;

	QString s, col="wwbgrcmylenuqphkWBGRCMYLENUQPH";
	res = "";
	for(i=0;i<10;i++)	// set light sources
	{
		if(!slight[i]->isChecked())	continue;
		if(xlight[i]->text().isEmpty() || ylight[i]->text().isEmpty() || zlight[i]->text().isEmpty())
		{
			QMessageBox::information(this, _("UDAV - Setup plot"), _("Light position should be filled. Ignore it."), QMessageBox::Ok);
			continue;
		}
		x1=xlight[i]->text().toDouble();	y1=ylight[i]->text().toDouble();
		z1=zlight[i]->text().toDouble();	j = clight[i]->currentIndex();
		if(blight[i]->text().isEmpty())
			s.sprintf("light %d %g %g %g '%c'\n", i,x1,y1,z1, col[j].toLatin1());
		else
			s.sprintf("light %d %g %g %g '%c' %g\n", i,x1,y1,z1, col[j].toLatin1(), blight[i]->text().toDouble());
		res += s;
	}
	u1 = !xmin->text().isEmpty();	if(u1)	x1 = xmin->text().toDouble();
	u2 = !xmax->text().isEmpty();	if(u2)	x2 = xmax->text().toDouble();
	v1 = !ymin->text().isEmpty();	if(v1)	y1 = ymin->text().toDouble();
	v2 = !ymin->text().isEmpty();	if(v2)	y2 = ymax->text().toDouble();
	w1 = !zmin->text().isEmpty();	if(w1)	z1 = zmin->text().toDouble();
	w2 = !zmin->text().isEmpty();	if(w2)	z2 = zmax->text().toDouble();
	if(u1&&v1&&w1&&u2&&v2&&w2)
	{	s.sprintf("axis %g %g %g %g %g %g\n",x1,y1,z1,x2,y2,z2);	res += s;	}
	else
	{
		if(u1 && u2)	{s.sprintf("xrange %g %g\n",x1,x2);	res += s;}
		if(v1 && v2)	{s.sprintf("yrange %g %g\n",y1,y2);	res += s;}
		if(w1 && w2)	{s.sprintf("zrange %g %g\n",z1,z2);	res += s;}
	}
	u1 = !cmin->text().isEmpty();	if(u1)	x1 = cmin->text().toDouble();
	u2 = !cmax->text().isEmpty();	if(u2)	x2 = cmax->text().toDouble();
	if(u1&&u2)	{s.sprintf("crange %g %g\n",x1,x2);	res += s;}

	u1 = !xmin->text().isEmpty();	if(u1)	x1 = xorg->text().toDouble();
	v1 = !yorg->text().isEmpty();	if(v1)	y1 = yorg->text().toDouble();
	w1 = !zorg->text().isEmpty();	if(w1)	z1 = zorg->text().toDouble();
	if(u1&&v1&&w1)	{s.sprintf("origin %g %g %g\n",x1,y1,z1);	res += s;}

	u1 = !xtck->text().isEmpty();	if(u1)	x1 = xtck->text().toDouble();
	u2 = !xsub->text().isEmpty();	if(u2)	x2 = xsub->text().toDouble();
	v1 = !ytck->text().isEmpty();	if(v1)	y1 = ytck->text().toDouble();
	v2 = !ysub->text().isEmpty();	if(v2)	y2 = ysub->text().toDouble();
	w1 = !ztck->text().isEmpty();	if(w1)	z1 = ztck->text().toDouble();
	w2 = !zsub->text().isEmpty();	if(w2)	z2 = zsub->text().toDouble();
	if(u1 && u2)
	{
		if(xort->text().isEmpty())	s.sprintf("xtick %g %g\n",x1,x2);
		else	s.sprintf("xtick %g %g %g\n",x1,x2,xort->text().toDouble());
		res += s;
	}
	if(v1 && v2)
	{
		if(yort->text().isEmpty())	s.sprintf("ytick %g %g\n",y1,y2);
		else	s.sprintf("ytick %g %g %g\n",y1,y2,yort->text().toDouble());
		res += s;
	}
	if(w1 && w2)
	{
		if(zort->text().isEmpty())	s.sprintf("ztick %g %g\n",z1,z2);
		else	s.sprintf("ztick %g %g %g\n",z1,z2,zort->text().toDouble());
		res += s;
	}
	if(u1 && !u2)	{s.sprintf("xtick %g\n",x1);	res += s;}
	if(v1 && !v2)	{s.sprintf("ytick %g\n",y1);	res += s;}
	if(w1 && !w2)	{s.sprintf("ztick %g\n",z1);	res += s;}
	if(!xtt->text().isEmpty())	res = res + "xtick '" + xtt->text() + "'\n";
	if(!ytt->text().isEmpty())	res = res + "ytick '" + ytt->text() + "'\n";
	if(!ztt->text().isEmpty())	res = res + "ztick '" + ztt->text() + "'\n";
	if(!ctt->text().isEmpty())	res = res + "ctick '" + ctt->text() + "'\n";

	if(!xlbl->text().isEmpty())
	{
		s.sprintf("' %d\n",	xpos->currentIndex()-1);
		res = res + "xlabel '"+ xlbl->text() + s;
	}
	if(!ylbl->text().isEmpty())
	{
		s.sprintf("' %d\n",	ypos->currentIndex()-1);
		res = res + "ylabel '"+ ylbl->text() + s;
	}
	if(!zlbl->text().isEmpty())
	{
		s.sprintf("' %d\n",	zpos->currentIndex()-1);
		res = res + "zlabel '"+ zlbl->text() + s;
	}

	if(!aldef->text().isEmpty())
	{	s.sprintf("alphadef %g\n",aldef->text().toDouble());	res += s;	}
	if(!amb->text().isEmpty())
	{	s.sprintf("ambient %g\n",amb->text().toDouble());		res += s;	}

	if(!basew->text().isEmpty())
	{	s.sprintf("baselinewidth %g\n",basew->text().toDouble());	res += s;	}
	if(!mesh->text().isEmpty())
	{	s.sprintf("meshnum %d\n",mesh->text().toInt());	res += s;	}
	if(axial->currentIndex()>0)
	{
		s.sprintf("axialdir '%c'\n",char('x'+axial->currentIndex()-1));
		res += s;
	}

	if(!font->text().isEmpty())
	{
		res = res + "font '" + font->text();
		if(!fsize->text().isEmpty())	s.sprintf("' %g\n",fsize->text().toDouble());
		else	s.sprintf("'\n");
		res += s;
	}
	if(rotate->isChecked())	res = res + "rotatetext off\n";

	if(alpha->isChecked())	res = res + "alpha on\n";
	if(light->isChecked())	res = res + "light on\n";
	return true;
}
//-----------------------------------------------------------------------------
