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
#include <QSettings>
#include <QLineEdit>
#include <QComboBox>
#include <QPushButton>
#include <QFileDialog>
#include <QTextStream>
#include <QRadioButton>
#include <mgl2/mgl.h>
#include "open_dlg.h"
int numDataOpened=0;
extern mglParse parser;
QStringList dataScr;
//-----------------------------------------------------------------------------
QWidget *createDataOpenDlg(QWidget *p)	{	return new DataOpenDialog(p);	}
QString getOpenDataFile(QWidget *w, QString filename)
{
	DataOpenDialog *d = dynamic_cast<DataOpenDialog *>(w);
	if(d)
	{
		d->setFile(filename);
		if(d->exec())	return d->getCode();
	}
	return QString();
}
//-----------------------------------------------------------------------------
DataOpenDialog::DataOpenDialog(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Open data file"));
	QHBoxLayout *a;
	QLabel *l;
	QPushButton *b;
	QVBoxLayout *o=new QVBoxLayout(this);

	a = new QHBoxLayout;	o->addLayout(a);
	l = new QLabel(_("Data name"));	a->addWidget(l);
	char buf[32];	snprintf(buf,32,"mgl_%d",numDataOpened);	buf[31]=0;
	name = new QLineEdit(buf,this);		a->addWidget(name);

	rA = new QRadioButton(_("Auto detect data sizes"), this);
	rA->setChecked(true);	o->addWidget(rA);
	rM = new QRadioButton(_("Set data sizes manually"), this);
	o->addWidget(rM);	a = new QHBoxLayout;	o->addLayout(a);
	l = new QLabel(_("Nx"));	a->addWidget(l);
	nx = new QLineEdit("1",this);	a->addWidget(nx);
	l = new QLabel(_("Ny"));	a->addWidget(l);
	ny = new QLineEdit("1",this);	a->addWidget(ny);
	l = new QLabel(_("Nz"));	a->addWidget(l);
	nz = new QLineEdit("1",this);	a->addWidget(nz);
	r2 = new QRadioButton(_("Matrix with sizes from file"), this);	o->addWidget(r2);
	r3 = new QRadioButton(_("3D data with sizes from file"), this);o->addWidget(r3);


	QSettings settings("udav","UDAV");
	settings.setPath(QSettings::IniFormat, QSettings::UserScope, "UDAV");
	settings.beginGroup("/UDAV");
	dataScr = settings.value("/dataScr").toStringList().mid(0,10);
	dataScr.removeDuplicates();
	settings.endGroup();

	a = new QHBoxLayout;		o->addLayout(a);
	l = new QLabel(_("Template"));	a->addWidget(l,0);
	scr = new QComboBox(this);		a->addWidget(scr,1);
	scr->setEditable(true);			scr->lineEdit()->setText("");
	scr->addItem(_("default"));	scr->addItems(dataScr);
	b = new QPushButton("...", this);	a->addWidget(b,0);
	connect(b, SIGNAL(clicked()),this, SLOT(selectScr()));

	a = new QHBoxLayout;	o->addLayout(a);	a->addStretch(1);
	b = new QPushButton(_("Cancel"),this);	a->addWidget(b);
	connect(b,SIGNAL(clicked()),this,SLOT(reject()));
	b = new QPushButton(_("OK"), this);	a->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(prepareResult()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
DataOpenDialog::~DataOpenDialog(){}
//-----------------------------------------------------------------------------
void DataOpenDialog::selectScr()
{
	QString str = QFileDialog::getOpenFileName(this, _("UDAV - Insert filename"),
					scr->lineEdit()->text(), _("MGL files (*.mgl)"));
	if(!str.isEmpty())
	{
		scr->lineEdit()->setText(str);
		scr->insertItem(1,str);
		dataScr.insert(0,str);
		dataScr.removeDuplicates();
	}
}

//-----------------------------------------------------------------------------
void DataOpenDialog::prepareResult()
{
	code = "";	numDataOpened++;	data = name->text();
	// prepare unique value of name for next time
	char buf[32];	snprintf(buf,32,"mgl_%d",numDataOpened);
	buf[31]=0;	name->setText(buf);
	mglData *v = dynamic_cast<mglData*>(parser.AddVar(data.toLocal8Bit().constData()));
	if(!v)	return;
	int dd=0;
	if(rA->isChecked())	//	auto sizes
	{
		v->Read(file.toLocal8Bit().constData());
		if(v->nx==1)	{	v->nx = v->ny;	v->ny = v->nz;	}
		code=QString("#read %1 '%2'\n").arg(data).arg(file);
	}
	else if(rM->isChecked())	//	manual sizes
	{
		int x=nx->text().toInt(), y=ny->text().toInt(), z=nz->text().toInt();
		v->Read(file.toLocal8Bit().constData(),x,y,z);
		code=QString("#read %1 '%2' %3 %4 %5\n").arg(data).arg(file).arg(x).arg(y).arg(z);
	}
	else if(r2->isChecked())	//	matrix
	{
		v->ReadMat(file.toLocal8Bit().constData());
		code=QString("#readmat %1 '%2'\n").arg(data).arg(file);		dd=1;
	}
	else if(r3->isChecked())	//	3d-data
	{
		v->ReadMat(file.toLocal8Bit().constData(),3);
		code=QString("#readmat %1 '%2' 3\n").arg(data).arg(file);	dd=2;
	}
	if(scr->lineEdit()->text().isEmpty() || scr->lineEdit()->text()==_("default"))
	{
		if(v->nz>1 || dd==2)
			code+=QString("rotate 40 60\ncrange %1:box\nsurf3 %1\n").arg(data);
		else if(v->ny>1 || dd==1)
			code+=QString("rotate 40 60\ncrange %1:zrange %1:box\nsurf %1\n").arg(data);
		else	code+=QString("yrange %1:box\nplot %1\n").arg(data);
	}
	else
	{
		QString str;
		QFile fp(scr->lineEdit()->text());
		if(fp.open(QFile::ReadOnly | QIODevice::Text))
		{
			QTextStream in(&fp);
			str = in.readAll();
			code += str.arg(data);
		}
	}

	QSettings settings("udav","UDAV");
	settings.setPath(QSettings::IniFormat, QSettings::UserScope, "UDAV");
	settings.beginGroup("/UDAV");
	settings.setValue("/dataScr", dataScr);
	settings.endGroup();

	accept();
}
//-----------------------------------------------------------------------------
void DataOpenDialog::setFile(const QString &fname)
{
	file=fname;
	mglData d(file.toLocal8Bit().constData());
	rA->setText(QString(_("Auto detect data sizes (%1 x %2 x %3)")).arg(d.nx).arg(d.ny).arg(d.nz));
}
//-----------------------------------------------------------------------------
