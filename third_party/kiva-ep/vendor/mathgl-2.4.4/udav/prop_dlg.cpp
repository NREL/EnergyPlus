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
#include <QApplication>
#include <QPushButton>
#include <QMessageBox>
#include <QCheckBox>
#include <QButtonGroup>
#include <QLayout>
#include <QLabel>
#include <QLineEdit>
#include <QColorDialog>
#include <QFontDialog>
#include <QFileDialog>
#include <QComboBox>
#include <QSettings>
#include <QDir>
//-----------------------------------------------------------------------------
#include <mgl2/qmathgl.h>
#include "prop_dlg.h"
#include "udav_wnd.h"
#include "plot_pnl.h"
#include "text_pnl.h"
//-----------------------------------------------------------------------------
extern QColor mglColorScheme[10];
extern QString defFontFamily;
extern int defFontSize;
extern QString pathHelp;
extern bool mglAutoExecute;
extern bool mglAutoSave;
// extern bool mglAutoPure;
extern bool mglCompleter;
extern bool editPosBottom;
extern bool loadInNewWnd;
extern bool mglHighlight;
extern bool mglDotsRefr;
extern bool mglWheelZoom;
int defWidth, defHeight;
QString pathFont;
QString lang[]={"en","ru","es"};
//-----------------------------------------------------------------------------
PropDialog::PropDialog(QWidget *parent) : QDialog(parent)
{
	setWindowTitle(_("UDAV - Properties"));
	QHBoxLayout *h;
	QVBoxLayout *v;
	QLabel *l;
	QPushButton *b;

	v = new QVBoxLayout(this);
	h = new QHBoxLayout();		v->addLayout(h);
	lbl = new QLabel(_("Current font"), this);	h->addWidget(lbl);
	lbl->setFont(QFont(defFontFamily, defFontSize));
	h->addStretch(1);	defFont = QFont(defFontFamily, defFontSize);
	b = new QPushButton(_("Change font"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(setF()));

	cc[0]=mglColorScheme[0];	cc[1]=mglColorScheme[1];
	cc[2]=mglColorScheme[2];	cc[3]=mglColorScheme[3];
	cc[4]=mglColorScheme[4];	cc[5]=mglColorScheme[5];
	cc[6]=mglColorScheme[6];	cc[7]=mglColorScheme[7];
	cc[8]=mglColorScheme[8];	cc[9]=mglColorScheme[9];
	QPixmap pic(16,16);
	l = new QLabel(_("Setup colors for:"), this);	v->addWidget(l, Qt::AlignHCenter);
	QGridLayout *g = new QGridLayout();		v->addLayout(g);
	pic.fill(cc[0]);	cb[0] = new QPushButton(pic, _("Comments"), this);
	connect(cb[0], SIGNAL(clicked()),this, SLOT(setC0()));
	g->addWidget(cb[0], 0, 0);
	pic.fill(cc[1]);	cb[1] = new QPushButton(pic, _("Strings"), this);
	connect(cb[1], SIGNAL(clicked()),this, SLOT(setC1()));
	g->addWidget(cb[1], 0, 1);
	pic.fill(cc[2]);	cb[2] = new QPushButton(pic, _("Keywords"), this);
	connect(cb[2], SIGNAL(clicked()),this, SLOT(setC2()));
	g->addWidget(cb[2], 0, 2);
	pic.fill(cc[3]);	cb[3] = new QPushButton(pic, _("Options"), this);
	connect(cb[3], SIGNAL(clicked()),this, SLOT(setC3()));
	g->addWidget(cb[3], 1, 0);
	pic.fill(cc[4]);	cb[4] = new QPushButton(pic, _("Suffixes"), this);
	connect(cb[4], SIGNAL(clicked()),this, SLOT(setC4()));
	g->addWidget(cb[4], 1, 1);
	pic.fill(cc[5]);	cb[5] = new QPushButton(pic, _("Numbers"), this);
	connect(cb[5], SIGNAL(clicked()),this, SLOT(setC5()));
	g->addWidget(cb[5], 1, 2);
	pic.fill(cc[6]);	cb[6] = new QPushButton(pic, _("AutoKey"), this);
	connect(cb[6], SIGNAL(clicked()),this, SLOT(setC6()));
	g->addWidget(cb[6], 2, 0);
	pic.fill(cc[7]);	cb[7] = new QPushButton(pic, _("FlowKey"), this);
	connect(cb[7], SIGNAL(clicked()),this, SLOT(setC7()));
	g->addWidget(cb[7], 2, 1);
	pic.fill(cc[9]);	cb[9] = new QPushButton(pic, _("CurrLine"), this);
	connect(cb[9], SIGNAL(clicked()),this, SLOT(setC9()));
	g->addWidget(cb[9], 2, 2);

	l = new QLabel(_("Path for help files"), this);	v->addWidget(l);
	h = new QHBoxLayout();		v->addLayout(h);
	hlp = new QLineEdit(pathHelp, this);	h->addWidget(hlp,1);
	b = new QPushButton("...", this);		h->addWidget(b,0);
	connect(b,SIGNAL(clicked()),this,SLOT(getPathH()));

	QStringList paths, files, filter;
	paths << "." << QDir::homePath() << QDir::rootPath()
		<< QCoreApplication::applicationDirPath()+"/fonts";
	filter << "*.vfm";
#ifndef WIN32
	paths << "/usr/local/share/mathgl/fonts/" << "/usr/local/mathgl/fonts/";
#endif
	QFileInfoList info;
	for(int i=0;i<paths.count();i++)
		info << QDir(paths.at(i)).entryInfoList(filter);
	for(int i=0;i<info.count();i++)
		if(!info.at(i).fileName().contains('_'))
			files << info.at(i).absoluteFilePath();

	l = new QLabel(_("Path for user MathGL font files"), this);	v->addWidget(l);
	h = new QHBoxLayout();		v->addLayout(h);
	fnt = new QComboBox(this);	h->addWidget(fnt,1);
	fnt->addItems(files);	fnt->setEditable(true);
	fnt->lineEdit()->setText(pathFont);
	b = new QPushButton("...", this);		h->addWidget(b,0);
	connect(b,SIGNAL(clicked()),this,SLOT(getPathF()));

	h = new QHBoxLayout();		v->addLayout(h);
	l = new QLabel(_("Language for UDAV"), this);	h->addWidget(l,0);
	lng = new QComboBox(this);		h->addWidget(lng,1);
	lng->addItem(_("English"));	lng->addItem(_("Russian"));	lng->addItem(_("Spanish"));

	QSettings settings("udav","UDAV");
	settings.setPath(QSettings::IniFormat, QSettings::UserScope, "UDAV");
	settings.beginGroup("/UDAV");
	QString prev = settings.value("/udavLang", "").toString();
	if(prev==lang[1])	lng->setCurrentIndex(1);
	if(prev==lang[2])	lng->setCurrentIndex(2);
	defWidth = settings.value("/defWidth", 640).toInt();
	defHeight = settings.value("/defHeight", 480).toInt();
	settings.endGroup();

	h = new QHBoxLayout();		v->addLayout(h);
	l = new QLabel(_("Image size"), this);	h->addWidget(l,0);
	defW = new QLineEdit(QString::number(defWidth),this);	h->addWidget(defW,1);
	l = new QLabel("x", this);	h->addWidget(l,0);
	defH = new QLineEdit(QString::number(defHeight),this);	h->addWidget(defH,1);

	run = new QCheckBox(_("Automatically execute script after loading"), this);
	run->setChecked(mglAutoExecute);	v->addWidget(run);
	edt = new QCheckBox(_("Place editor at top"), this);
	edt->setChecked(editPosBottom);	v->addWidget(edt);
	load = new QCheckBox(_("Load script to new window"), this);
	load->setChecked(loadInNewWnd);	v->addWidget(load);
	save = new QCheckBox(_("Automatically save before redrawing (F5)"), this);
	save->setChecked(mglAutoSave);	v->addWidget(save);
//	pure = new QCheckBox(_("Disable face drawing (faster) for mouse rotation/shift/zoom."), this);
//	pure->setChecked(mglAutoPure);	v->addWidget(pure);	pure->setEnabled(false);
	wheel = new QCheckBox(_("Enable mouse wheel for zooming"), this);
	wheel->setChecked(mglWheelZoom);	v->addWidget(wheel);
	cmpl = new QCheckBox(_("Enable keywords completion"), this);
	cmpl->setChecked(mglCompleter);	v->addWidget(cmpl);
	high = new QCheckBox(_("Highlight current object(s)"), this);
	high->setChecked(mglHighlight);	v->addWidget(high);
	dots = new QCheckBox(_("Use dots plot for preview"), this);
	dots->setChecked(mglDotsRefr);	v->addWidget(dots);

	h = new QHBoxLayout();		v->addLayout(h);
	h->addStretch(1);
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(applyChanges()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
PropDialog::~PropDialog()	{}
//-----------------------------------------------------------------------------
void PropDialog::getPathH()
{
	QString str = QFileDialog::getExistingDirectory(this,
				_("UDAV - Insert filename"), hlp->text());
	if(!str.isEmpty())	hlp->setText(str+'/');
}
//-----------------------------------------------------------------------------
void PropDialog::getPathF()
{
	QString str = QFileDialog::getOpenFileName(this, _("UDAV - Insert filename"),
					fnt->lineEdit()->text(), _("Font files (*.vfm)"));
	if(!str.isEmpty())
	{	str = str.mid(0,str.lastIndexOf(".vfm"));	fnt->lineEdit()->setText(str);	}
}
//-----------------------------------------------------------------------------
void PropDialog::setC(int k)
{
	if(k<0 || k>9)	return;
	QColor c = QColorDialog::getColor(cc[k], this);
	if(c.isValid())
	{
		QPixmap p(16,16);	p.fill(c);
		cb[k]->setIcon(p);	cc[k] = c;
	}
}
//-----------------------------------------------------------------------------
void PropDialog::setF()
{
	bool ok;
	QFont f = QFontDialog::getFont(&ok, defFont, this);
	if(ok)	{	defFont = f;	lbl->setFont(f);	}
}
//-----------------------------------------------------------------------------
void PropDialog::applyChanges()
{
	// defFont editPosBottom pathFont
	mglColorScheme[0]=cc[0];	mglColorScheme[1]=cc[1];
	mglColorScheme[2]=cc[2];	mglColorScheme[3]=cc[3];
	mglColorScheme[4]=cc[4];	mglColorScheme[5]=cc[5];
	mglColorScheme[6]=cc[6];	mglColorScheme[7]=cc[7];
	mglColorScheme[8]=cc[8];	mglColorScheme[9]=cc[9];
	mglAutoExecute = run->isChecked();
	editPosBottom = edt->isChecked();
	pathHelp = hlp->text();	pathFont = fnt->lineEdit()->text();
	if(pathHelp.isEmpty())	pathHelp=MGL_DOC_DIR;
	defFontFamily = defFont.family();
	defFontSize = defFont.pointSize();
	loadInNewWnd = load->isChecked();
	mglAutoSave = save->isChecked();
	mglHighlight = high->isChecked();
// 	mglAutoPure = pure->isChecked();
	mglCompleter = cmpl->isChecked();
	mglDotsRefr = dots->isChecked();
	mglWheelZoom = wheel->isChecked();

	// apply changes for all windows
#ifdef WIN32
	pathFont = pathFont.replace('/','\\');
#endif
	bool ok=true;
	foreach(QWidget *w, QApplication::topLevelWidgets())
	{
		if(w->inherits("MainWindow"))
		{
			MainWindow *s = (MainWindow *)w;
			if(ok)	{	s->writeSettings();	ok = false;	}
			s->edit->setEditorFont();
			s->graph->setMGLFont(pathFont);
			s->graph->mgl->enableWheel = mglWheelZoom;
			s->setEditPos(editPosBottom);
			s->edit->setCompleter(mglCompleter);
			s->update();
		}
	}

	if(defWidth!=defW->text().toInt() || defHeight!=defH->text().toInt())
	{
		defWidth = defW->text().toInt();
		defHeight = defH->text().toInt();
		sizeChanged(defWidth, defHeight);
	}

	QSettings settings("udav","UDAV");
	settings.setPath(QSettings::IniFormat, QSettings::UserScope, "UDAV");
	settings.beginGroup("/UDAV");
	QString prev = settings.value("/udavLang", "").toString();
	int cur=lng->currentIndex();
	if(cur>=0 && prev!=lang[cur])
	{
		settings.setValue("/udavLang", lang[lng->currentIndex()]);
#if WIN32
		const char *loc[]={"C.UTF8",	"ru_RU.cp1251",	"es_ES.utf8",	""};
#else
		const char *loc[]={"C.UTF8",	"ru_RU.utf8",	"es_ES.utf8",	""};
#endif
		mgl_textdomain(NULL,loc[cur]);
		QMessageBox::critical(this,_("UDAV - Properties"),_("You need to restart UDAV for applying the changes."));
	}
	settings.setValue("/defWidth", defWidth);
	settings.setValue("/defHeight", defHeight);
	settings.endGroup();

	accept();	emit propUpdated();
}
//-----------------------------------------------------------------------------
