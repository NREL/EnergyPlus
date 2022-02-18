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
#include <QComboBox>
#include <QCheckBox>
#include <QRadioButton>
#include <QGroupBox>
#include <QTabWidget>
#include <QSlider>
#include <QSpinBox>
#include <QLayout>
#include <QLabel>
#include <QLineEdit>
#include <QToolButton>
#include <mgl2/mgl.h>
//-----------------------------------------------------------------------------
#include "style_dlg.h"
#include "mgl2/define.h"
void fillColors(QComboBox *cb);
void fillArrows(QComboBox *cb);
void fillBArrows(QComboBox *cb);
void fillDashes(QComboBox *cb);
void fillMarkers(QComboBox *cb);
void fillMasks(QComboBox *cb);
void convertFromGraph(QPixmap &pic, mglGraph *gr, uchar **buf);
//-----------------------------------------------------------------------------
StyleDialog::StyleDialog(QWidget *parent) : QDialog(parent)
{
	grBuf = 0;
	setWindowTitle(_("UDAV - Insert style/scheme"));
	QWidget *p;
	QHBoxLayout *h;
	QVBoxLayout *v, *u, *vv;
	QGridLayout *g;
	QLabel *l;
	QPushButton *b;
	tab = new QTabWidget(this);

	// line style
	p = new QWidget(this);	v = new QVBoxLayout(p);
	g = new QGridLayout;	g->setAlignment(Qt::AlignTop);	v->addLayout(g);
//	g->setColStretch(0, 1);	g->setColStretch(1, 1);	g->setColStretch(2, 1);
	l = new QLabel(_("Arrow at start"), p);	g->addWidget(l, 0, 0);
	l = new QLabel(_("Dashing"), p);		g->addWidget(l, 0, 1);
	l = new QLabel(_("Arrow at end"), p);	g->addWidget(l, 0, 2);
	a1 = new QComboBox(p);	g->addWidget(a1, 1, 0);	fillArrows(a1);
	dash = new QComboBox(p);	g->addWidget(dash, 1, 1);	fillDashes(dash);
	a2 = new QComboBox(p);	g->addWidget(a2, 1, 2);	fillBArrows(a2);
	l = new QLabel(_("Color"), p);	g->addWidget(l, 2, 0, Qt::AlignRight);
	cline=new QComboBox(p);	g->addWidget(cline, 2, 1);	fillColors(cline);

	nline = new QSlider(p);		g->addWidget(nline, 2, 2);
	nline->setRange(1, 9);		nline->setValue(5);
	nline->setTickPosition(QSlider::TicksBothSides);
	nline->setTickInterval(1);	nline->setPageStep(2);
	nline->setOrientation(Qt::Horizontal);

	l = new QLabel(_("Marks"), p);	g->addWidget(l, 3, 0, Qt::AlignRight);
	mark = new QComboBox(p);	g->addWidget(mark, 3, 1);	fillMarkers(mark);
	l = new QLabel(_("Line width"), p);	g->addWidget(l, 4, 0, Qt::AlignRight);
	width = new QSpinBox(p);	g->addWidget(width, 4, 1);
	width->setRange(1,9);	width->setValue(1);

	v->addStretch(1);
	l = new QLabel(_("Manual dashing"), p);	v->addWidget(l);
	h = new QHBoxLayout;	v->addLayout(h);	h->setSpacing(1);
	for(int i=0;i<16;i++)
	{
		dash_bit[i] = new QToolButton(this);
		dash_bit[i]->setCheckable(true);
		h->addWidget(dash_bit[i]);
		connect(dash_bit[i],SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	}
	connect(a1,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(a2,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(dash,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(mark,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(cline,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(nline,SIGNAL(valueChanged(int)), this, SLOT(updatePic()));
	connect(width,SIGNAL(valueChanged(int)), this, SLOT(updatePic()));
	tab->addTab(p, _("Line style"));

	// color scheme
	p = new QWidget(this);
	v = new QVBoxLayout(p);	v->setAlignment(Qt::AlignTop);
	g = new QGridLayout();	v->addLayout(g);
//	g->setColStretch(0, 1);			g->setColStretch(1, 1);
	l = new QLabel(_("Color order"), p);	g->addWidget(l, 0, 0);
	l = new QLabel(_("Saturation"),p);		g->addWidget(l, 0, 1);
	for(int i=0;i<7;i++)
	{
		cc[i] = new QComboBox(p);	g->addWidget(cc[i], i+1, 0);
		fillColors(cc[i]);
		nn[i] = new QSlider(p);		g->addWidget(nn[i], i+1, 1);
		nn[i]->setRange(1, 9);		nn[i]->setValue(5);
		nn[i]->setTickPosition(QSlider::TicksBothSides);
		nn[i]->setTickInterval(1);	nn[i]->setPageStep(2);
		nn[i]->setOrientation(Qt::Horizontal);
		connect(cc[i],SIGNAL(activated(int)), this, SLOT(updatePic()));
		connect(nn[i],SIGNAL(valueChanged(int)), this, SLOT(updatePic()));
	}
	swire = new QCheckBox(_("Wire or mesh plot"),p);	v->addWidget(swire);
	g = new QGridLayout();	v->addLayout(g);
	l = new QLabel(_("Axial direction"), p);	g->addWidget(l, 0, 0, Qt::AlignRight);
	l = new QLabel(_("Text on contours"), p);	g->addWidget(l, 1, 0, Qt::AlignRight);
	l = new QLabel(_("Mask for bitmap coloring"), p);	g->addWidget(l, 2, 0, Qt::AlignRight);
	l = new QLabel(_("Mask rotation angle"), p);	g->addWidget(l, 3, 0, Qt::AlignRight);
	l = new QLabel(_("Mask size"), p);	g->addWidget(l, 4, 0, Qt::AlignRight);
	axial = new QComboBox(p);	g->addWidget(axial, 0, 1);
	axial->addItem(_("none"));	axial->addItem("x");
	axial->addItem("y");	axial->addItem("z");
	ctext = new QComboBox(p);	g->addWidget(ctext, 1, 1);
	ctext->addItem(_("none"));	ctext->addItem(_("under"));	ctext->addItem(_("above"));
	mask = new QComboBox(p);	g->addWidget(mask, 2, 1);	fillMasks(mask);
	angle = new QComboBox(p);	g->addWidget(angle, 3, 1);
	angle->addItem(_("none"));
	angle->addItem(QString::fromWCharArray(L"+45\xb0"));
	angle->addItem(QString::fromWCharArray(L"-45\xb0"));
	angle->addItem(QString::fromWCharArray(L"90\xb0"));	// \xb0 <-> °
	msize = new QSlider(p);		g->addWidget(msize, 4, 1);
	msize->setRange(1, 9);		msize->setValue(1);
	msize->setTickPosition(QSlider::TicksBothSides);
	msize->setTickInterval(1);	msize->setPageStep(2);
	msize->setOrientation(Qt::Horizontal);

	connect(axial,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(ctext,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(swire,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(mask,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(angle,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(msize,SIGNAL(valueChanged(int)), this, SLOT(updatePic()));
	tab->addTab(p, _("Color scheme"));

	// font style
	p = new QWidget(this);
	v = new QVBoxLayout(p);	v->setAlignment(Qt::AlignTop);
	h = new QHBoxLayout();	v->addLayout(h);
	u = new QVBoxLayout();	h->addLayout(u);
	bold = new QCheckBox(_("Bold style"), p);	u->addWidget(bold);
	ital = new QCheckBox(_("Italic style"), p);u->addWidget(ital);
	wire = new QCheckBox(_("Wire style"), p);	u->addWidget(wire);
	uline = new QCheckBox(_("Underline"), p);	u->addWidget(uline);
	oline = new QCheckBox(_("Overline"), p);	u->addWidget(oline);
	font_sch = new QCheckBox(_("Use color scheme"), p);	u->addWidget(font_sch);
	u = new QVBoxLayout();	h->addLayout(u);
	l = new QLabel(_("Text color"), p);		u->addWidget(l);
	cfont = new QComboBox(p);	fillColors(cfont);	u->addWidget(cfont);
	u->addSpacing(6);
	align = new QGroupBox(_("Text align"), p);	u->addWidget(align);
	vv = new QVBoxLayout(align);		//vv->addSpacing(11);
	rbL = new QRadioButton(_("left"), align);	vv->addWidget(rbL);
	rbC = new QRadioButton(_("at center"), align);
	vv->addWidget(rbC);	rbC->setChecked(true);
	rbR = new QRadioButton(_("right"), align);	vv->addWidget(rbR);
	connect(bold,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(ital,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(wire,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(uline,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(oline,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(font_sch,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(cfont,SIGNAL(activated(int)), this, SLOT(updatePic()));
	connect(rbL,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(rbC,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	connect(rbR,SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	tab->addTab(p, _("Font style"));
	connect(tab,SIGNAL(currentChanged(int)), this, SLOT(updatePic()));

	// hex-mask
	p = new QWidget(this);
	g = new QGridLayout(p);	g->setAlignment(Qt::AlignTop);
	for(int i=0;i<64;i++)
	{
		mask_bit[i] = new QToolButton(this);
		mask_bit[i]->setCheckable(true);
		g->addWidget(mask_bit[i],7-i/8,i%8);
		connect(mask_bit[i],SIGNAL(toggled(bool)), this, SLOT(updatePic()));
	}
	tab->addTab(p, _("Manual mask"));

	// dialog itself
	v = new QVBoxLayout(this);	v->addWidget(tab);
	h = new QHBoxLayout();		v->addLayout(h);
	l = new QLabel(_("Resulting string"), this);	h->addWidget(l);	h->addStretch(1);
	pic = new QLabel(this);	pic->setMinimumSize(QSize(128,30));	h->addWidget(pic);
	res = new QLineEdit(this);	res->setReadOnly(true);	v->addWidget(res);

	h = new QHBoxLayout();	v->addLayout(h);	h->addStretch(1);
	b = new QPushButton(_("Cancel"), this);	h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(reject()));
	b = new QPushButton(_("OK"), this);		h->addWidget(b);
	connect(b, SIGNAL(clicked()),this, SLOT(accept()));
	b->setDefault(true);
}
//-----------------------------------------------------------------------------
StyleDialog::~StyleDialog()	{	if(grBuf)	delete []grBuf;	grBuf = 0;	}
//-----------------------------------------------------------------------------
#include "xpm/none.xpm"
void fillColors(QComboBox *cb)
{
//	string id : 	"wbgrcmylenuqphkWBGRCMYLENUQPH"
	QPixmap pic(16,16);
	cb->addItem(QPixmap(none_xpm), _("none or default"));
	pic.fill(QColor(255,255,255));	cb->addItem(pic, _("w - white"));
	pic.fill(QColor(0,0,255));		cb->addItem(pic, _("b - blue"));
	pic.fill(QColor(0,255,0));		cb->addItem(pic, _("g - lime"));
	pic.fill(QColor(255,0,0));		cb->addItem(pic, _("r - red"));
	pic.fill(QColor(0,255,255));	cb->addItem(pic, _("c - cyan"));
	pic.fill(QColor(255,0,255));	cb->addItem(pic, _("m - magenta"));
	pic.fill(QColor(255,255,0));	cb->addItem(pic, _("y - yellow"));
	pic.fill(QColor(0,255,127));	cb->addItem(pic, _("l - springgreen"));
	pic.fill(QColor(127,255,0));	cb->addItem(pic, _("e - lawngreen"));
	pic.fill(QColor(0,127,255));	cb->addItem(pic, _("n - skyblue"));
	pic.fill(QColor(127,0,255));	cb->addItem(pic, _("u - blueviolet"));
	pic.fill(QColor(255,127,0));	cb->addItem(pic, _("q - orange"));
	pic.fill(QColor(255,0,127));	cb->addItem(pic, _("p - deeppink"));
	pic.fill(QColor(127,127,127));	cb->addItem(pic, _("h - gray"));
	pic.fill(QColor(0,0,0));		cb->addItem(pic, _("k - black"));
	pic.fill(QColor(179,179,179));	cb->addItem(pic, _("W - lightgray"));
	pic.fill(QColor(0,0,127));		cb->addItem(pic, _("B - navy"));
	pic.fill(QColor(0,127,0));		cb->addItem(pic, _("G - green"));
	pic.fill(QColor(127,0,0));		cb->addItem(pic, _("R - maroon"));
	pic.fill(QColor(0,127,127));	cb->addItem(pic, _("C - teal"));
	pic.fill(QColor(127,0,127));	cb->addItem(pic, _("M - purple"));
	pic.fill(QColor(127,127,0));	cb->addItem(pic, _("Y - olive"));
	pic.fill(QColor(0,127,77));		cb->addItem(pic, _("L - seagreen"));
	pic.fill(QColor(77,127,0));		cb->addItem(pic, _("E - darklawn"));
	pic.fill(QColor(0,77,127));		cb->addItem(pic, _("N - darkskyblue"));
	pic.fill(QColor(77,0,127));		cb->addItem(pic, _("U - indigo"));
	pic.fill(QColor(127,77,0));		cb->addItem(pic, _("Q - brown"));
	pic.fill(QColor(127,0,77));		cb->addItem(pic, _("P - darkpink"));
	pic.fill(QColor(77,77,77));		cb->addItem(pic, _("H - darkgray"));
}
//-----------------------------------------------------------------------------
#include "xpm/arrow_n.xpm"
#include "xpm/arrow_a.xpm"
#include "xpm/arrow_v.xpm"
#include "xpm/arrow_i.xpm"
#include "xpm/arrow_k.xpm"
#include "xpm/arrow_t.xpm"
#include "xpm/arrow_s.xpm"
#include "xpm/arrow_d.xpm"
#include "xpm/arrow_o.xpm"
void fillArrows(QComboBox *cb)
{
	// "AVIKTSDO"
	cb->addItem(QPixmap(arrow_n_xpm), _("'_' none"));
	cb->addItem(QPixmap(arrow_a_xpm), _("'A' arrow"));
	cb->addItem(QPixmap(arrow_v_xpm), _("'V' back arrow"));
	cb->addItem(QPixmap(arrow_i_xpm), _("'I' stop"));
	cb->addItem(QPixmap(arrow_k_xpm), _("'K' size"));
	cb->addItem(QPixmap(arrow_t_xpm), _("'T' triangle"));
	cb->addItem(QPixmap(arrow_s_xpm), _("'S' square"));
	cb->addItem(QPixmap(arrow_d_xpm), _("'D' rhomb"));
	cb->addItem(QPixmap(arrow_o_xpm), _("'O' circle"));
}
//-----------------------------------------------------------------------------
#include "xpm/barrow_n.xpm"
#include "xpm/barrow_a.xpm"
#include "xpm/barrow_v.xpm"
#include "xpm/barrow_i.xpm"
#include "xpm/barrow_k.xpm"
#include "xpm/barrow_t.xpm"
#include "xpm/barrow_s.xpm"
#include "xpm/barrow_d.xpm"
#include "xpm/barrow_o.xpm"
void fillBArrows(QComboBox *cb)
{
	// "AVIKTSDO"
	cb->addItem(QPixmap(barrow_n_xpm), _("'_' none"));
	cb->addItem(QPixmap(barrow_a_xpm), _("'A' arrow"));
	cb->addItem(QPixmap(barrow_v_xpm), _("'V' back arrow"));
	cb->addItem(QPixmap(barrow_i_xpm), _("'I' stop"));
	cb->addItem(QPixmap(barrow_k_xpm), _("'K' size"));
	cb->addItem(QPixmap(barrow_t_xpm), _("'T' triangle"));
	cb->addItem(QPixmap(barrow_s_xpm), _("'S' square"));
	cb->addItem(QPixmap(barrow_d_xpm), _("'D' rhomb"));
	cb->addItem(QPixmap(barrow_o_xpm), _("'O' circle"));
}
//-----------------------------------------------------------------------------
#include "xpm/dash_e.xpm"
#include "xpm/dash_s.xpm"
#include "xpm/dash_l.xpm"
#include "xpm/dash_m.xpm"
#include "xpm/dash_d.xpm"
#include "xpm/dash_i.xpm"
#include "xpm/dash_j.xpm"
#include "xpm/mark_n.xpm"
void fillDashes(QComboBox *cb)
{
	// "-|;=ji: "
	cb->addItem(QPixmap(dash_s_xpm), _("'-' solid"));
	cb->addItem(QPixmap(dash_l_xpm), _("'|' long dash"));
	cb->addItem(QPixmap(dash_m_xpm), _("';' dash"));
	cb->addItem(QPixmap(dash_e_xpm), _("'=' small dash"));
	cb->addItem(QPixmap(dash_j_xpm), _("'j' dash dot"));
	cb->addItem(QPixmap(dash_i_xpm), _("'i' small dash dot"));
	cb->addItem(QPixmap(dash_d_xpm), _("':' dots"));
	cb->addItem(QPixmap(mark_n_xpm), _("' ' none"));
	cb->addItem(QPixmap(":/png/tools-wizard.png"), _("manual"));
}
//-----------------------------------------------------------------------------
#include "xpm/mask_a.xpm"
#include "xpm/mask_d.xpm"
#include "xpm/mask_d_.xpm"
#include "xpm/mask_e.xpm"
#include "xpm/mask_i.xpm"
#include "xpm/mask_j.xpm"
#include "xpm/mask_l.xpm"
#include "xpm/mask_m.xpm"
#include "xpm/mask_o.xpm"
#include "xpm/mask_o_.xpm"
#include "xpm/mask_p.xpm"
#include "xpm/mask_r.xpm"
#include "xpm/mask_s.xpm"
#include "xpm/mask_s_.xpm"
#include "xpm/mask_t.xpm"
#include "xpm/mask_u.xpm"
void fillMasks(QComboBox *cb)
{
	// "-+=;oOsS~<>jdD*^"
	cb->addItem(QPixmap(none_xpm), _("none"));
	cb->addItem(QPixmap(mask_m_xpm), _("'-' lines"));
	cb->addItem(QPixmap(mask_p_xpm), _("'+' plus"));
	cb->addItem(QPixmap(mask_e_xpm), _("'=' double lines"));
	cb->addItem(QPixmap(mask_i_xpm), _("';' dash"));
	cb->addItem(QPixmap(mask_o_xpm), _("'o' circle"));
	cb->addItem(QPixmap(mask_O_xpm), _("'O' solid circle"));
	cb->addItem(QPixmap(mask_s_xpm), _("'s' square"));
	cb->addItem(QPixmap(mask_S_xpm), _("'S' solid square"));
	cb->addItem(QPixmap(mask_t_xpm), _("'~' waves"));
	cb->addItem(QPixmap(mask_l_xpm), _("'<' left sign"));
	cb->addItem(QPixmap(mask_r_xpm), _("'>' right sign"));
	cb->addItem(QPixmap(mask_j_xpm), _("'j' dash dot"));
	cb->addItem(QPixmap(mask_d_xpm), _("'d' rhomb"));
	cb->addItem(QPixmap(mask_D_xpm), _("'D' solid rhomb"));
	cb->addItem(QPixmap(mask_a_xpm), _("'*' cross"));
	cb->addItem(QPixmap(mask_u_xpm), _("'^' hats"));
	cb->addItem(QPixmap(":/png/tools-wizard.png"), _("manual"));
}
//-----------------------------------------------------------------------------
#include "xpm/mark_.xpm"
#include "xpm/mark_cf.xpm"
#include "xpm/mark_x.xpm"
#include "xpm/mark_p.xpm"
#include "xpm/mark_pf.xpm"
#include "xpm/mark_o.xpm"
#include "xpm/mark_of.xpm"
#include "xpm/mark_s.xpm"
#include "xpm/mark_sf.xpm"
#include "xpm/mark_d.xpm"
#include "xpm/mark_df.xpm"
#include "xpm/mark_v.xpm"
#include "xpm/mark_vf.xpm"
#include "xpm/mark_t.xpm"
#include "xpm/mark_tf.xpm"
#include "xpm/mark_l.xpm"
#include "xpm/mark_lf.xpm"
#include "xpm/mark_r.xpm"
#include "xpm/mark_rf.xpm"
#include "xpm/mark_y.xpm"
#include "xpm/mark_a.xpm"
void fillMarkers(QComboBox *cb)
{
	// ".+x*sdv^<>o.*+xsdv^<>o" : nf = 10
	cb->addItem(QPixmap(mark_n_xpm), _("none"));
	cb->addItem(QPixmap(mark__xpm), _("'.' dot"));
	cb->addItem(QPixmap(mark_p_xpm), _("'+' plus"));
	cb->addItem(QPixmap(mark_x_xpm), _("'x' skew cross"));
	cb->addItem(QPixmap(mark_a_xpm), _("'*' star"));
	cb->addItem(QPixmap(mark_s_xpm), _("'s' square"));
	cb->addItem(QPixmap(mark_d_xpm), _("'d' rhomb"));
	cb->addItem(QPixmap(mark_v_xpm), _("'v' triangle down"));
	cb->addItem(QPixmap(mark_t_xpm), _("'^' triangle up"));
	cb->addItem(QPixmap(mark_l_xpm), _("'<' triangle left"));
	cb->addItem(QPixmap(mark_r_xpm), _("'>' triangle right"));
	cb->addItem(QPixmap(mark_o_xpm), _("'o' circle"));

	cb->addItem(QPixmap(mark_cf_xpm), _("'#.' circled dot"));
	cb->addItem(QPixmap(mark_y_xpm),  _("'#*' Y-sign"));
	cb->addItem(QPixmap(mark_pf_xpm), _("'#+' squared plus"));
	cb->addItem(QPixmap(none_xpm),	  _("'#x' squared cross"));

	cb->addItem(QPixmap(mark_sf_xpm), _("'#s' solid square"));
	cb->addItem(QPixmap(mark_df_xpm), _("'#d' solid rhomb"));
	cb->addItem(QPixmap(mark_vf_xpm), _("'#v' solid triangle down"));
	cb->addItem(QPixmap(mark_tf_xpm), _("'#^' solid triangle up"));
	cb->addItem(QPixmap(mark_lf_xpm), _("'#<' solid triangle left"));
	cb->addItem(QPixmap(mark_rf_xpm), _("'#>' solid triangle right"));
	cb->addItem(QPixmap(mark_of_xpm), _("'#o' solid circle"));
}
//-----------------------------------------------------------------------------
void StyleDialog::updatePic()
{
	static mglGraph gr(0,128,30);
	static bool f = true;
	mglData x(3), y(3), a(32,2);
	x.Fill(-1,1);	a.Fill(-1,1);
	if(!f)	gr.Clf();
	if(f)
	{
		gr.SubPlot(1,1,0,"");
		gr.SetMarkSize(15);
		gr.SetArrowSize(20);
		f = false;
	}
	result = "";
	int i,j;
	QString col="wbgrcmylenuqphkWBGRCMYLENUQPH", mrk=".+x*sdv^<>o.*+xsdv^<>o", dsh="|;=ji: ", arw="AVIKTSDO", s;
	QString msk="-+=;oOsS~<>jdD*^", dir="/\\I";
	switch(tab->currentIndex())
	{
	case 0:	// line style
		i = a2->currentIndex();		if(i>0)	result += arw[i-1];
		j = a1->currentIndex();		if(j>0)
		{
			if(i==0)	result += '_';
			result += arw[j-1];
		}
		i = dash->currentIndex();
		if(i>0 && i<8)	result += dsh[i-1];
		else if(i==8)	// manual
		{
			int d=0;
			for(int i=0;i<16;i++)	if(dash_bit[i]->isChecked())	d += 1<<i;
			result += "{d"+QString::number(d,16)+"}";
		}
		i = mark->currentIndex();	if(i>0)	result += mrk[i-1];
		if(i>11)	result += '#';
		i = cline->currentIndex();
		if(i>0)
		{
			j = nline->value();
			if(j!=5)	result += "{"+col[i-1]+char('0'+j)+"}";
			else		result += col[i-1];
		}
		i = width->value();		if(i>1)	result += char('0'+i);
		gr.Plot(x,y,result.toLocal8Bit().constData());
		break;
	case 1: // color sceheme
	case 3: // manual mask
		for(j=0;j<7;j++)
		{
			i = cc[j]->currentIndex();
			if(i<1)	break;
			QCharRef c = col[i-1];
			i = nn[j]->value();
			if(i!=5)	result += "{"+c+char('0'+i)+"}";
			else		result += c;
		}
		if(swire->isChecked())	result += '#';
		i = ctext->currentIndex();
		if(i==1)	result += 't';
		if(i==2)	result += 'T';
		i = mask->currentIndex();
		if(i>0 && i<17)
		{
			result += msk[i-1];
			i = angle->currentIndex();
			if(i>0)	result += dir[i-1];
			i = msize->value();
			if(i>1)	result += char('0'+i);
		}
		else if(i==17)
		{
			uint64_t t=0;
			for(int j=0;j<64;j++)	if(mask_bit[j]->isChecked())	t += uint64_t(1)<<j;
			result += "{s"+QString::number(t,16)+"}";
			// TODO get hex-mask
			i = angle->currentIndex();
			if(i>0)	result += dir[i-1];
			i = msize->value();
			if(i>1)	result += char('0'+i);
		}


		i = axial->currentIndex();
		if(i>0)	result = result+':'+char('x'+i-1);
		gr.Surf(a,result.toLocal8Bit().constData());
		break;
	case 2: // text style
		if(font_sch->isChecked())	for(j=0;j<7;j++)
		{
			i = cc[j]->currentIndex();
			if(i<1)	break;
			QCharRef c = col[i-1];
			i = nn[j]->value();
			if(i!=5)	result += "{"+c+char('0'+i)+"}";
			else		result += c;
		}
		else
		{
			i = cfont->currentIndex();
			if(i>1)	result += col[i-1];
		}
		result += ':';
		if(bold->isChecked())	result += 'b';
		if(ital->isChecked())	result += 'i';
		if(wire->isChecked())	result += 'w';
		if(uline->isChecked())	result += 'u';
		if(oline->isChecked())	result += 'o';
		if(rbL->isChecked())	result += 'L';
		if(rbC->isChecked())	result += 'C';
		if(rbR->isChecked())	result += 'R';
		gr.Puts(mglPoint(0,-0.5),"Font test",result.toLocal8Bit().constData(),-10);
		break;
	}
	result = "'" + result + "'";
	res->setText(result);
	QPixmap p;
	convertFromGraph(p, &gr, &grBuf);
	pic->setPixmap(p);
}
//-----------------------------------------------------------------------------
void convertFromGraph(QPixmap &pic, mglGraph *gr, uchar **buf)
{
	long w=gr->GetWidth(), h=gr->GetHeight();
	if(*buf)	delete [](*buf);
	*buf = new uchar[4*w*h];
	gr->GetBGRN(*buf,4*w*h);
	QImage img(*buf, w, h, QImage::Format_RGB32);
	pic = QPixmap::fromImage(img);
}
//-----------------------------------------------------------------------------
void StyleDialog::showFontPage()
{	tab->setCurrentIndex(2);	}
//-----------------------------------------------------------------------------
void StyleDialog::showPenPage()
{	tab->setCurrentIndex(0);	}
//-----------------------------------------------------------------------------
void StyleDialog::showSchPage()
{	tab->setCurrentIndex(1);	}
//-----------------------------------------------------------------------------
