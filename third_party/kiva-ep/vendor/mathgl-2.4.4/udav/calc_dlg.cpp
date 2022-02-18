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
#include <QListView>
#include <QComboBox>
#include <QTextEdit>
#include <mgl2/mgl.h>
#include "calc_dlg.h"
extern mglParse parser;
//-----------------------------------------------------------------------------
//
//	Calc dialog
//
//-----------------------------------------------------------------------------
QWidget *createCalcDlg(QWidget *p, QTextEdit *e)
{
	CalcDialog *c = new CalcDialog(p);
	QObject::connect(c, SIGNAL(putNumber(QString)),e,SLOT(insertPlainText(QString)));
	return c;
}
//-----------------------------------------------------------------------------
CalcDialog::CalcDialog(QWidget *parent) : QWidget(parent)
{
	QPushButton *b;
	QHBoxLayout *m=new QHBoxLayout(this);
	QVBoxLayout *o=new QVBoxLayout;	m->addLayout(o);	m->setStretchFactor(o,1);
//	QStandardItem *it;

	text = new QLineEdit(this);	o->addWidget(text);
	connect(text,SIGNAL(textChanged(QString)),this,SLOT(evaluate()));
	connect(text,SIGNAL(returnPressed()),this,SLOT(addResult()));
	hist = new QStandardItemModel(this);
//	it = new QStandardItem(_("Formula"));	hist->setHorizontalHeaderItem(0,it);
//	it = new QStandardItem(_("Result"));	hist->setHorizontalHeaderItem(1,it);
	prev = new QListView(this);	o->addWidget(prev);
	connect(prev,SIGNAL(clicked(QModelIndex)),this,SLOT(putText(QModelIndex)));
	QFont f(font());	f.setPointSize(f.pointSize()*0.75);
	prev->setModel(hist);	prev->setFont(f);
	prev->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Ignored);


	o = new QVBoxLayout;		m->addLayout(o);
	QLabel *l = new QLabel(_("Result"),this);	o->addWidget(l);
	result=new QLineEdit(this);	result->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
	result->setReadOnly(true);	o->addWidget(result);
	b = new QPushButton(_("To script"), this);	o->addWidget(b);
	connect(b, SIGNAL(clicked()), this, SLOT(keyPut()));
	b = new QPushButton(_("Clear"), this);	o->addWidget(b);
	connect(b, SIGNAL(clicked()), this, SLOT(clear()));

	QGridLayout *g = new QGridLayout;	m->addLayout(g);	m->setStretchFactor(g,0);
	b = new QPushButton("7", this);	g->addWidget(b, 0, 0);
	int minw=b->height();	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key7()));
	b = new QPushButton("8", this);	g->addWidget(b, 0, 1);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key8()));
	b = new QPushButton("9", this);	g->addWidget(b, 0, 2);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key9()));
	b = new QPushButton("+", this);	g->addWidget(b, 0, 3);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyAdd()));
	b = new QPushButton(QString::fromWCharArray(L"\u03c0"), this);	g->addWidget(b, 0, 4);
	connect(b, SIGNAL(clicked()), this, SLOT(keyPi()));		b->setMaximumWidth(minw);

	b = new QPushButton("4", this);	g->addWidget(b, 1, 0);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key4()));
	b = new QPushButton("5", this);	g->addWidget(b, 1, 1);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key5()));
	b = new QPushButton("6", this);	g->addWidget(b, 1, 2);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key6()));
	b = new QPushButton("-", this);	g->addWidget(b, 1, 3);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keySub()));
	b = new QPushButton(QString::fromWCharArray(L"x\xb2"), this);	g->addWidget(b, 1, 4);
	connect(b, SIGNAL(clicked()), this, SLOT(keyX2()));		b->setMaximumWidth(minw);

	b = new QPushButton("1", this);	g->addWidget(b, 2, 0);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key1()));
	b = new QPushButton("2", this);	g->addWidget(b, 2, 1);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key2()));
	b = new QPushButton("3", this);	g->addWidget(b, 2, 2);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key3()));
	b = new QPushButton("*", this);	g->addWidget(b, 2, 3);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyMul()));
	b = new QPushButton("(", this);	g->addWidget(b, 2, 4);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyBrO()));

	b = new QPushButton("0", this);	g->addWidget(b, 3, 0);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(key0()));
	b = new QPushButton(".", this);	g->addWidget(b, 3, 1);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyDot()));
	b = new QPushButton("E", this);	g->addWidget(b, 3, 2);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyE()));
	b = new QPushButton("/", this);	g->addWidget(b, 3, 3);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyDiv()));
	b = new QPushButton(")", this);	g->addWidget(b, 3, 4);	b->setMaximumWidth(minw);
	connect(b, SIGNAL(clicked()), this, SLOT(keyBrC()));

	fillFuncName();
	o=new QVBoxLayout;	m->addLayout(o);	m->setStretchFactor(o,0);
	type = new QComboBox(this);		o->addWidget(type);
	type->addItems(names);	type->setCurrentIndex(0);
	func = new QComboBox(this);		o->addWidget(func);
	func->addItems(funcName[0]);	type->setCurrentIndex(0);
	descr= new QLabel(this);		o->addWidget(descr);	o->setStretchFactor(descr,0);
	descr->setText(funcInfo[0].at(0));
	connect(type, SIGNAL(currentIndexChanged(int)), this, SLOT(typeUpdate(int)));
	connect(func, SIGNAL(currentIndexChanged(int)), this, SLOT(funcUpdate(int)));
	b = new QPushButton(_("Put function"), this);	o->addWidget(b);
	connect(b, SIGNAL(clicked()), this, SLOT(keyFnc()));
}
//-----------------------------------------------------------------------------
CalcDialog::~CalcDialog()	{}
void CalcDialog::foc()	{	text->setFocus(Qt::ActiveWindowFocusReason);	}
//-----------------------------------------------------------------------------
void CalcDialog::key1()		{	text->insert("1");	foc();	}
void CalcDialog::key2()		{	text->insert("2");	foc();	}
void CalcDialog::key3()		{	text->insert("3");	foc();	}
void CalcDialog::key4()		{	text->insert("4");	foc();	}
void CalcDialog::key5()		{	text->insert("5");	foc();	}
void CalcDialog::key6()		{	text->insert("6");	foc();	}
void CalcDialog::key7()		{	text->insert("7");	foc();	}
void CalcDialog::key8()		{	text->insert("8");	foc();	}
void CalcDialog::key9()		{	text->insert("9");	foc();	}
void CalcDialog::key0()		{	text->insert("0");	foc();	}
void CalcDialog::keyE()		{	text->insert("E");	foc();	}
void CalcDialog::keyPi()	{	text->insert("pi");	foc();	}
void CalcDialog::keyX2()	{	text->insert("^2");	foc();	}
void CalcDialog::keyAdd()	{	text->insert("+");	foc();	}
void CalcDialog::keyMul()	{	text->insert("*");	foc();	}
void CalcDialog::keySub()	{	text->insert("-");	foc();	}
void CalcDialog::keyDiv()	{	text->insert("/");	foc();	}
void CalcDialog::keyBrO()	{	text->insert("(");	foc();	}
void CalcDialog::keyBrC()	{	text->insert(")");	foc();	}
void CalcDialog::keyDot()	{	text->insert(".");	foc();	}
void CalcDialog::clear()	{	text->clear();	foc();	}
//-----------------------------------------------------------------------------
void CalcDialog::keyFnc()
{
	text->insert(func->currentText());
	text->setCursorPosition(text->cursorPosition()-1);	foc();
}
//-----------------------------------------------------------------------------
void CalcDialog::keyPut()	{	emit putNumber(result->text());	}
//-----------------------------------------------------------------------------
void CalcDialog::putText(QModelIndex ind)
{	text->setText(hist->data(ind).toString());	}
//-----------------------------------------------------------------------------
void CalcDialog::addResult()
{
	QStandardItem *it;
	QFont f(prev->font());	f.setBold(true);
	hist->insertRows(0,2);
	it = new QStandardItem(text->text());	it->setFont(f);	hist->setItem(0,it);
	it = new QStandardItem(result->text());	hist->setItem(1,it);
}
//-----------------------------------------------------------------------------
void CalcDialog::evaluate()
{
	QString sel=text->text();
	if(sel.isEmpty())	return;
	wchar_t *txt=new wchar_t[sel.length()+1];
	sel.toWCharArray(txt);	txt[sel.length()]=0;
	setlocale(LC_NUMERIC, "C");
	mglData res=parser.Calc(txt);
	setlocale(LC_NUMERIC, "");
//	result->setText(QString::fromWCharArray(txt));
	delete []txt;
	result->setText(QString::number(res.GetVal(0)));
}
//-----------------------------------------------------------------------------
void CalcDialog::fillFuncName()
{
	names<<_("Basic")<<_("Exp and log")<<_("Trigonometric")<<_("Hyperbolic")
			<<_("Bessel")<<_("Elliptic")<<_("Jacobi")<<_("Airy and Gamma")
			<<_("Exp-integrals")<<_("Special");
	// basic
	funcName[0]<<"abs()"<<"sign()"<<"step()"<<"sqrt()"<<"mod(,)"<<"arg(,)";
	funcInfo[0]<<"Absolute value"<<"Sign of number"<<"Step function"
			<<"Square root"<<"x modulo y"<<"Argument of complex number";
	// exp and logarithms
	funcName[1]<<"exp()"<<"pow(,)"<<"ln()"<<"lg()"<<"log(,)";
	funcInfo[1]<<"Exponential function e^x"<<"Power x^y"<<"Logarithm of x"
			<<"Decimal logarithm of x"<<"Logarithm of x on base a";
	// trigonometric
	funcName[2]<<"sin()"<<"cos()"<<"tan()"<<"sinc()"<<"asin()"<<"acos()"<<"atan()";
	funcInfo[2]<<"Sine function"<<"Cosine function"<<"Tangent function"<<"sin(x)/x"
			<<"Inverse sine function"<<"Inverse cosine function"<<"Inverse tangent function";
	// hyperbolic
	funcName[3]<<"sinh()"<<"cosh()"<<"tanh()"<<"asinh()"<<"acosh()"<<"atanh()";
	funcInfo[3]<<"Hyperbolic sine function"<<"Hyperbolic cosine function"
			<<"Hyperbolic tangent function"<<"Inverse hyperbolic sine function"
			<<"Inverse hyperbolic cosine function"<<"Inverse hyperbolic tangent function";
	// bessel
	funcName[4]<<"bessel_j(,)"<<"bessel_y(,)"<<"bessel_i(,)"<<"bessel_k(,)";
	funcInfo[4]<<"Regular cylindrical Bessel function"<<"Irregular cylindrical Bessel function"
			<<"Regular modified Bessel function"<<"Irregular modified Bessel function";
	// elliptic
	funcName[5]<<"elliptic_e(,)"<<"elliptic_f(,)"<<"elliptic_ec()"<<"elliptic_kc()";
	funcInfo[5]<<"Elliptic integral E(phi,k)"<<"Elliptic integral F(phi,k)"
			<<"Complete elliptic integral E(k)"<<"Complete elliptic integral K(k)";
	// jacobi
	funcName[6]<<"sn(,)"<<"cn(,)"<<"dn(,)"<<"sc(,)"<<"dc(,)"<<"nc(,)"<<"cs(,)"
			<<"ds(,)"<<"ns(,)"<<"sd(,)"<<"cd(,)"<<"nd(,)";
	funcInfo[6]<<"Jacobi function sn(u|m)"<<"Jacobi function cn(u|m)"
			<<"Jacobi function dn(u|m)"<<"Jacobi function sn(u|m)/cn(u|m)"
			<<"Jacobi function dn(u|m)/cn(u|m)"<<"Jacobi function 1/cn(u|m)"
			<<"Jacobi function cn(u|m)/sn(u|m)"<<"Jacobi function dn(u|m)/sn(u|m)"
			<<"Jacobi function 1/sn(u|m)"<<"Jacobi function sn(u|m)/dn(u|m)"
			<<"Jacobi function cn(u|m)/dn(u|m)"<<"Jacobi function 1/dn(u|m)";
	// airy and gamma
	funcName[7]<<"airy_ai()"<<"airy_bi()"<<"airy_dai()"<<"airy_dbi()"<<"gamma()"<<"psi()"<<"beta(,)";
	funcInfo[7]<<"Airy function Ai(x)"<<"Airy function Bi(x)"
			<<"Derivative of Airy function Ai'(x)"<<"Derivative of Airy function Bi'(x)"
			<<QString::fromWCharArray(L"Gamma function \u0393(x)")
			<<QString::fromWCharArray(L"Digamma function \u0393'(x)/\u0393(x)")
			<<QString::fromWCharArray(L"Beta function \u0393(x)*\u0393(y)/\u0393(x+y)");
	// exp integrals
	funcName[8]<<"ci()"<<"si()"<<"ei()"<<"e1()"<<"e2()"<<"ei3()";
	funcInfo[8]<<QString::fromWCharArray(L"Cosine integral \u222bdt cos(t)/t")
			<<QString::fromWCharArray(L"Sine integral \u222bdt sin(t)/t")
			<<QString::fromWCharArray(L"Integral -\u222bdt exp(-t)/t")
			<<QString::fromWCharArray(L"Integral Re \u222bdt exp(-xt)/t")
			<<QString::fromWCharArray(L"Integral Re\u222bdt exp(-xt)/t^2")
			<<QString::fromWCharArray(L"Integral \u222bdt exp(-t^3)");
	// special
	funcName[9]<<"erf()"<<"z()"<<"legendre(,)"<<"dilog()"<<"eta()"<<"zeta()"<<"w0()"<<"w1()";
	funcInfo[9]<<QString::fromWCharArray(L"Error function 2/√π \u222bdt exp(-t^2)")<<"Dawson function"
			<<"Legendre polynomial P_l(x)"<<QString::fromWCharArray(L"Dilogarithm -Re\u222bds ln(1-s)/s")
			<<"Eta function (1-2/2^s)*zeta(s)"<<"Riemann zeta function"
			<<"Lambert W function W_0(x)"<<"Lambert W function W_{-1}(x)";
}
//-----------------------------------------------------------------------------
void CalcDialog::typeUpdate(int s)
{
	if(s<0 || s>9)	return;
	func->clear();	func->addItems(funcName[s]);	func->setCurrentIndex(0);
}
//-----------------------------------------------------------------------------
void CalcDialog::funcUpdate(int f)
{
	int s=type->currentIndex();
	if(s<0 || s>9 || f<0)	return;	// wrong index
	descr->setText(funcInfo[s].at(f));
}
//-----------------------------------------------------------------------------
