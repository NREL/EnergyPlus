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
#ifndef CALC_DLG_H
#define CALC_DLG_H
//-----------------------------------------------------------------------------
#include <QDialog>
#include <QStringList>
#include <QStandardItemModel>
class QLabel;
class QLineEdit;
class QTextEdit;
class QComboBox;
class QListView;
class QPushButton;
//-----------------------------------------------------------------------------
/// Dialog for finding something in text
class CalcDialog : public QWidget
{
Q_OBJECT
public:
	CalcDialog(QWidget *parent = 0);
	~CalcDialog();
public slots:
	void evaluate();
signals:
	void putNumber(const QString &str);
private slots:
	void keyPut();
	void key1();
	void key2();
	void key3();
	void key4();
	void key5();
	void key6();
	void key7();
	void key8();
	void key9();
	void key0();
	void keyE();
	void keyDot();
	void keyMul();
	void keyDiv();
	void keyAdd();
	void keySub();
	void keyBrO();
	void keyBrC();
	void keyFnc();
	void keyPi();
	void keyX2();
	void typeUpdate(int s);
	void funcUpdate(int s);
	void foc();
	void clear();
	void putText(QModelIndex ind);
	void addResult();

private:
	QLineEdit *text;
	QLineEdit *result;
	QComboBox *type, *func;
	QLabel *descr;
	QListView *prev;
	QStringList names, funcName[10], funcInfo[10];
	QStandardItemModel *hist;

	void fillFuncName();
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
