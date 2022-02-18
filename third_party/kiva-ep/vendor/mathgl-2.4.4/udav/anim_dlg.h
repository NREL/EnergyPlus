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
#ifndef ANIMPARAM_H
#define ANIMPARAM_H
//-----------------------------------------------------------------------------
#include <QDialog>
#include "mgl2/define.h"
//-----------------------------------------------------------------------------
class QLineEdit;
class QTextEdit;
class QRadioButton;
class QCheckBox;
/// Setup animation parqmeters
class AnimParam : public QDialog
{
Q_OBJECT
public:
	bool gifOn, jpgOn;
	/// string with resulting animation parameters
	const QString &getResult()	{	return res;	}
	void setResult(const QString &s);
	void setResult(double a1,double a2,double da);
	AnimParam(QWidget *parent=0);
	~AnimParam();
signals:
	void putText(const QString &par);
private slots:
	void fillRes();
	void putTxt();
	void setRBF();
	void setRBT();
private:
	QString res;
	QLineEdit *p1, *p2, *dp, *delay;//, *fname;
	QTextEdit *text;
	QRadioButton *rbt, *rbf;
	QCheckBox *gif, *jpg;
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
