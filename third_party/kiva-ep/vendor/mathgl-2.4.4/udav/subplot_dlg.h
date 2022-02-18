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
#ifndef SUBPLOTDIALOG_H
#define SUBPLOTDIALOG_H
//-----------------------------------------------------------------------------
#include <QDialog>
class QRadioButton;
class QSpinBox;
class QCheckBox;
class QLabel;
class QLineEdit;
class StyleDialog;
//-----------------------------------------------------------------------------
class SubplotDialog : public QDialog
{
Q_OBJECT
public:
	SubplotDialog(QWidget *p);
	~SubplotDialog();
	const QString &getCommand()	{	return cmd;	}
public slots:
	void parseCmd(const QString &txt, bool final=true);
	void finish();
signals:
	void result(const QString &txt);
private slots:
	void updatePic();
	void titleStl();
private:
	QRadioButton *cb, *ci, *cm, *cg, *cc, *cs;
	QSpinBox *tet, *phi;
	QLineEdit *axz, *ayz;
	QLineEdit *title, *res;
	QSpinBox *bn,*bm,*bk;			// subplot
	QLineEdit *x1,*x2,*y1,*y2;		// inplot
	QSpinBox *sn,*sk;				// stickplot
	QSpinBox *mn,*mm,*mk,*mx,*my;	// multiplot
	QSpinBox *gm,*gn,*gk;			// gridplot
	QSpinBox *cn,*ck;				// columnplot
	QLineEdit *cd, *gd;
	QCheckBox *rb,*rr,*rl,*rt,*rw;	// style (where reserve)
	QString cmd;	// resulting command
	QString fmt;	// format for title
	QLabel *pic;	// resulting image
//	bool replace;	// flag to be used in result() signal
	uchar *grBuf;
	StyleDialog *stlDialog;
};
//-----------------------------------------------------------------------------
#endif // SUBPLOTDIALOG_H
//-----------------------------------------------------------------------------
