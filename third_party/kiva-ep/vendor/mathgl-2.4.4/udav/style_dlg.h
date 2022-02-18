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
#ifndef STYLEDIALOG_H
#define STYLEDIALOG_H
//-----------------------------------------------------------------------------
#include <qdialog.h>
class QComboBox;
class QSlider;
class QSpinBox;
class QGroupBox;
class QTabWidget;
class QCheckBox;
class QLabel;
class QLineEdit;
class QRadioButton;
class QToolButton;
//-----------------------------------------------------------------------------
/// Selecting styles of command (like line style, color scheme, font style, axis style)
class StyleDialog : public QDialog
{
Q_OBJECT
public:
	QString getStyle()	{	return result;	};
	StyleDialog(QWidget *parent = 0);
	~StyleDialog();
	
	void showFontPage();
	void showPenPage();
	void showSchPage();
private slots:
	void updatePic();
private:
	QString result;
	QComboBox *cc[8], *cline, *cfont;
	QSlider *nn[8], *nline;
	QComboBox *axial, *ctext, *a1, *a2, *dash, *mark;
	QCheckBox *swire;
	QCheckBox *ital, *bold, *wire, *uline, *oline;
	QSpinBox *width;
	QGroupBox *align;
	QTabWidget *tab;
	QLabel *pic;
	QLineEdit *res;
	QRadioButton *rbL, *rbC, *rbR;
	uchar *grBuf;
	QCheckBox *font_sch;
	QComboBox *mask;
	QComboBox *angle;
	QSlider *msize;

	QToolButton *dash_bit[16];	// 8 buttons for dashing
	QToolButton *mask_bit[64];	// 8*8 buttons for mask
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
