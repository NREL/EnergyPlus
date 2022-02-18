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
#ifndef SETUPDIALOG_H
#define SETUPDIALOG_H
//-----------------------------------------------------------------------------
#include <qdialog.h>
#if defined(_MSC_VER)
#include <mgl2/define.h>
#endif
class QLineEdit;
class QCheckBox;
class QComboBox;
//-----------------------------------------------------------------------------
/// Add setup code to script or save it to file (template)
class SetupDialog : public QDialog
{
Q_OBJECT
public:
	SetupDialog(QWidget *parent = 0);
	~SetupDialog();

signals:
	void putText(const QString &par);

private slots:
	void saveTmpl();
	void toScript();

private:
	QString res;
	QLineEdit *xmin, *xmax, *xorg, *xlbl, *xtck, *xsub, *xort, *xtt;
	QLineEdit *ymin, *ymax, *yorg, *ylbl, *ytck, *ysub, *yort, *ytt;
	QLineEdit *zmin, *zmax, *zorg, *zlbl, *ztck, *zsub, *zort, *ztt;
	QLineEdit *cmin, *cmax, *aldef,*amb,  *basew,*mesh, *font, *fsize, *fname, *ctt;
	QLineEdit *xlight[10], *ylight[10], *zlight[10], *blight[10];
	QCheckBox *slight[10], *alpha, *light, *rotate;
	QComboBox *clight[10], *xpos, *ypos, *zpos, *axial;
	
	bool convert();
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
