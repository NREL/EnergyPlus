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
#ifndef DAT_PNL_H
#define DAT_PNL_H
//-----------------------------------------------------------------------------
#include <QWidget>
//-----------------------------------------------------------------------------
class mglDataA;
class QMenu;
class QBoxLayout;
class QTableWidget;
class QSpinBox;
class InfoDialog;
//-----------------------------------------------------------------------------
/// Window for viewing, handling and editing the data array
class DatPanel : public QWidget
{
Q_OBJECT
public:
	QMenu *menu;
	InfoDialog *infoDlg;	///< Reference to dialog !!!
	DatPanel(InfoDialog *inf, QWidget *parent = 0);
	~DatPanel();

	void setVar(mglDataA *v);
	inline long GetNz()	{	return nz;	}	///< Get number of slices
	QString dataName();

public slots:
	QString dataOper()	{	return opers;	}
	void refresh();		///< Refresh table with new data values

signals:
	void sliceChanged(int);
	void nzChanged(int);

private slots:
	void setSlice(int k);
	void putValue(int r, int c);
	// menu genereal
	void load();
	void save();
	void copy();
	void paste();
	void plot();
	void list();
	// menu fill
	void inrange();
	void norm();
	void normsl();
	// menu resize
	void create();
	void reSize();
	void squize();
	void crop();
	void rearrange();
	// menu change
	void oper();
	// menu another
	void newdat();
	void hist();
	// menu navigation
	void first();
	void last();
	void next();
	void prev();
	void gosl();
	void setNz(int nz);

private:
	int nx,ny,nz;	///< Data sizes
	QString id;
	QString opers;	///< MGL script of all operations done on data
	QTableWidget *tab;	///< Table itself
	int kz;			///< Current z-slice
	mglDataA *var;	///< Variable with data
//	QSpinBox *sb;	///< SpinBox for selecting slice
	bool ready;		///< Data is refreshed
	QSpinBox *sb;

	bool sizesDialog(const QString &cap, const QString &lab, const QString &desc1, const QString &desc2, const QString &desc3, QString &val1, QString &val2, QString &val3);
	void toolTop(QBoxLayout *l);
	void toolLeft(QBoxLayout *l);
};
//-----------------------------------------------------------------------------
#endif // DAT_PNL_H
//-----------------------------------------------------------------------------
