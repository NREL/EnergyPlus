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
#ifndef PLOT_PNL_H
#define PLOT_PNL_H
//-----------------------------------------------------------------------------
#include <QWidget>
#include <qabstractitemmodel.h>
//-----------------------------------------------------------------------------
class QMenu;
class QTimer;
class QSpinBox;
class QPopupMenu;
class QScrollArea;
class QBoxLayout;
class QTextEdit;
class QMathGL;
class mglDrawScript;
class InfoDialog;
class AnimParam;
class DatPanel;
class StyleDialog;
class QPrinter;
class NewCmdDialog;
class SubplotDialog;
//-----------------------------------------------------------------------------
class PlotPanel : public QWidget
{
Q_OBJECT
public:
	QMenu *menu;
	QMathGL *mgl;
	mglDrawScript *draw;	///< Class for drawing MGL script
	QTextEdit *textMGL;		///< Editor with MGL script body
	NewCmdDialog *newCmdDlg;
	SubplotDialog *subplotDlg;
	PlotPanel(QWidget *wp=0);
	~PlotPanel();
	void setMGLFont(const QString &path);
	QString getFit();

public slots:
	void animParseText(const QString &txt);
	void setCurPos(int pos=-1);
	void execute();

signals:
	void save();
	void animPutText(const QString &);
	void setStatus(const QString &);
	void animSwitch(bool);
	void giveFocus();
	void clearWarn();

private slots:
	void animText(const QString &);
	void next();
	void nextSlide();
	void prevSlide();
	void animStart(bool st);
	void animSetup();
	void adjust();
	void pressF9();
	void stop();
	void setStyle(int id);
	void setSubId(int id);
	void deleteSelected();
	void hideSelected();
	void putCmd(const QString &cmd);
	void insCmd(const QString &cmd);
	void movePlotUp();
	void movePlotDown();

private:
	bool gifOn, jpgOn;
	QScrollArea* sv;
	QSpinBox *tet, *phi;
	// animation
	QString animParam;
	int animPos;
	int curPos;
	QTimer *timer;
	AnimParam *animDialog;
	QMenu *popup;
	QPrinter *printer;
	StyleDialog *stlDialog;
	int objId;
	int subId;
	
	void toolTop(QBoxLayout *l);
	void toolLeft(QBoxLayout *l);
};
//-----------------------------------------------------------------------------
#endif // PLOT_PNL_H
