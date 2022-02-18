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
#ifndef NEWCMDDIALOG_H
#define NEWCMDDIALOG_H
//-----------------------------------------------------------------------------
#include <QDialog>
#include <QStringList>
#define NUM_CH	16		// number of argument sets for a command
class QComboBox;
class QLabel;
class QLineEdit;
class QTableWidget;
class QTextBrowser;
class OptionDialog;
class StyleDialog;
class DataDialog;
//-----------------------------------------------------------------------------
class NewCmdDialog : public QDialog
{
Q_OBJECT
public:
	NewCmdDialog(QWidget *p);
	const QString &getCommand()	{	return cmd;	}

private slots:
	void typeChanged(int);
	void nameChanged(int);
	void kindChanged(int);
	void insertData();
	void insertOpt();
	void insertStl();
	void finish();
	void zoomIn();
	void zoomOut();

public slots:
	void parseCmd(const QString &txt);

signals:
	void result(const QString &txt, bool replace);

private:
	QTextBrowser *help;
	QComboBox *type, *name, *kind;
	QLineEdit *opt;
	QLabel *info;
	QTableWidget *args;
	QString cmd;
	QStringList types, cmds[17], argn[NUM_CH], kinds;
	OptionDialog *optDialog;
	StyleDialog *stlDialog;
	DataDialog *datDialog;
	bool replace;	// flag to be used in result() signal

	void fillList();
};
//-----------------------------------------------------------------------------
#endif // NEWCMDDIALOG_H
//-----------------------------------------------------------------------------
