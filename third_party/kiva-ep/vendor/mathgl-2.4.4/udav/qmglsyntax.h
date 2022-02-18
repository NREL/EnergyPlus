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
#ifndef QMGLSYNTAX_H
#define QMGLSYNTAX_H
//-----------------------------------------------------------------------------
#include <QSyntaxHighlighter>
#include <QDialog>
#include <QLineEdit>
#include <QTextEdit>
//-----------------------------------------------------------------------------
///Syntax highlighter for MGL script language.
class QMGLSyntax : public QSyntaxHighlighter
{
public:
	QMGLSyntax(QTextEdit *textEdit);
	void highlightBlock(const QString &text);
};
//-----------------------------------------------------------------------------
///Syntax highlighter for messages window.
class MessSyntax : public QSyntaxHighlighter
{
public:
	MessSyntax(QTextEdit *textEdit);
	void highlightBlock(const QString &text);
};
//-----------------------------------------------------------------------------
#endif
//-----------------------------------------------------------------------------
