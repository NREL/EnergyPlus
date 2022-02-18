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
#include <QPushButton>
#include <QLabel>
#include <QLayout>
#include "qmglsyntax.h"
#include <mgl2/mgl.h>
mglParse parser;
int mgl_cmd_cmp(const void *a, const void *b);
// comment string keyword option suffix number
QColor mglColorScheme[10] = {QColor(0,127,0), QColor(255,0,0), QColor(0,0,127), QColor(127,0,0), QColor(127,0,0), QColor(0,0,255), QColor(127,0,127), QColor(0,127,127), QColor(0,0,127), QColor(255,255,204)};
//-----------------------------------------------------------------------------
QMGLSyntax::QMGLSyntax(QTextEdit *textEdit) : QSyntaxHighlighter(textEdit)	{}
//-----------------------------------------------------------------------------
void QMGLSyntax::highlightBlock(const QString &text)
{
	int i, j, m = text.length(),s=0;
	bool arg = false, nl = true;
	QString num("+-.0123456789:");
	i=0;
	setCurrentBlockState(-1);
	if(previousBlockState()==1)
	{
		bool cont=false;
		j=i;	i++;
		for(;i<m && text[i]!='\'';i++)
		{
			if(text[i]>' ')		cont=false;
			if(text[i]=='\\')	cont=true;
		}
		setFormat(j,i-j+1,mglColorScheme[1]);
		if(cont && i==m)	setCurrentBlockState(1);
	}
	for(;i<m;i++)				// highlight paragraph
	{
		if(text[i]=='(')	s++;
		if(text[i]==')')	s--;
		if(text[i]==' ' || text[i]=='\t')	continue;
		else if(text[i]=='#')	// comment
		{	setFormat(i,m-i,mglColorScheme[0]);	break;	}
		else if(text[i]=='\'')	// string
		{
			bool cont=false;
			j=i;	i++;
			for(;i<m && text[i]!='\'';i++)
			{
				if(text[i]>' ')		cont=false;
				if(text[i]=='\\')	cont=true;
			}
			setFormat(j,i-j+1,mglColorScheme[1]);
			if(cont && i==m)	setCurrentBlockState(1);
		}
		else if(nl)				// keyword
		{
			char *s = new char[m+1];
			for(j=i;j<text.length() && !text[j].isSpace() && text[j]!=':';j++)
				s[j-i] = text[j].toLatin1();
			s[j-i]=0;
			int type = parser.CmdType(s);
			if(type)	setFormat(i,j-i+1,type!=7 ? (type==5 ? mglColorScheme[6] : mglColorScheme[2]) : mglColorScheme[7]);
			delete []s;
		}
		else if(text[i]==';')	{	arg = true;	nl = false;	continue;	}
		else if(text[i]==':' && s==0)	{	nl=true;	continue;	}
		else if(arg)			// option
		{
			const char *o[13]={"xrange","yrange","zrange","cut","meshnum","alpha","light","ambient","diffuse","size","legend","number","value"};
			unsigned l;
			for(j=0;j<13;j++)
			{
				l = strlen(o[j]);
				if(text.indexOf(o[j],i)==i && (i+l==long(text.length()) || text[i+l].isSpace()))
					setFormat(i,l,mglColorScheme[3]);
			}
		}
		else if(text[i]=='.' && i+1<text.length() && text[i+1].isLetter())	// suffix
		{
			for(j=i;j<text.length() && !text[j].isSpace();j++){};
			setFormat(i,j-i+1,mglColorScheme[4]);
		}
		else if(num.contains(text[i]))	// number
			setFormat(i,1,mglColorScheme[5]);
		else if((text[i]=='e' || text[i]=='E') && i+1<text.length() && num.contains(text[i-1]) && num.contains(text[i+1]) )
			setFormat(i,1,mglColorScheme[5]);
		else		// number as its symbolic id
		{
			const char *o[6]={"nan","inf","pi","on","off","all"};
			int l[6] = {3, 3, 2, 2, 3, 3};
			for(j=0;j<6;j++)
				if(text.indexOf(o[j],i)==i && (i+l[j]==text.length() || text[i+l[j]].isSpace()))
					setFormat(i,l[j],mglColorScheme[5]);
		}
		arg = nl = false;
	}
}
//-----------------------------------------------------------------------------
MessSyntax::MessSyntax(QTextEdit *textEdit) : QSyntaxHighlighter(textEdit)	{}
//-----------------------------------------------------------------------------
void MessSyntax::highlightBlock(const QString &text)
{
	if(text.contains("in line "))
		setFormat(0, text.length(), QColor(255,0,0));
}
//-----------------------------------------------------------------------------
