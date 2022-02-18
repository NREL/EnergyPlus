/****************************************************************************
 **
 ** Copyright (C) 2010 Nokia Corporation and/or its subsidiary(-ies).
 ** All rights reserved.
 ** Contact: Nokia Corporation (qt-info@nokia.com)
 **
 ** This file is part of the examples of the Qt Toolkit.
 **
 ** $QT_BEGIN_LICENSE:BSD$
 ** You may use this file under the terms of the BSD license as follows:
 **
 ** "Redistribution and use in source and binary forms, with or without
 ** modification, are permitted provided that the following conditions are
 ** met:
 **   * Redistributions of source code must retain the above copyright
 **     notice, this list of conditions and the following disclaimer.
 **   * Redistributions in binary form must reproduce the above copyright
 **     notice, this list of conditions and the following disclaimer in
 **     the documentation and/or other materials provided with the
 **     distribution.
 **   * Neither the name of Nokia Corporation and its Subsidiary(-ies) nor
 **     the names of its contributors may be used to endorse or promote
 **     products derived from this software without specific prior written
 **     permission.
 **
 ** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 ** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 ** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 ** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 ** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 ** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 ** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 ** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 ** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 ** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 ** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
 ** $QT_END_LICENSE$
 **
 ****************************************************************************/

#include "textedit.h"
#include <string.h>
#include <QCompleter>
#include <QKeyEvent>
#include <QAbstractItemView>
#include <QtDebug>
#include <QApplication>
#include <QModelIndex>
#include <QAbstractItemModel>
#include <QScrollBar>
#include <QPainter>
#include <QTextBlock>
#include <QAbstractTextDocumentLayout>

extern QColor mglColorScheme[10];
//-----------------------------------------------------------------------------
TextEdit::TextEdit(QWidget *parent) : QTextEdit(parent)
{
	c=0;
	connect(this, SIGNAL(cursorPositionChanged()), this, SLOT(highlight()));
	// Line numbers
	lineNumberArea = new LineNumberArea(this);
	connect(document(), SIGNAL(blockCountChanged(int)), this, SLOT(updateLineNumberAreaWidth(int)));
	connect(document(), SIGNAL(blockCountChanged(int)), this, SLOT(updateLineNumberArea(int)));
	connect(verticalScrollBar(), SIGNAL(valueChanged(int)), this, SLOT(updateLineNumberArea(int)));
	connect(this, SIGNAL(textChanged()), this, SLOT(updateLineNumberArea()));
	connect(this, SIGNAL(cursorPositionChanged()), this, SLOT(updateLineNumberArea()));
	updateLineNumberAreaWidth(0);
}
//-----------------------------------------------------------------------------
void TextEdit::highlight()
{
	QList<QTextEdit::ExtraSelection> extraSelections;
	if (!isReadOnly())
	{
		QTextEdit::ExtraSelection selection;
		selection.format.setBackground(mglColorScheme[9]);
		selection.format.setProperty(QTextFormat::FullWidthSelection, true);
		selection.cursor = textCursor();
		selection.cursor.clearSelection();
		extraSelections.append(selection);
	}
	setExtraSelections(extraSelections);
}
//-----------------------------------------------------------------------------
void TextEdit::setCompleter(QCompleter *completer)
{
	if(c && c!=completer)
	{	QObject::disconnect(c);	c->setWidget(0);	delete c;	c=0;	}
	if(!completer)	return;
	c = completer;
	c->setWidget(this);
	c->setCompletionMode(QCompleter::PopupCompletion);
	c->setCaseSensitivity(Qt::CaseInsensitive);
	QObject::connect(c, SIGNAL(activated(QString)), this, SLOT(insertCompletion(QString)));
}
//-----------------------------------------------------------------------------
void TextEdit::insertCompletion(const QString& completion)
{
	if (!c || c->widget() != this)	return;
	QTextCursor tc = textCursor();
	int extra = completion.length() - c->completionPrefix().length();
	tc.movePosition(QTextCursor::Left);
	tc.movePosition(QTextCursor::EndOfWord);
	tc.insertText(completion.right(extra));
	setTextCursor(tc);
}
//-----------------------------------------------------------------------------
QString TextEdit::textUnderCursor() const
{
	QTextCursor tc = textCursor();
	tc.select(QTextCursor::WordUnderCursor);
	return tc.selectedText();
}
//-----------------------------------------------------------------------------
void TextEdit::focusInEvent(QFocusEvent *e)
{	if (c)	c->setWidget(this);	QTextEdit::focusInEvent(e);	}
//-----------------------------------------------------------------------------
void TextEdit::keyPressEvent(QKeyEvent *e)
{
	if (c && c->popup()->isVisible())
	{
		// The following keys are forwarded by the completer to the widget
		switch (e->key())
		{
		case Qt::Key_Enter:
		case Qt::Key_Return:
		case Qt::Key_Escape:
		case Qt::Key_Tab:
		case Qt::Key_Backtab:
			e->ignore();	return; // let the completer do default behavior
		default:
			break;
		}
	}

	bool isShortcut = ((e->modifiers() & Qt::ControlModifier) && e->key() == Qt::Key_E); // CTRL+E
	if (!c || !isShortcut) // do not process the shortcut when we have a completer
		QTextEdit::keyPressEvent(e);

	const bool ctrlOrShift = e->modifiers() & (Qt::ControlModifier | Qt::ShiftModifier);
	if (!c || (ctrlOrShift && e->text().isEmpty()))
		return;

	static QString eow("~!@#$%^&*()_+{}|:\"<>?,./;'[]\\-="); // end of word
	bool hasModifier = (e->modifiers() != Qt::NoModifier) && !ctrlOrShift;
	QString completionPrefix = textUnderCursor();

	if (!isShortcut && (hasModifier || e->text().isEmpty()|| completionPrefix.length() < 3
						|| eow.contains(e->text().right(1))))
	{	c->popup()->hide();		return;	}

	if (completionPrefix != c->completionPrefix())
	{
		c->setCompletionPrefix(completionPrefix);
		c->popup()->setCurrentIndex(c->completionModel()->index(0, 0));
	}
	QRect cr = cursorRect();
	cr.setWidth(c->popup()->sizeHintForColumn(0) + c->popup()->verticalScrollBar()->sizeHint().width());
	c->complete(cr); // popup it up!
}
//-----------------------------------------------------------------------------
//
//	Line numbering (slightly modified code of Gauthier Boaglio)
//	Original: https://stackoverflow.com/questions/2443358/how-to-add-lines-numbers-to-qtextedit/
//
//-----------------------------------------------------------------------------
int TextEdit::lineNumberAreaWidth()
{
	int digits = 1;
	int max = qMax(1, document()->blockCount());
	while (max >= 10)	{	max /= 10;	++digits;	}
	int space = 13 +  fontMetrics().width(QLatin1Char('9')) * (digits);
	return space;
}
//-----------------------------------------------------------------------------
void TextEdit::updateLineNumberAreaWidth(int /* newBlockCount */)
{	setViewportMargins(lineNumberAreaWidth(), 0, 0, 0);	}
//-----------------------------------------------------------------------------
void TextEdit::updateLineNumberArea(QRectF /*rect_f*/)
{	TextEdit::updateLineNumberArea();	}
//-----------------------------------------------------------------------------
void TextEdit::updateLineNumberArea(int /*slider_pos*/)
{	TextEdit::updateLineNumberArea();	}
//-----------------------------------------------------------------------------
void TextEdit::updateLineNumberArea()
{
	/* When the signal is emitted, the sliderPosition has been adjusted according to the action,
	 * but the value has not yet been propagated (meaning the valueChanged() signal was not yet emitted),
	 * and the visual display has not been updated. In slots connected to this signal you can thus safely
	 * adjust any action by calling setSliderPosition() yourself, based on both the action and the
	 * slider's value. */
	// Make sure the sliderPosition triggers one last time the valueChanged() signal with the actual value !!!!
	verticalScrollBar()->setSliderPosition(verticalScrollBar()->sliderPosition());

	// Since "QTextEdit" does not have an "updateRequest(...)" signal, we chose
	// to grab the imformations from "sliderPosition()" and "contentsRect()".
	// See the necessary connections used (Class constructor implementation part).

	QRect rect =  contentsRect();
	lineNumberArea->update(0, rect.y(), lineNumberArea->width(), rect.height());
	updateLineNumberAreaWidth(0);
	//----------
	int dy = verticalScrollBar()->sliderPosition();
	if (dy > -1) {
		lineNumberArea->scroll(0, dy);
	}

	// Addjust slider to alway see the number of the currently being edited line...
	int first_block_id = getFirstVisibleBlockId();
	if (first_block_id == 0 || textCursor().block().blockNumber() == first_block_id-1)
		verticalScrollBar()->setSliderPosition(dy-document()->documentMargin());
}
//-----------------------------------------------------------------------------
void TextEdit::resizeEvent(QResizeEvent *e)
{
	QTextEdit::resizeEvent(e);
	QRect cr = contentsRect();
	lineNumberArea->setGeometry(QRect(cr.left(), cr.top(), lineNumberAreaWidth(), cr.height()));
}
//-----------------------------------------------------------------------------
int TextEdit::getFirstVisibleBlockId()
{
	// Detect the first block for which bounding rect - once translated
	// in absolute coordinated - is contained by the editor's text area

	// Costly way of doing but since "blockBoundingGeometry(...)" doesn't
	// exists for "QTextEdit"...

	QTextCursor curs = QTextCursor(document());
	curs.movePosition(QTextCursor::Start);
	for(int i=0; i < document()->blockCount(); ++i)
	{
		QTextBlock block = curs.block();

		QRect r1 = viewport()->geometry();
		QRect r2 = document()->documentLayout()->blockBoundingRect(block).translated(
					viewport()->geometry().x(), viewport()->geometry().y() - (
						verticalScrollBar()->sliderPosition()
						) ).toRect();

		if (r1.contains(r2, true)) { return i; }

		curs.movePosition(QTextCursor::NextBlock);
	}

	return 0;
}
//-----------------------------------------------------------------------------
void TextEdit::lineNumberAreaPaintEvent(QPaintEvent *event)
{
	verticalScrollBar()->setSliderPosition(verticalScrollBar()->sliderPosition());

	QPainter painter(lineNumberArea);
	painter.fillRect(event->rect(), Qt::lightGray);
	int blockNumber = getFirstVisibleBlockId();

	QTextBlock block = document()->findBlockByNumber(blockNumber);
	QTextBlock prev_block = (blockNumber > 0) ? document()->findBlockByNumber(blockNumber-1) : block;
	int translate_y = (blockNumber > 0) ? -verticalScrollBar()->sliderPosition() : 0;

	int top = viewport()->geometry().top();

	// Adjust text position according to the previous "non entirely visible" block
	// if applicable. Also takes in consideration the document's margin offset.
	int additional_margin;
	if (blockNumber == 0)	// Simply adjust to document's margin
		additional_margin = (int) document()->documentMargin() -1 - verticalScrollBar()->sliderPosition();
	else	// Getting the height of the visible part of the previous "non entirely visible" block
		additional_margin = (int) document()->documentLayout()->blockBoundingRect(prev_block)
				.translated(0, translate_y).intersected(viewport()->geometry()).height();
	top += additional_margin;	// Shift the starting point

	int bottom = top + (int) document()->documentLayout()->blockBoundingRect(block).height();

	QColor colErr(255, 0, 0);	// Error line
	QColor colCur(0, 128, 255);	// Current line
	QColor colDef(0,0,0);		// Other lines

	// Draw the numbers (displaying the current line number in green)
	while (block.isValid() && top <= event->rect().bottom())
	{
		if (block.isVisible() && bottom >= event->rect().top())
		{
			QString number = QString::number(blockNumber + 1);
			painter.setPen(QColor(120, 120, 120));
			painter.setPen((textCursor().blockNumber() == blockNumber) ? colCur :
										(isErrLine(blockNumber+1)?colErr:colDef) );
			painter.drawText(-5, top, lineNumberArea->width(), fontMetrics().height(), Qt::AlignRight, number);
		}

		block = block.next();
		top = bottom;
		bottom = top + (int) document()->documentLayout()->blockBoundingRect(block).height();
		blockNumber++;
	}
}
//-----------------------------------------------------------------------------
bool TextEdit::isErrLine(int line) const
{
	for(size_t i=0;i<err.size();i++)	if(err[i]==line)	return true;
	return false;
}
//-----------------------------------------------------------------------------
void TextEdit::setErrMessage(const QString &mess)
{
	err.clear();
	QByteArray qs = mess.toLatin1();
	const char *s = qs.constData();
	s = strstr(s,"in line ");
	while(s)
	{
		err.push_back(atoi(s+8));
		s = strstr(s+8,"in line ");
	}
}
//-----------------------------------------------------------------------------
LineNumberArea::LineNumberArea(TextEdit *editor) : QWidget(editor)
{	codeEditor = editor;	}
//-----------------------------------------------------------------------------
QSize LineNumberArea::sizeHint() const
{	return QSize(codeEditor->lineNumberAreaWidth(), 0);	}
//-----------------------------------------------------------------------------
void LineNumberArea::paintEvent(QPaintEvent *event)
{	codeEditor->lineNumberAreaPaintEvent(event);	}
//-----------------------------------------------------------------------------
