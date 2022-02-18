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
#include <QPrintDialog>
#include <QMessageBox>
#include <QTextStream>
#include <QFileDialog>
#include <QToolButton>
#include <QToolBar>
#include <QCompleter>
#include <QBoxLayout>
#include <QPrinter>
#include <QMenu>
#include "mgl2/qmathgl.h"
//-----------------------------------------------------------------------------
#include "udav_wnd.h"
#include "qmglsyntax.h"
#include "find_dlg.h"
#include "opt_dlg.h"
#include "style_dlg.h"
#include "files_dlg.h"
#include "newcmd_dlg.h"
#include "setup_dlg.h"
#include "text_pnl.h"
#include "plot_pnl.h"
#include "subplot_dlg.h"
//-----------------------------------------------------------------------------
FilesDialog *files_dlg=0;
QString defFontFamily;
int defFontSize;
bool mglAutoExecute = true;
extern mglParse parser;
extern bool mglCompleter;
QWidget *createDataOpenDlg(QWidget *p);
QString getOpenDataFile(QWidget *w, QString filename);
//-----------------------------------------------------------------------------
TextPanel::TextPanel(QWidget *parent) : QWidget(parent)
{
	printer = new QPrinter;
	findDialog = new FindDialog(this);
	optDialog = new OptionDialog(this);
	stlDialog = new StyleDialog(this);
	newCmdDlg = new NewCmdDialog(this);
	subplotDlg = new SubplotDialog(this);
	setupDlg = new SetupDialog(this);
	dataOpenDlg = createDataOpenDlg(this);
	if(!files_dlg)	files_dlg= new FilesDialog;

	int n=parser.GetCmdNum();
	for(int i=0;i<n;i++) 	words<<QString::fromLatin1(parser.GetCmdName(i));
	vars = words;

	connect(setupDlg, SIGNAL(putText(const QString &)), this, SLOT(animPutText(const QString &)));
	connect(newCmdDlg, SIGNAL(result(const QString&, bool)), this, SLOT(putLine(const QString&, bool)));
	connect(subplotDlg, SIGNAL(result(const QString&)), this, SLOT(putLine(const QString&)));
	connect(findDialog, SIGNAL(findText(const QString &, bool, bool)), this, SLOT(findText(const QString &, bool, bool)));
	connect(findDialog, SIGNAL(replText(const QString &, const QString &, bool, bool)), this, SLOT(replText(const QString &, const QString &, bool, bool)));

	edit = new TextEdit(this);	edit->setAcceptRichText(false);
	new QMGLSyntax(edit);
	defFontFamily = edit->fontFamily();
	defFontSize = int(edit->fontPointSize());
	edit->setLineWrapMode(QTextEdit::NoWrap);
	setCompleter(mglCompleter);
	QFontMetrics metrics(edit->currentFont());
	edit->setTabStopWidth(4 * metrics.width(' '));

	menu = new QMenu(_("Edit"),this);
	QBoxLayout *v = new QVBoxLayout(this);
	toolTop(v);	v->addWidget(edit);
}
//-----------------------------------------------------------------------------
TextPanel::~TextPanel()	{	delete printer;	}
//-----------------------------------------------------------------------------
void TextPanel::setCompleter(bool en)
{
	if(en)
	{
		QCompleter *completer = new QCompleter(vars, this);
		completer->setCaseSensitivity(Qt::CaseInsensitive);
		completer->setCompletionMode(QCompleter::PopupCompletion);
		edit->setCompleter(completer);
	}
	else	edit->setCompleter(0);
//	completer->setCompletionMode(en ? QCompleter::PopupCompletion : QCompleter::InlineCompletion);
}
//-----------------------------------------------------------------------------
void TextPanel::insNVal()
{
	QString sel=edit->textCursor().selectedText();
	if(sel.isEmpty())
	{
		QMessageBox::warning(this,_("UDAV"),_("There is no selection to evaluate."));
		return;
	}
	wchar_t *txt=new wchar_t[sel.length()+1];
	sel.toWCharArray(txt);	txt[sel.length()]=0;
	mglData res=parser.Calc(txt);
	delete []txt;
	edit->textCursor().insertText(QString::number(res.GetVal(0)));
}
//-----------------------------------------------------------------------------
void TextPanel::insPrim()
{
	QString str(graph->mgl->primitives);
	if(str.isEmpty())
	{
		QMessageBox::warning(this,_("UDAV"),_("There is manual primitives."));
		return;
	}
	edit->moveCursor(QTextCursor::Start);
	edit->insertPlainText("subplot 1 1 0 '#'\n"+str+"subplot 1 1 0\n#----------\n");
	graph->mgl->primitives = "";
}
//-----------------------------------------------------------------------------
void TextPanel::insFitF()
{
	QString str(graph->getFit());
	if(str.isEmpty())
	{
		QMessageBox::warning(this,_("UDAV"),_("There is no fitted formula."));
		return;
	}
	edit->textCursor().insertText("'"+str+"'");
}
//-----------------------------------------------------------------------------
void TextPanel::insFile()
{
	QString str = QFileDialog::getOpenFileName(this, _("UDAV - Insert filename"));
	if(str.isEmpty())	return;
	edit->textCursor().insertText("'"+str+"'");
}
//-----------------------------------------------------------------------------
void TextPanel::insPath()
{
	QString str = QFileDialog::getExistingDirectory(this, _("UDAV - Insert path"));
	if(str.isEmpty())	return;
	edit->textCursor().insertText("'"+str+"'");
}
//-----------------------------------------------------------------------------
void TextPanel::refreshData()
{
	vars=words;
	long i,n=parser.GetNumVar();
	for(i=0;i<n;i++)
	{
		const mglDataA *v=parser.GetVar(i);
		if(v && wcslen(v->Name())>2)	vars<<QString::fromWCharArray(v->Name());
	}
	setCompleter(mglCompleter);
}
//-----------------------------------------------------------------------------
void TextPanel::printText()
{
	QPrintDialog printDlg(printer, this);
	if (printDlg.exec() == QDialog::Accepted)
	{
		setStatus(_("Printing..."));
		edit->print(printer);
		setStatus(_("Printing completed"));
	}
	else	setStatus(_("Printing aborted"));
}
//-----------------------------------------------------------------------------
void TextPanel::find()
{
	findDialog->show();
	findDialog->raise();
	findDialog->activateWindow();
}
//-----------------------------------------------------------------------------
bool TextPanel::findText(const QString &str, bool cs, bool fw)
{
//	static int para=0, index=0;
	static QTextDocument::FindFlags f;
	static QString stri="";
	if(!str.isEmpty())
	{
		stri = str;
		f = QTextDocument::FindFlags();
		if(fw)	f = f|QTextDocument::FindBackward;
		if(cs)	f = f|QTextDocument::FindCaseSensitively;
	}
	bool res = edit->find(stri, f);
	if(!res)
		QMessageBox::information(this, _("UDAV - find text"), _("No string occurrence is found"));
	return res;
}
//-----------------------------------------------------------------------------
void TextPanel::replText(const QString &str, const QString &txt, bool cs, bool fw)
{
	static bool res=false;
	if(str.isEmpty())	{	res = false;	return;	}
	if(res)	edit->textCursor().insertText(txt);
	res = findText(str, cs, fw);
}
//-----------------------------------------------------------------------------
void TextPanel::addOptions()
{
	if(optDialog->exec()==QDialog::Accepted)
	{
		edit->moveCursor(QTextCursor::EndOfLine);
		edit->insertPlainText(optDialog->getOption());
	}
}
//-----------------------------------------------------------------------------
void TextPanel::animPutText(const QString &s)
{	edit->moveCursor(QTextCursor::Start);	edit->insertPlainText(s);	}
//-----------------------------------------------------------------------------
//void TextPanel::putText(const QString &txt)	{	edit->insertPlainText(txt);	}
//-----------------------------------------------------------------------------
void TextPanel::putLine(const QString &txt, bool replace)
{
	edit->moveCursor(QTextCursor::StartOfLine);
	if(replace)
	{
		QTextCursor c = edit->textCursor();
		c.select(QTextCursor::BlockUnderCursor);
		c.removeSelectedText();
		edit->setTextCursor(c);
		if(c.atStart())	edit->insertPlainText(txt);
		else	edit->insertPlainText("\n"+txt);
	}
	else	edit->insertPlainText(txt+"\n");
}
//-----------------------------------------------------------------------------
void TextPanel::addStyle()
{
	if(stlDialog->exec()==QDialog::Accepted)
	{
		QString s = edit->textCursor().block().text();
		int i = s.indexOf(';');
		if(i<0)	edit->moveCursor(QTextCursor::EndOfLine);
		else
		{
			edit->moveCursor(QTextCursor::StartOfBlock);
			// foolish way :(
			for(;i>0;i--)	edit->moveCursor(QTextCursor::Left);
		}
		edit->insertPlainText(stlDialog->getStyle());
	}
}
//-----------------------------------------------------------------------------
void TextPanel::setEditorFont(QFont *f)
{
	QFont d(defFontFamily, defFontSize);
	edit->setFont(f ? *f : d);
	QFontMetrics metrics(f ? *f : d);
	edit->setTabStopWidth(4 * metrics.width(' '));
}
//-----------------------------------------------------------------------------
QString TextPanel::selection()
{	return edit->textCursor().block().text();	}
//-----------------------------------------------------------------------------
void TextPanel::setCursorPosition(int n)
{
	if(n<0)	return;
	edit->moveCursor(QTextCursor::Start);
	for(int i=0;i<n;i++)	edit->moveCursor(QTextCursor::NextBlock);
	edit->setFocus();
}
//-----------------------------------------------------------------------------
void TextPanel::newCmd(int n)
{
	if(n>0)	setCursorPosition(n-1);
	else if(n==0)	return;
	newCmdDlg->parseCmd(edit->textCursor().block().text());
	newCmdDlg->show();
}
//-----------------------------------------------------------------------------
#if MGL_HAVE_HDF5
#define H5_USE_16_API
#include <hdf5.h>
void TextPanel::loadHDF5(const QString &fileName)
{
	// H5T_C_S1 - C string
	hid_t hf,hg,hd,hs,ht;
	hsize_t dims[3];
	long rank;
	hf = H5Fopen(fileName.toLocal8Bit().constData(), H5F_ACC_RDONLY, H5P_DEFAULT);
	if(!hf)	return;
	hg = H5Gopen(hf, "/");
	hsize_t num, nx, ny, nz, i;
	char name[256];
	H5Gget_num_objs(hg, &num);
	for(i=0;i<num;i++)
	{
		if(H5Gget_objtype_by_idx(hg, i)!=H5G_DATASET)	continue;
		H5Gget_objname_by_idx(hg, i, name, 256);
		hd = H5Dopen(hg,name);	hs = H5Dget_space(hd);
		ht = H5Dget_type(hd);
		rank = H5Sget_simple_extent_ndims(hs);
		if(H5Tget_class(ht)==H5T_STRING)	// load script
		{
			H5Sget_simple_extent_dims(hs,dims,0);
			char *buf = new char[dims[0]+1];
			H5Dread(hd, H5T_C_S1, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
			buf[dims[0]]=0;		// to be sure :)
			QString str = buf;
			if(str.contains("subplot 1 1 0\n#----- End of QMathGL block -----\n"))
			{
				graph->mgl->primitives = str.section("subplot 1 1 0\n#----- End of QMathGL block -----\n",0,0).section("subplot 1 1 0 '#'\n",1);
				str = str.section("subplot 1 1 0\n#----- End of QMathGL block -----\n",1);
			}
			edit->setText(str);
			graph->animParseText(edit->toPlainText());
			setCurrentFile(fileName);
			delete []buf;
			setStatus(QString(_("Loaded document %1")).arg(fileName));
			if(mglAutoExecute)	graph->execute();
		}
		else if(H5Tget_class(ht)==H5T_FLOAT || H5Tget_class(ht)==H5T_INTEGER)
		{
			for(int j=0;name[j];j++)	if(!isalnum(name[j]))	name[j]='_';
			mglData *v = (mglData*) parser.AddVar(name);
			nx = ny = nz = 1;
			if(rank>0 && rank<=3)
			{
				H5Sget_simple_extent_dims(hs,dims,0);
				switch(rank)
				{
					case 1:	nx=dims[0];	break;
					case 2:	nx=dims[1];	ny=dims[0];	break;
					case 3:	nx=dims[2];	ny=dims[1];	nz=dims[0];	break;
				}
				v->Create(nx, ny, nz);
#if MGL_USE_DOUBLE
				H5Dread(hd, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, v->a);
#else
				H5Dread(hd, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, v->a);
#endif
			}
		}
		H5Dclose(hd);	H5Sclose(hs);	H5Tclose(ht);
	}
	H5Gclose(hg);	H5Fclose(hf);
}
//-----------------------------------------------------------------------------
void TextPanel::saveHDF5(const QString &fileName)
{
	hid_t hf,hd,hs;
	hsize_t dims[3];
	long rank = 3;

	H5Eset_auto(0,0);
	hf = H5Fcreate(fileName.toLocal8Bit().constData(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if(hf<0)
	{
		setStatus(QString(_("Could not write to %1")).arg(fileName));
		return;
	}
	{	// save script
		QString txt;
		if(!graph->mgl->primitives.isEmpty())
			txt += "subplot 1 1 0 '#'\n"+graph->mgl->primitives + "subplot 1 1 0\n#----- End of QMathGL block -----\n";
		txt += edit->toPlainText();
		dims[0] = txt.length()+1;
		char *buf = new char[dims[0]+1];
		memcpy(buf, txt.toLocal8Bit().constData(), dims[0]);
		buf[dims[0]]=0;
		hs = H5Screate_simple(1, dims, 0);
		hd = H5Dcreate(hf, "mgl_script", H5T_C_S1, hs, H5P_DEFAULT);
		H5Dwrite(hd, H5T_C_S1, hs, hs, H5P_DEFAULT, buf);
		H5Dclose(hd);	H5Sclose(hs);
		delete []buf;
	}
	long n = parser.GetNumVar();
	char name[256];
	for(long i=0;i<n;i++)
	{
		const mglData *v = dynamic_cast<const mglData *>(parser.GetVar(i));
		mglData tmp;
		if(!v)	{	tmp.Set(parser.GetVar(i));	v = &tmp;	}
		wcstombs(name,v->Name(),wcslen(v->Name())+1);
		if(v->nz==1 && v->ny == 1)
		{	rank = 1;	dims[0] = v->nx;	}
		else if(v->nz==1)
		{	rank = 2;	dims[0] = v->ny;	dims[1] = v->nx;	}
		else
		{	rank = 3;	dims[0] = v->nz;	dims[1] = v->ny;	dims[2] = v->nx;	}
		hs = H5Screate_simple(rank, dims, 0);
		hd = H5Dcreate(hf, name, H5T_IEEE_F32LE, hs, H5P_DEFAULT);

		H5Dwrite(hd, H5T_NATIVE_FLOAT, hs, hs, H5P_DEFAULT, v->a);
		H5Dclose(hd);	H5Sclose(hs);
	}
	H5Fclose(hf);
	setCurrentFile(fileName);
	setStatus(QString(_("File %1 saved")).arg(fileName));
	return;
}
#else
void TextPanel::saveHDF5(const QString &fileName){}
void TextPanel::loadHDF5(const QString &fileName){}
#endif
//-----------------------------------------------------------------------------
void TextPanel::load(const QString &fileName)
{
	if(fileName.right(4).toLower()==".dat")
	{
		QString code = getOpenDataFile(dataOpenDlg, fileName);
		if(!code.isEmpty())
		{
			setCurrentFile(fileName.left(fileName.length()-3)+"mgl");
			edit->setText(code);
		}
	}
	else if(fileName.right(4).toLower()==".hdf" || fileName.right(3).toLower()==".h5")
		loadHDF5(fileName);
	else
	{
		QFile f(fileName);
		if(!f.open(QIODevice::ReadOnly))
		{
			QMessageBox::warning(this,_("UDAV - open file"), _("Couldn't open file ") + QString("'") + fileName+"'", QMessageBox::Ok,0,0);
			return;
		}

		QTextStream ts(&f);
		ts.setAutoDetectUnicode(true);
//		ts.setCodec(QTextCodec::codecForLocale());

		QString str=ts.readAll();
		int narg=0,i=-1;
		if(str.contains('%'))
		{
			while((i = str.indexOf('%',i+1))>0)
			{
				char ch = str.at(i+1).toLatin1();
				if(ch>='1' && ch<='9' && ch-'0'>narg)
					narg = ch-'0';
			}
			if(narg>0)
			{
				files_dlg->setNumFiles(narg);
				if(!files_dlg->exec())	return;	// nothing to do
				str = files_dlg->putFiles(str);
			}
		}
		if(str.contains("#----- End of QMathGL block -----\n"))
		{
			graph->mgl->primitives = str.section("#----- End of QMathGL block -----\n",0,0);
			str = str.section("#----- End of QMathGL block -----\n",1);
		}

		if(narg>0)	setCurrentFile(fileName.left(fileName.length()-3)+"mgl");
		edit->setText(str);
		graph->animParseText(edit->toPlainText());
		if(narg==0)	setCurrentFile(fileName);
	}
	setStatus(_("Loaded document ")+fileName);
	if(mglAutoExecute)	graph->execute();
}
//-----------------------------------------------------------------------------
void TextPanel::save(const QString &fileName)
{
	if(fileName.right(4)==".hdf" || fileName.right(3)==".h5")
	{	saveHDF5(fileName);	return;	}
	QString text;
	if(!graph->mgl->primitives.isEmpty())
		text += graph->mgl->primitives + "#----- End of QMathGL block -----\n";
	text += edit->toPlainText();
	QFile f(fileName);
	if(!f.open(QIODevice::WriteOnly))
	{
		setStatus(QString(_("Could not write to %1")).arg(fileName));
		return;
	}
	QTextStream t(&f);
	t.setAutoDetectUnicode(true);
	t << text;	f.close();
	setCurrentFile(fileName);
	setStatus(QString(_("File %1 saved")).arg(fileName));
}
//-----------------------------------------------------------------------------
void TextPanel::addSetup()	{	setupDlg->exec();	}
//-----------------------------------------------------------------------------
#include "xpm/option.xpm"
#include "xpm/style.xpm"
#include "xpm/curve.xpm"
#include "xpm/box.xpm"
//-----------------------------------------------------------------------------
void TextPanel::toolTop(QBoxLayout *v)
{
	QToolBar *t = new QToolBar(this);	v->addWidget(t);	t->setMovable(false);
	QAction *a, *aa;
	QMenu *o=menu, *oo;
	const MainWindow *mw=findMain(this);

	// general buttons
	if(mw)
	{
		t->addAction(mw->aload);	t->addAction(mw->asave);	t->addAction(mw->acalc);
	}
	QToolButton *bb = new QToolButton(this);
	bb->setPopupMode(QToolButton::MenuButtonPopup);
	t->addWidget(bb);

	// edit menu
	a = new QAction(QPixmap(":/png/edit-undo.png"), _("Undo"), this);
	connect(a, SIGNAL(triggered()), edit, SLOT(undo()));
	a->setToolTip(_("Undo editor change (Ctrl+Z)."));
	a->setShortcut(Qt::CTRL+Qt::Key_Z);	o->addAction(a);	t->addAction(a);

	a = new QAction(QPixmap(":/png/edit-redo.png"), _("Redo"), this);
	connect(a, SIGNAL(triggered()), edit, SLOT(redo()));
	a->setToolTip(_("Redo editor change (Ctrl+Shift+Z)."));
	a->setShortcut(Qt::CTRL+Qt::SHIFT+Qt::Key_Z);	o->addAction(a);	t->addAction(a);

	o->addSeparator();
	o->addAction(_("Clear all"), edit, SLOT(clear()));
	a = new QAction(QPixmap(":/png/edit-cut.png"), _("Cut text"), this);
	connect(a, SIGNAL(triggered()), edit, SLOT(cut()));
	a->setToolTip(_("Cut selected text to clipboard (Ctrl+X)."));
	a->setShortcut(Qt::CTRL+Qt::Key_X);	o->addAction(a);	t->addAction(a);

	a = new QAction(QPixmap(":/png/edit-copy.png"), _("Copy text"), this);
	connect(a, SIGNAL(triggered()), edit, SLOT(copy()));
	a->setToolTip(_("Copy selected text or data to clipboard (Ctrl+C)."));
	a->setShortcut(Qt::CTRL+Qt::Key_C);	o->addAction(a);	t->addAction(a);

	a = new QAction(QPixmap(":/png/edit-paste.png"), _("Paste text"), this);
	connect(a, SIGNAL(triggered()), edit, SLOT(paste()));
	a->setToolTip(_("Paste text or data from clipboard (Ctrl+V)."));
	a->setShortcut(Qt::CTRL+Qt::Key_V);	o->addAction(a);	t->addAction(a);

	o->addAction(QPixmap(":/png/edit-select-all.png"), _("Select all"), edit, SLOT(selectAll()), Qt::CTRL+Qt::Key_A);
	o->addSeparator();

	a = new QAction(QPixmap(":/png/edit-find.png"), _("Find/Replace"), this);
	connect(a, SIGNAL(triggered()), this, SLOT(find()));
	a->setToolTip(_("Show dialog for text finding (Ctrl+F)."));
	a->setShortcut(Qt::CTRL+Qt::Key_F);	o->addAction(a);	t->addAction(a);

	a = new QAction(_("Find next"), this);
	connect(a, SIGNAL(triggered()), this, SLOT(findText()));
	a->setShortcut(Qt::Key_F3);	o->addAction(a);
	o->addSeparator();

	// insert menu
	oo = o->addMenu(_("Insert"));
	aa=a = new QAction(QPixmap(":/png/format-indent-more.png"), _("New command"), this);
	a->setShortcut(Qt::META+Qt::Key_C);	connect(a, SIGNAL(triggered()), this, SLOT(newCmd()));
	a->setToolTip(_("Show dialog for new command or edit arguments of existed one."));
	oo->addAction(a);
	a = new QAction(QPixmap(box_xpm), _("New inplot"), this);
	a->setShortcut(Qt::META+Qt::Key_C);	connect(a, SIGNAL(triggered()), subplotDlg, SLOT(show()));
	a->setToolTip(_("Show dialog for new inplot and put it into the script."));
	oo->addAction(a);

	a = new QAction(_("Fitted formula"), this);
	a->setShortcut(Qt::META+Qt::Key_F);	connect(a, SIGNAL(triggered()), this, SLOT(insFitF()));
	a->setToolTip(_("Insert last fitted formula with found coefficients."));
	oo->addAction(a);
	a = new QAction(QPixmap(style_xpm), _("Plot style"), this);
	a->setShortcut(Qt::META+Qt::Key_S);	connect(a, SIGNAL(triggered()), this, SLOT(addStyle()));
	a->setToolTip(_("Show dialog for styles and put it into the script.\nStyles define the plot view (color scheme, marks, dashing and so on)."));
	oo->addAction(a);
	a = new QAction(QPixmap(option_xpm), _("Command options"), this);
	a->setShortcut(Qt::META+Qt::Key_O);	connect(a, SIGNAL(triggered()), this, SLOT(addOptions()));
	a->setToolTip(_("Show dialog for options and put it into the script.\nOptions are used for additional setup the plot."));
	oo->addAction(a);
	a = new QAction(_("Numeric value"), this);
	a->setShortcut(Qt::META+Qt::Key_N);	connect(a, SIGNAL(triggered()), this, SLOT(insNVal()));
	a->setToolTip(_("Replace expression by its numerical value."));
	oo->addAction(a);
	a = new QAction(QPixmap(":/png/text-csv.png"), _("File name"), this);
	a->setShortcut(Qt::META+Qt::Key_P);	connect(a, SIGNAL(triggered()), this, SLOT(insFile()));
	a->setToolTip(_("Select and insert file name."));
	oo->addAction(a);
	a = new QAction(QPixmap(":/png/folder.png"), _("Folder path"), this);
	connect(a, SIGNAL(triggered()), this, SLOT(insPath()));
	a->setToolTip(_("Select and insert folder name."));
	oo->addAction(a);
	a = new QAction(QPixmap(curve_xpm), _("Manual primitives"), this);
	a->setShortcut(Qt::META+Qt::Key_P);	connect(a, SIGNAL(triggered()), this, SLOT(insPrim()));
	a->setToolTip(_("Move mouse-handled primitives to script."));
	oo->addAction(a);	bb->setMenu(oo);	bb->setDefaultAction(aa);

	a = new QAction(QPixmap(":/png/document-properties.png"), _("Graphics setup"), this);
	a->setShortcut(Qt::META+Qt::Key_G);	connect(a, SIGNAL(triggered()), this, SLOT(addSetup()));
	a->setToolTip(_("Show dialog for plot setup and put code into the script.\nThis dialog setup axis, labels, lighting and other general things."));
	o->addAction(a);	t->addAction(a);
}
//-----------------------------------------------------------------------------
