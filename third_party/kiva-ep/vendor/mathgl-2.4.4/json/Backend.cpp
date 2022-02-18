#include <QMessageBox>
#include <QTextStream>
#include <QFile>
#include <QDebug>
#include "Backend.hpp"
#include <mgl2/mgl.h>
#undef sprintf	// fix libintl bug of defining sprintf
//-----------------------------------------------------------------------------
Backend::Backend(QObject *parent) : QObject(parent) { }
//-----------------------------------------------------------------------------
QString Backend::show(const QString& text) const
{
	static size_t counter=size_t(0xffffffff*mgl_rnd());
	char tmp[256];
	snprintf(tmp,256,"%s/mathgl%lu.json", P_tmpdir, counter);
	tmp[255]=0;	counter++;
	qDebug() << __FUNCTION__;
	wchar_t *wtext;
	mglGraph gr;
	gr.SetFaceNum(200);
	mglParse pr;
	pr.AllowSetSize(true);
	setlocale(LC_ALL, "");	setlocale(LC_NUMERIC, "C");
	wtext = new wchar_t[text.size()+1];
	text.toWCharArray(wtext);
	wtext[text.size()] = 0;
	pr.Execute(&gr,wtext);
	delete[] wtext;
	gr.WriteJSON(tmp);
	setlocale(LC_NUMERIC, "");

	QFile f(tmp);
	f.open(QIODevice::ReadOnly);
	QTextStream ts(&f);
	ts.setAutoDetectUnicode(true);
	const QString json = ts.readAll();
	f.remove();
	return json;
}
//-----------------------------------------------------------------------------
QString Backend::coor(const QString& xy, const QString& text) const
{
	wchar_t *wtext;
	qDebug() << __FUNCTION__;
	mglGraph gr;
	mglParse pr;
	pr.AllowSetSize(true);
	setlocale(LC_ALL, "");	setlocale(LC_NUMERIC, "C");
	wtext = new wchar_t[text.size()+1];
	text.toWCharArray(wtext);
	wtext[text.size()] = 0;
	pr.Execute(&gr,wtext);
	delete[] wtext;
	gr.Finish();

	int x = (int)xy.section(" ",0,0).toDouble();
	int y = (int)xy.section(" ",1,1).toDouble();
	mglPoint p = gr.CalcXYZ(x,y);
	QString res;
	res.sprintf("x = %g, y = %g, z = %g for point (%d, %d)\n", p.x, p.y, p.z, x,y);
	qDebug() << res+"\nask"+xy;
	return res+"\nask"+xy;
}
//-----------------------------------------------------------------------------
QString Backend::geometry(const QString& mgl) const
{
	qDebug() << __FUNCTION__;
	char tmp[256];
	static size_t counter=size_t(0xffffffff*mgl_rnd());
	snprintf(tmp,256,"%s/mathgl%lu.json", P_tmpdir, counter);
	tmp[255]=0;	counter++;
	wchar_t *wmgl;
	mglGraph gr;
#if 0
	gr.SetFaceNum(200);
#endif
	mglParse pr;
	pr.AllowSetSize(true);
	setlocale(LC_ALL, "");	setlocale(LC_NUMERIC, "C");
	wmgl = new wchar_t[mgl.size()+1];
	mgl.toWCharArray(wmgl);
	wmgl[mgl.size()] = 0;
	pr.Execute(&gr,wmgl);
	delete[] wmgl;
	gr.WriteJSON(tmp);
	setlocale(LC_NUMERIC, "");

	QFile f(tmp);
	f.open(QIODevice::ReadOnly);
	QTextStream ts(&f);
	ts.setAutoDetectUnicode(true);
	const QString json = ts.readAll();
	f.remove();
	return json;
}
//-----------------------------------------------------------------------------
