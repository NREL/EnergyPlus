/***************************************************************************
 * qt.cpp is part of Math Graphic Library                              *
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Library General Public License as       *
 *   published by the Free Software Foundation; either version 3 of the    *
 *   License, or (at your option) any later version.                       *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU Library General Public     *
 *   License along with this program; if not, write to the                 *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
#include <QTimer>
#include <QApplication>
#include <QMouseEvent>
#include <QMessageBox>
#include <QMenu>
#include <QClipboard>
#include <QTextEdit>
#include <QPainter>
#include <QCursor>
#include <QImage>
#include <QScrollArea>
#include <QMainWindow>
#include <QToolBar>
#include <QMenuBar>
#include <QSpinBox>
#include <QPrinter>
#include <QPrintDialog>
#include <QFileDialog>
#include <QInputDialog>
#include <QThread>
#include <limits.h>

#include "mgl2/canvas_wnd.h"
#include "mgl2/qmathgl.h"
#include "mgl2/qt.h"
#undef sprintf	// fix libintl bug of defining sprintf
//-----------------------------------------------------------------------------
#define MGL_MAX_LINES	(INT_MAX-1000)
//-----------------------------------------------------------------------------
/// Base class for windows containing MathGL graphics
class mglCanvasQT : public mglCanvasWnd
{
public:
using mglCanvasWnd::Window;
	int sshow;		///< Current state of animation switch (toggle button)
	QMathGL *QMGL;	///< Control which draw graphics
	QMainWindow *Wnd;	///< Pointer to window

	mglCanvasQT();
    virtual ~mglCanvasQT();

	/// Create a window for plotting. Now implemeted only for GLUT.
	void Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p),const char *title,
						void *par=NULL, void (*reload)(void *p)=NULL, bool maximize=false);
	/// Switch on/off transparency (do not overwrite switches in user drawing function)
	void ToggleAlpha();
	/// Switch on/off lighting (do not overwrite switches in user drawing function)
	void ToggleLight();
	void ToggleRotate();///< Switch on/off rotation by mouse
	void ToggleZoom();	///< Switch on/off zooming by mouse
	void ToggleNo();	///< Switch off all zooming and rotation
	void Update();		///< Update picture by calling user drawing function
	void Adjust();		///< Adjust size of bitmap to window size
	void GotoFrame(int d);	///< Show arbitrary frame (use relative step)
	void Animation();		///< Run slideshow (animation) of frames
	void MakeDialog(const char *ids, char const * const *args, const char *title=""){}	// TODO
	void *Window()	{return Wnd;}	///< Return pointer to widget (QMainWindow*) used for plotting
	void *Widget()	{return QMGL;}	///< Return pointer to widget (QMathGL*) used for plotting
	void WndSize(int w, int h)	{	Wnd->resize(w,h);	}	///< Resize window
	void WndMove(int x, int y)	{	Wnd->move(x,y);	}	///< Move window


protected:
	QScrollArea *scroll;	///< Scrolling area
	QMenu *popup;			///< Popup menu
	QSpinBox *tet, *phi;	///< Spin box for angles
};
//-----------------------------------------------------------------------------
void MGL_EXPORT mgl_ask_qt(const wchar_t *quest, wchar_t *res)
{	QInputDialog::getText(QApplication::activeWindow(), "MathGL",
						QString::fromWCharArray(quest)).toWCharArray(res);	}
//-----------------------------------------------------------------------------
/*void mglTask::doWork()
{
	setlocale(LC_NUMERIC, "C");
	if(mgl_is_frames(gr))	mgl_new_frame(gr);
	if(draw_func)	draw_func(gr, draw_par);
	else if(draw)	{	mglGraph g(gr);	draw->Draw(&g);	}
	if(mgl_is_frames(gr))	mgl_end_frame(gr);
	setlocale(LC_NUMERIC, "");
	gr->Finish();
	emit plotDone();
}*/
//-----------------------------------------------------------------------------
QMathGL::QMathGL(QWidget *parent, Qt::WindowFlags f) : QWidget(parent, f)
{
	autoResize = false;	draw_par = 0;	draw_func = 0;
	dotsRefr = true;
	gr = new mglCanvas;	appName = "MathGL";
	setMinimumSize(gr->GetWidth(),gr->GetHeight());
	popup = 0;	grBuf = 0;	draw = 0;	prevQuality=MGL_DRAW_NORM;
	phi = tet = per = 0;	x0=y0=xe=ye=0;
	x1 = y1 = ax1 = ay1 = 0;	x2 = y2 = ax2 = ay2 = 1;
	alpha = light = zoom = rotate = grid = viewYZ = custZoom = custDraw = pause = false;
	resize(600, 400);	mgl_set_flag(gr, true, MGL_CLF_ON_UPD);
	timer = new QTimer(this);
	timerRefr = new QTimer(this);	timerRefr->setInterval(100);
	timerRefr->setSingleShot(true);
	enableWheel = enableMouse = true;
	connect(timer, SIGNAL(timeout()), this, SLOT(nextSlide()));
	connect(timerRefr, SIGNAL(timeout()), this, SLOT(refreshHQ()));
	sclS = 0.25;	sclZ = 0.5;

/*	thread = new QThread();
	task = new mglTask();	task->moveToThread(thread);
	connect(thread, SIGNAL(started()), task, SLOT(doWork()));
	connect(task, SIGNAL(plotDone()), thread, SLOT(quit()));
	connect(thread, SIGNAL(finished()), this, SLOT(afterPlot()));*/
}
//-----------------------------------------------------------------------------
QMathGL::~QMathGL()
{
	timer->stop();	timerRefr->stop();
	if(gr && mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);
	if(grBuf)	delete []grBuf;
	if(draw)	delete draw;
}
//-----------------------------------------------------------------------------
void QMathGL::setDotsPreview(bool d)
{	dotsRefr = d;	}
//-----------------------------------------------------------------------------
void QMathGL::setDraw(int (*func)(mglBase *gr, void *par), void *par)
{
	if(draw)	delete draw;
	draw = 0;	draw_func = func;	draw_par = par;
	emit usePrimChanged(draw_func || draw);
}
//-----------------------------------------------------------------------------
void QMathGL::setDraw(mglDraw *dr)
{
	if(draw)	delete draw;
	draw = dr;	draw_func = 0;
	emit usePrimChanged(draw_func || draw);
}
//-----------------------------------------------------------------------------
double QMathGL::getRatio()	{	return double(mgl_get_width(gr))/mgl_get_height(gr);	}
void mgl_qt_event_func(void *)	{	QApplication::processEvents();	}
//-----------------------------------------------------------------------------
void QMathGL::setGraph(HMGL GR)	///< Set grapher object
{
	mglCanvas *gg = dynamic_cast<mglCanvas *>(GR);
	if(!gg)	return;
	if(mgl_use_graph(gr,-1)<1)	mgl_delete_graph(gr);
	gr=gg;	mgl_use_graph(gg,1);
	gr->SetEventFunc(mgl_qt_event_func, NULL);
	setMinimumSize(gr->GetWidth(),gr->GetHeight());
}
//-----------------------------------------------------------------------------
void QMathGL::paintEvent(QPaintEvent *)
{
	QPainter paint;
	paint.begin(this);
	paint.drawPixmap(0,0,pic);
	if(zoom)	paint.drawRect(x0,y0,xe-x0,ye-y0);
	if(mgl_get_flag(gr,MGL_SHOW_POS) && !mousePos.isEmpty())
		paint.drawText(0,12,mousePos);
	if(grid)
	{
		long d, h=pic.height(), w=pic.width();
		paint.setPen(QColor(192,192,192));
		for(long i=1;i<10;i++)
		{
			paint.drawText(0,i*h/10,QString::number(1-i*0.1));
			paint.drawLine(0,i*h/10,w,i*h/10);
			paint.drawText(i*w/10,h,QString::number(i*0.1));
			paint.drawLine(i*w/10,0,i*w/10,h);
		}
		paint.setPen(QColor(0,0,0));
		d = (h>w?w:h)/100;
		if(mgl_is_frames(gr))
			for(size_t i=0;i<gr->Act.size();i++)
			{
				const mglActivePos &p=gr->Act[i];
				QRect rf(p.x-d/2,p.y-d/2-1,d,d);
				paint.drawRect(rf);
				paint.fillRect(rf,QBrush(QColor(127,255,63)));
			}
	}
	paint.end();
}
//-----------------------------------------------------------------------------
void QMathGL::resizeEvent(QResizeEvent *ev)
{
	if(autoResize && ev->size().width()>0 && ev->size().height()>0)
	{	mgl_set_size(gr, ev->size().width(), ev->size().height());	update();	}
//	else	resize(graph->GetWidth(), graph->GetHeight());
}
//-----------------------------------------------------------------------------
void QMathGL::setPer(int p)
{
	if(per!=p && p>=0 && p<100)
	{	per = 100*p;	emit perChanged(p);	refresh();	}
}
//-----------------------------------------------------------------------------
void QMathGL::setPhi(int p)
{	if(phi!=p)	{	phi = p;	emit phiChanged(p);	refresh();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setTet(int t)
{	if(tet!=t)	{	tet = t;	emit tetChanged(t);	refresh();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setAlpha(bool a)
{	if(alpha!=a)	{	alpha = a;	emit alphaChanged(a);	update();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setLight(bool l)
{	if(light!=l)	{	light = l;	emit lightChanged(l);	update();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setGrid(bool g)
{	if(grid!=g)	{	grid = g;	emit gridChanged(g); 	refresh();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setViewYZ(bool a)
{	if(viewYZ!=a)	{	viewYZ = a;	emit viewYZChanged(a);	refresh();	}	}
//-----------------------------------------------------------------------------
void QMathGL::setPause(bool p)
{
#if MGL_HAVE_PTHR_WIDGET
	if(pause!=p)
	{
		pthread_mutex_t *mutex=0;
		mglCanvasWnd *g=dynamic_cast<mglCanvasWnd *>(gr);
		if(g && g->mutex)	mutex = g->mutex;
		else
		{
			mglDraw *d=getClass();
			if(d)	mutex = &(d->mutex);
		}
		if(mutex)
		{
			if(p)	pthread_mutex_lock(mutex);
			else	pthread_mutex_unlock(mutex);
		}
		pause=p;
		emit pauseChanged(p);
	}
#endif
}
//-----------------------------------------------------------------------------
void QMathGL::setRotate(bool r)
{
	if(rotate!=r)
	{	zoom=false;	rotate=r;	refresh();	emit rotateChanged(r);	}
}
//-----------------------------------------------------------------------------
void QMathGL::setZoom(bool z)
{
	if(zoom!=z)
	{	zoom=z;	rotate=false;	refresh();
	emit zoomChanged(z);	emit rotateChanged(false);	}
}
//-----------------------------------------------------------------------------
void QMathGL::setCustZoom(bool z)	{	custZoom = z;	}
//-----------------------------------------------------------------------------
void QMathGL::setCustDraw(bool z)	{	custDraw = z;	}
//-----------------------------------------------------------------------------
void QMathGL::setZoomScl(double s)	{	if(s>0)	sclZ = s;	}
//-----------------------------------------------------------------------------
void QMathGL::setShiftScl(double s)	{	if(s)	sclS = s;	}
//-----------------------------------------------------------------------------
void QMathGL::shiftDown()
{	mreal d=(y2-y1)*sclS;	y1+=d;	y2+=d;	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::shiftUp()
{	mreal d=(y2-y1)*sclS;	y1-=d;	y2-=d;	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::shiftRight()
{	mreal d=(x2-x1)*sclS;	x1-=d;	x2-=d;	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::shiftLeft()
{	mreal d=(x2-x1)*sclS;	x1+=d;	x2+=d;	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::zoomIn()
{
	mreal d,c;
	d = (y2-y1)*sclZ/2;	c = (y2+y1)/2;	y1 = c-d;	y2 = c+d;
	d = (x2-x1)*sclZ/2;	c = (x2+x1)/2;	x1 = c-d;	x2 = c+d;
	refresh();
}
//-----------------------------------------------------------------------------
void QMathGL::zoomOut()
{
	mreal d,c;
	d = (y2-y1)/sclZ/2;	c = (y2+y1)/2;	y1 = c-d;	y2 = c+d;
	d = (x2-x1)/sclZ/2;	c = (x2+x1)/2;	x1 = c-d;	x2 = c+d;
	refresh();
}
//-----------------------------------------------------------------------------
void QMathGL::restore()
{
	setPhi(0);	setTet(0);	setPer(0);
	x1=y1=0; 	x2=y2=1;		zoom=rotate=false;
	emit zoomChanged(false);	emit rotateChanged(false);
	if(ax1!=0 || ay1!=0 || ax2!=1 || ay2!=1)
	{
		ax1=ay1=0;		ax2=ay2=1;
		mgl_zoom_axis(gr,0,0,0,0,1,1,1,1);
		update();
	}
	else refresh();
}
//-----------------------------------------------------------------------------
void QMathGL::stop()	{	gr->AskStop(true);	}
//-----------------------------------------------------------------------------
void QMathGL::update()
{
	if(draw_func || draw)
	{
		mgl_reset_frames(gr);	// remove previous frames
		if(mgl_get_flag(gr,MGL_CLF_ON_UPD))	mgl_set_def_param(gr);
		mgl_set_alpha(gr,alpha);	mgl_set_light(gr,light);
		// use frames for quickly redrawing while adding/changing primitives
		if(custDraw)	emit customDraw(x1,y1,x2,y2,true);

		if(!isHidden())	QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
/*		task->gr = gr;	task->draw = draw;
		task->draw_func = draw_func;
		task->draw_par = draw_par;
		thread->start();*/
		const std::string loc = setlocale(LC_NUMERIC, "C");
		if(mgl_is_frames(gr))	mgl_new_frame(gr);
		if(draw_func)	draw_func(gr, draw_par);
		else if(draw)	{	mglGraph g(gr);	draw->Draw(&g);	}
		if(mgl_is_frames(gr))	mgl_end_frame(gr);
		setlocale(LC_NUMERIC, loc.c_str());
		gr->AskStop(false);
		setMinimumSize(gr->GetWidth(),gr->GetHeight());
	}
	else if(mgl_get_num_frame(gr)>0)
	{
		mgl_set_alpha(gr,alpha);	mgl_set_light(gr,light);
//		mgl_zoom(gr,x1,y1,x2,y2);	mgl_view(gr,-phi,-tet,0);
		mgl_get_frame(gr,0);
	}
	afterPlot();
}
//-----------------------------------------------------------------------------
void QMathGL::afterPlot()
{
	emit refreshData();
	emit showWarn(mgl_get_mess(gr));
	mousePos="";
	if(!isHidden())	QApplication::restoreOverrideCursor();
	refresh();
}
//-----------------------------------------------------------------------------
void QMathGL::drawPrim()
{
	if(!gr)	return;
	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	mgl_get_frame(gr, g?g->GetCurFig():mgl_get_num_frame(gr)-1);
	mglParse pr;
	long i, n=primitives.count('\n');
	mglGraph gg(gr);
	const std::string loc = setlocale(LC_NUMERIC, "C");
	gg.Push();	gg.SubPlot(1,1,0,"#");
	mglPoint ox1=gr->Min, ox2=gr->Max;
	gg.SetRanges(mglPoint(-1,-1,-1),mglPoint(1,1,1));
	for(i=0;i<n;i++)
	{
		mgl_set_obj_id(gr,i+MGL_MAX_LINES);
		QString tst = primitives.section('\n',i,i);
		pr.Parse(&gg,primitives.section('\n',i,i).toLocal8Bit().constData(),i+MGL_MAX_LINES);
	}
	gg.SetRanges(ox1,ox2);	gg.Pop();	setlocale(LC_NUMERIC, loc.c_str());
}
//-----------------------------------------------------------------------------
void QMathGL::refresh()
{
	if(dotsRefr)
	{
		timerRefr->start();
		int q = gr->GetQuality();
		prevQuality = q!=MGL_DRAW_DOTS?q:prevQuality;
		gr->SetQuality(MGL_DRAW_DOTS);
	}
	if(mgl_is_frames(gr) && mgl_get_num_frame(gr)>0)
	{
		drawPrim();
		if(custZoom)	emit customZoom(x1,y1,x2,y2,tet,phi,per);
		else
		{	mgl_zoom(gr,x1,y1,x2,y2);
			mgl_ask_perspective(gr,per);
			if(viewYZ)	mgl_view(gr,0,-tet,-phi);
			else 		mgl_view(gr,-phi,-tet,0);
		}
	}
	mglConvertFromGraph(pic, gr, &grBuf);
	if(pic.size()!=size())	setSize(pic.width(), pic.height());
	repaint();
}
//-----------------------------------------------------------------------------
void QMathGL::refreshHQ()
{
	gr->SetQuality(prevQuality);
	if(mgl_is_frames(gr) && mgl_get_num_frame(gr)>0)
	{
		drawPrim();
		if(custZoom)	emit customZoom(x1,y1,x2,y2,tet,phi,per);
		else
		{	mgl_zoom(gr,x1,y1,x2,y2);
			mgl_ask_perspective(gr,per);
			if(viewYZ)	mgl_view(gr,0,-tet,-phi);
			else 		mgl_view(gr,-phi,-tet,0);
		}
	}
	mglConvertFromGraph(pic, gr, &grBuf, &img);
	if(pic.size()!=size())	setSize(pic.width(), pic.height());
	repaint();
}
//-----------------------------------------------------------------------------
void QMathGL::mousePressEvent(QMouseEvent *ev)
{
	if(!zoom && !rotate && ev->button()&Qt::LeftButton)
	{
		mglPoint p = gr->CalcXYZ(ev->x(), ev->y());
		mglCanvasWnd *g=dynamic_cast<mglCanvasWnd *>(gr);
		if(g)	g->LastMousePos = p;
		if(g && g->ClickFunc)	g->ClickFunc(draw_par);
		emit mouseClick(p.x,p.y,p.z);
		int id = mgl_get_obj_id(gr,ev->x(),ev->y());
		if(id<MGL_MAX_LINES)	emit objChanged(id-1);

		p = gr->CalcXYZ(ev->x(), ev->y(), true);
		if(mgl_isnan(p.x))	mousePos = "";
		else	mousePos.sprintf("x=%g, y=%g, z=%g",p.x,p.y,p.z);
		emit posChanged(mousePos);
		repaint();
	}
	xe=x0=ev->x();	ye=y0=ev->y();	ev->accept();
}
//-----------------------------------------------------------------------------
void QMathGL::mouseReleaseEvent(QMouseEvent *ev)
{
	if(ev->button()&Qt::LeftButton && enableMouse)
	{
		if(zoom)
		{
			int w1=width(),h1=height();
			mreal _x1,_x2,_y1,_y2;
			_x1 = x1+(x2-x1)*(x0-x())/mreal(w1);	_y1 = y2-(y2-y1)*(ye-y())/mreal(h1);
			_x2 = x1+(x2-x1)*(xe-x())/mreal(w1);	_y2 = y2-(y2-y1)*(y0-y())/mreal(h1);
			x1=_x1;		x2=_x2;		y1=_y1;		y2=_y2;
			if(x1>x2)	{	_x1=x1;	x1=x2;	x2=_x1;	}
			if(y1>y2)	{	_x1=y1;	y1=y2;	y2=_x1;	}
			x0 = xe;	y0 = ye;
			if(custDraw)
			{
				emit customDraw(x1,y1,x2,y2,false);
				update();
			}
			else	refresh();
		}
	}
	if(ev->button()&Qt::RightButton && popup && !rotate)	// popup menu
		popup->popup(QCursor::pos());
	ev->accept();
}
//-----------------------------------------------------------------------------
void QMathGL::mouseMoveEvent(QMouseEvent *ev)
{
	if(!enableMouse)	{	ev->ignore();	return;	}
	xe=ev->x();	ye=ev->y();
	if(rotate)
	{
		if(ev->buttons()&Qt::LeftButton)	// rotate
		{
			mreal ff = 240/sqrt(mreal(width()*height()));
			phi += int((x0-xe)*ff);
			tet -= int((y0-ye)*ff);
			if(phi>180)		phi-=360;
			if(phi<-180)	phi+=360;
			if(tet>180)		tet-=360;
			if(tet<-180)	tet+=360;
			emit tetChanged(int(tet));		emit phiChanged(int(phi));
			refresh();
		}
		if(ev->buttons()&Qt::RightButton)	// zoom and perspective
		{
			mreal ff = 2.*(y0-ye)/width(), gg = 0.5*(xe-x0)/height();
			mreal cx = (x1+x2)/2, cy = (y1+y2)/2;
			x1 = cx+(x1-cx)*exp(-ff);	x2 = cx+(x2-cx)*exp(-ff);
			y1 = cy+(y1-cy)*exp(-ff);	y2 = cy+(y2-cy)*exp(-ff);
			per = per + gg;
			if(per<0)	per = 0;
			if(per>=1)	per = 0.9999;
			if(gg)	emit perChanged(int(per));
			refresh();
		}
		if(ev->buttons()&Qt::MidButton)	// shift
		{
			mreal ff = 1./sqrt(mreal(width()*height()));
			mreal dx = (x0-xe)*ff*(x2-x1), dy = (y0-ye)*ff*(y2-y1);
			x1 += dx;	x2 += dx;	y1 -= dy;	y2 -= dy;
		}
		x0 = xe;	y0 = ye;
		refresh();
	}
	else if(zoom)	refresh();
	else if(ev->buttons()&Qt::MidButton)	// shift axis
	{
		mreal ff = 1./sqrt(mreal(width()*height()));
		mreal dx = (x0-xe)*ff*(ax2-ax1), dy = (y0-ye)*ff*(ay2-ay1);
		ax1 += dx;	ax2 += dx;	ay1 -= dy;	ay2 -= dy;
		mgl_zoom_axis(gr,ax1,ay1,0,0,ax2,ay2,0,0);
		update();	x0 = xe;	y0 = ye;
	}
	else if(ev->buttons()&Qt::LeftButton)	// move primitives
	{
		long h=pic.height(), w=pic.width(), d=(h>w?w:h)/100;
		long pos = mgl_is_active(gr,x0,y0,d);
		long id = long(mgl_get_obj_id(gr,x0,y0))-MGL_MAX_LINES;
		if(grid && pos>=0)	// this active point
		{
			const mglActivePos &p = gr->Act[pos];
			id = long(p.id)-MGL_MAX_LINES;
			if(id>=0)	// this is our primitive
			{
				// try "attract" mouse
				for(size_t i=0;i<=10;i++)
				{
					int tt = i*(w/10);	if(abs(xe-tt)<2*d)	xe = tt;
					tt = i*(h/10);	if(abs(ye-tt)<2*d)	ye = tt;
				}
				for(size_t i=0;i<gr->Act.size();i++)
				{
					const mglActivePos &q = gr->Act[i];
					if(abs(xe-q.x)<2*d && abs(ye-q.y)<2*d)	{	xe=q.x;	ye=q.y;	}
				}
				// now move point
				QString tst = primitives.section('\n',id,id), cmd=tst.section(' ',0,0), res;
				float dx = 2*(xe-x0)/float(w), dy = 2*(y0-ye)/float(h);
				float xx=tst.section(' ',1,1).toFloat(), yy=tst.section(' ',2,2).toFloat();
				if(p.n==0)
					res = cmd+" "+QString::number(xx+dx)+" "+QString::number(yy+dy)+" "+tst.section(' ',3);
				else if(cmd=="rect")
				{
					float x_=tst.section(' ',3,3).toFloat(), y_=tst.section(' ',4,4).toFloat();
					if(p.n==1)	{	xx+=dx;	y_+=dy;	}
					if(p.n==2)	{	x_+=dx;	yy+=dy;	}
					if(p.n==3)	{	x_+=dx;	y_+=dy;	}
					res = "rect "+QString::number(xx)+" "+QString::number(yy)+" "+
						QString::number(x_)+" "+QString::number(y_)+" "+tst.section(' ',5);
				}
				else if(p.n==1)
				{
					xx=tst.section(' ',3,3).toFloat();	yy=tst.section(' ',4,4).toFloat();
					res = tst.section(' ',0,2)+" "+QString::number(xx+dx)+" "+QString::number(yy+dy)+" "+tst.section(' ',5);
				}
				else if(cmd=="rhomb" || cmd=="ellipse")
				{
					float x_=tst.section(' ',3,3).toFloat()-xx, y_=tst.section(' ',4,4).toFloat()-yy, dr=0;
					if(x_*x_+y_*y_>0)
					{
						dr = (dx*x_+dy*y_)/(x_*x_+y_*y_);
						dr = hypot(dx-dr*x_,dy-dr*y_);
					}
					else	dr = hypot(dx,dy);
					res = tst.section(' ',0,4)+" "+QString::number(tst.section(' ',5,5).toFloat()+dr)+" "+tst.section(' ',6);
				}
				else if(cmd=="arc")
				{
					double x_=tst.section(' ',3,3).toFloat()-xx, y_=tst.section(' ',4,4).toFloat()-yy, a_=tst.section(' ',5,5).toFloat();
					double c=cos(M_PI*a_/180), s=sin(M_PI*a_/180);
					double a = atan2(x_,y_) - atan2(x_*c-y_*s+dx,x_*s+y_*c+dy);
					res = tst.section(' ',0,4)+" "+QString::number(a*180/M_PI)+" "+tst.section(' ',6);
				}
				else if(p.n==2)
				{
					xx=tst.section(' ',5,5).toFloat();	yy=tst.section(' ',6,6).toFloat();
					res = tst.section(' ',0,4)+" "+QString::number(xx+dx)+" "+QString::number(yy+dy)+" "+tst.section(' ',7);
				}
				else if(p.n==3)
				{
					xx=tst.section(' ',7,7).toFloat();	yy=tst.section(' ',8,8).toFloat();
					if(cmd=="curve")	{	dx*=-1;	dy*=-1;	}
					res = tst.section(' ',0,6)+" "+QString::number(xx+dx)+" "+QString::number(yy+dy)+" "+tst.section(' ',9);
				}
				if(id>0) 	res = primitives.section('\n',0,id-1) + "\n" + res;
				primitives = res + "\n" + primitives.section('\n',id+1);
				refresh();	x0 = xe;	y0 = ye;
			}
		}
		else if(id>=0)	// this is primitive
		{
			QString tst = primitives.section('\n',id,id), cmd=tst.section(' ',0,0), res;
			float dx = 2*(xe-x0)/float(w), dy = 2*(y0-ye)/float(h);
			float x1=tst.section(' ',1,1).toFloat(), y1=tst.section(' ',2,2).toFloat(),x2,y2;
			if(cmd=="ball")
				res = cmd+" "+QString::number(x1+dx)+" "+QString::number(y1+dy)+" "+tst.section(' ',3);
			else if(cmd=="curve")
			{
				x2=tst.section(' ',5,5).toFloat();	y2=tst.section(' ',6,6).toFloat();
				res = cmd+" "+QString::number(x1+dx)+" "+QString::number(y1+dy)+" "+tst.section(' ',3,4)+
						" "+QString::number(x2+dx)+" "+QString::number(y2+dy)+" "+tst.section(' ',7);
			}
			else
			{
				x2=tst.section(' ',3,3).toFloat();	y2=tst.section(' ',4,4).toFloat();
				res = cmd+" "+QString::number(x1+dx)+" "+QString::number(y1+dy)+" "+
						QString::number(x2+dx)+" "+QString::number(y2+dy)+" "+tst.section(' ',5);
			}
			if(id>0) 	res = primitives.section('\n',0,id-1) + "\n" + res;
			primitives = res + "\n" + primitives.section('\n',id+1);
			refresh();	x0 = xe;	y0 = ye;
		}
	}
/*	else if(mgl_get_flag(gr,MGL_SHOW_POS) && !(ev->button()&Qt::LeftButton))
	{
		mglPoint p = gr->CalcXYZ(ev->x(), ev->y(), true);
		if(mgl_isnan(p.x))	mousePos = "";
		else	mousePos.sprintf("x=%g, y=%g, z=%g",p.x,p.y,p.z);
		emit posChanged(mousePos);
	}*/
	ev->accept();
}
//-----------------------------------------------------------------------------
void QMathGL::mouseDoubleClickEvent(QMouseEvent *ev)
{
	long h=pic.height(), w=pic.width(), d=(h>w?w:h)/100;
	long pos = mgl_is_active(gr,x0,y0,d);
	long id = long(mgl_get_obj_id(gr,x0,y0));
	if(grid && pos>=0)	// this active point -> delete primitive
	{
		const mglActivePos &p = gr->Act[pos];
		id = long(p.id)-MGL_MAX_LINES;
		QString res;
		if(id>0) 	res = primitives.section('\n',0,id-1) + "\n";
		if(id>=0)	primitives = res + primitives.section('\n',id+1);
		refresh();	x0 = xe;	y0 = ye;
	}
	else if(id>=MGL_MAX_LINES)	// option for primitives
		emit askStyle(id-MGL_MAX_LINES);
	else	emit doubleClick(id);
	ev->accept();
}
//-----------------------------------------------------------------------------
void QMathGL::setStyle(int id, QString stl)
{
	QString tst = primitives.section('\n',id,id), res;
	res = tst.section(' ',0,-2) + " " + stl;
	if(id>0) 	res = primitives.section('\n',0,id-1) + "\n" + res;
	primitives = res + "\n" + primitives.section('\n',id+1);
	refresh();	x0 = xe;	y0 = ye;
}
//-----------------------------------------------------------------------------
void QMathGL::wheelEvent(QWheelEvent *ev)
{
	if(!enableWheel)	{	ev->ignore();	return;	}
	if(rotate)	// zoom
	{
		mreal d,c,f=exp(0.001*ev->delta())/2;
		d = (y2-y1)*f;	c = (y2+y1)/2;	y1 = c-d;	y2 = c+d;
		d = (x2-x1)*f;	c = (x2+x1)/2;	x1 = c-d;	x2 = c+d;
		refresh();	ev->accept();
	}
	else 		// zoom axis
	{
		mreal d,c,f=exp(0.001*ev->delta())/2;
		d = (ay2-ay1)*f;	c = (ay2+ay1)/2;	ay1 = c-d;	ay2 = c+d;
		d = (ax2-ax1)*f;	c = (ax2+ax1)/2;	ax1 = c-d;	ax2 = c+d;
		mgl_zoom_axis(gr,ax1,ay1,0,0,ax2,ay2,0,0);
		update();	ev->accept();
	}
}
//-----------------------------------------------------------------------------
void QMathGL::imgSize(int w, int h)
{	if(w>0 && h>0)	{	mgl_set_size(gr,w,h);	update();	}	}
//-----------------------------------------------------------------------------
QString setExtension(const QString &fname, const char *ext)
{
	QString oname=fname;
	if(fname.right(4)!="."+QString(ext))	oname = fname+"."+QString(ext);
	return oname;
}
//-----------------------------------------------------------------------------
void QMathGL::exportGIF(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
#if MGL_HAVE_GIF
		mgl_write_gif(gr,setExtension(fname,"png").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
#else
		img.save(setExtension(fname,"gif"));
#endif
}
//-----------------------------------------------------------------------------
void QMathGL::exportPNG(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
#if MGL_HAVE_PNG
		mgl_write_png(gr,setExtension(fname,"png").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
#else
		img.save(setExtension(fname,"png"));
#endif
}
//-----------------------------------------------------------------------------
void QMathGL::exportPNGs(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
#if MGL_HAVE_PNG
		mgl_write_png_solid(gr,setExtension(fname,"png").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
#else
		img.save(setExtension(fname,"png"));
#endif
}
//-----------------------------------------------------------------------------
void QMathGL::exportJPG(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
#if MGL_HAVE_JPEG
		mgl_write_jpg(gr,setExtension(fname,"jpg").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
#else
		img.save(setExtension(fname,"jpg"));
#endif
}
//-----------------------------------------------------------------------------
void QMathGL::exportBPS(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_bps(gr,setExtension(fname,"eps").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportEPS(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_eps(gr,setExtension(fname,"eps").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportSVG(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_svg(gr,setExtension(fname,"svg").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportXYZ(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_xyz(gr,setExtension(fname,"xyz").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportTEX(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_tex(gr,setExtension(fname,"tex").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportOFF(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_off(gr,setExtension(fname,"off").toLocal8Bit().constData(), appName.toLocal8Bit().constData(),0);
}
//-----------------------------------------------------------------------------
void QMathGL::exportOBJ(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_obj(gr,setExtension(fname,"obj").toLocal8Bit().constData(), appName.toLocal8Bit().constData(),1);
}
//-----------------------------------------------------------------------------
void QMathGL::exportSTL(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_stl(gr,setExtension(fname,"stl").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
/*void QMathGL::exportX3D(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_x3d(gr,setExtension(fname,"x3d").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}*/
//-----------------------------------------------------------------------------
void QMathGL::exportTGA(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_tga(gr,setExtension(fname,"tga").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void QMathGL::exportPRC(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_write_prc(gr,setExtension(fname,"prc").toLocal8Bit().constData(), appName.toLocal8Bit().constData(),1);
}
//-----------------------------------------------------------------------------
void QMathGL::exportMGLD(QString fname)
{
	if(fname.isEmpty())	fname = mgl_get_plotid(gr);
	if(fname.isEmpty())	QMessageBox::critical(this, appName, _("No filename."),QMessageBox::Ok,0,0);
	else
		mgl_export_mgld(gr,setExtension(fname,"mgld").toLocal8Bit().constData(), appName.toLocal8Bit().constData());
}
//-----------------------------------------------------------------------------
void mglConvertFromGraph(QPixmap &pic, mglCanvas *gr, uchar **buf, QImage *out)
{
	const uchar *bb = mgl_get_rgb(gr);
	long w=mgl_get_width(gr), h=mgl_get_height(gr);
	if(*buf)	delete [](*buf);
	*buf = new uchar[4*w*h];
	for(long i=0;i<w*h;i++)
	{
		(*buf)[4*i]   = bb[3*i+2];
		(*buf)[4*i+1] = bb[3*i+1];
		(*buf)[4*i+2] = bb[3*i];
		(*buf)[4*i+3] = 255;
	}
	QImage img(*buf, w, h, QImage::Format_RGB32);
	if(out)	*out = img;
	pic = QPixmap::fromImage(img);
}
//-----------------------------------------------------------------------------
void QMathGL::copy()
{	QClipboard *cb = QApplication::clipboard();
	cb->setPixmap(pic, QClipboard::Clipboard);		}
//-----------------------------------------------------------------------------
void QMathGL::copyClickCoor()
{	QApplication::clipboard()->setText(mousePos);	}
//-----------------------------------------------------------------------------
void QMathGL::setMGLFont(QString path)
{	if(path.isEmpty())	mgl_restore_font(gr);
	else 	mgl_load_font(gr,path.toLocal8Bit().constData(),0);	}
//-----------------------------------------------------------------------------
void QMathGL::setSize(int w, int h)
{
	resize(w, h);
	if(w!=pic.width() || h!=pic.height())	// update only when image size is changed
	{	mgl_set_size(gr,w,h);	update();
		setMinimumSize(gr->GetWidth(),gr->GetHeight());	}
}
//-----------------------------------------------------------------------------
void QMathGL::about()
{
	QString s = _("MathGL v. 2.") + QString::number(MGL_VER2) + _("\n(c) Alexey Balakin, 2007\nhttp://mathgl.sourceforge.net/");
	QMessageBox::about(this, _("MathGL - about"), s);
}
//-----------------------------------------------------------------------------
void QMathGL::aboutQt()	{	QMessageBox::aboutQt(this, _("About Qt"));	}
//-----------------------------------------------------------------------------
void QMathGL::print()
{
	QPrinter *printer = new QPrinter;
	printer->setOrientation(getRatio()>1 ? QPrinter::Landscape : QPrinter::Portrait);
	QPrintDialog printDlg(printer, this);
	if (printDlg.exec() == QDialog::Accepted)
	{
		QRectF r = printer->pageRect(QPrinter::Inch);
		int d1 = int(pic.width()/r.width()), d2 = int(pic.height()/r.height());
		int dpi = printer->resolution();
		if(dpi<d1)	dpi=d1;
		if(dpi<d2)	dpi=d2;
		printer->setResolution(dpi);

		QPainter p;
		if(!p.begin(printer))	return;	// paint on printer
		p.drawPixmap(0,0,pic);
	}
	delete printer;
}
//-----------------------------------------------------------------------------
void QMathGL::nextSlide()
{
	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	if(g && g->GetNumFig()>1)	g->NextFrame();
	emit frameChanged(+1);
}
void QMathGL::prevSlide()
{
	mglCanvasWnd *g = dynamic_cast<mglCanvasWnd *>(gr);
	if(g && g->GetNumFig()>1)	g->PrevFrame();
	emit frameChanged(-1);
}
//-----------------------------------------------------------------------------
void QMathGL::animation(bool st)
{
	if(!st)	timer->stop();
	else	timer->start(int(mgl_wnd_get_delay(gr)*1000));
}
//-----------------------------------------------------------------------------
void QMathGL::adjust()
{
	mgl_set_size(gr, parentWidget()->width()-3, parentWidget()->height()-3);
	setSize(parentWidget()->width()-3, parentWidget()->height()-3);
	update();	// TODO replace to refresh ?!?
}
//-----------------------------------------------------------------------------
void QMathGL::addMark()
{	primitives += "ball 0 0 'r*'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addLine()
{	primitives += "line -0.2 0 0.2 0 'r2'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addRect()
{	primitives += "rect -0.2 -0.2 0.2 0.2 'r'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addCurve()
{	primitives += "curve -0.2 0 0 0.5 0.2 0 0 0.5 'r2'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addRhomb()
{	primitives += "rhomb -0.2 0 0.2 0 0.1 'r'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addEllipse()
{	primitives += "ellipse -0.2 0 0.2 0 0.1 'r'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addArc()
{	primitives += "arc 0 0 0.2 0 60 'r2'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addPolygon(int n)
{
	if(n<3)
		n = QInputDialog::getText(QApplication::activeWindow(), "MathGL", _("Enter number of vertexes")).toInt();
	if(n>=3)
	{	primitives += "polygon 0 0 0 0.2 "+QString::number(n)+" 'r'\n";	refresh();	}
}
//{	primitives += "arc -0.2 0 0.2 0 0.1 'r'\n";	refresh();	}
//-----------------------------------------------------------------------------
void QMathGL::addText(QString txt)
{
	if(txt.isEmpty())
		txt = QInputDialog::getText(QApplication::activeWindow(), "MathGL", _("Enter text"));
	if(!txt.isEmpty())
	{	primitives += "text 0 0 0.1 0 '"+txt+"' ''\n";	refresh();	}
}
//-----------------------------------------------------------------------------
//
//		class mglCanvasQT
//
//-----------------------------------------------------------------------------
mglCanvasQT::mglCanvasQT() : mglCanvasWnd()
{	Wnd = 0;	}
mglCanvasQT::~mglCanvasQT()
{	if(Wnd)	{	QMGL->gr=0;	delete Wnd;	}	}
//-----------------------------------------------------------------------------
void mglCanvasQT::GotoFrame(int d)
{
	int f = GetCurFig()+d;
	if(f>=GetNumFig())	f = 0;
	if(f<0)	f = GetNumFig()-1;
	if(GetNumFig()>0 && d)	{	SetCurFig(f);	QMGL->refresh();	}
}
//-----------------------------------------------------------------------------
void mglCanvasQT::Animation()
{
	static bool start=true;
	QMGL->animation(start);
	start = !start;
}
//-----------------------------------------------------------------------------
void mglCanvasQT::ToggleAlpha()	{	QMGL->setAlpha(!QMGL->getAlpha());	}
//-----------------------------------------------------------------------------
void mglCanvasQT::ToggleLight()	{	QMGL->setLight(!QMGL->getLight());	}
//-----------------------------------------------------------------------------
void mglCanvasQT::ToggleNo()	{	QMGL->restore();	}
//-----------------------------------------------------------------------------
void mglCanvasQT::ToggleZoom()	{	QMGL->setZoom(!QMGL->getZoom());	}
//-----------------------------------------------------------------------------
void mglCanvasQT::ToggleRotate(){	QMGL->setRotate(!QMGL->getRotate());}
//-----------------------------------------------------------------------------
void mglCanvasQT::Update()		{	SetCurFig(0);	QMGL->update();	Wnd->show();	}
//-----------------------------------------------------------------------------
void mglCanvasQT::Adjust()		{	QMGL->adjust();	}
//-----------------------------------------------------------------------------
void mglCanvasQT::Window(int argc, char **argv, int (*draw)(mglBase *gr, void *p), const char *title, void *par, void (*reload)(void *p), bool maximize)
{
	static char arg=0, *parg=&arg, **argv_=&parg;
	static int argc_=0;
	SetDrawFunc(draw, par, reload);
	if(Wnd)
	{
		Wnd->setWindowTitle(title);
		if(maximize)
		{	Wnd->showMaximized();	}
		else	Wnd->show();
		return;
	}

	if(!qApp)
	{
		QCoreApplication::setAttribute(Qt::AA_X11InitThreads);
		if(!argv)	{	argc_ = 0;	argv_=&parg;	}
		else		{	argc_ = argc;	argv_=argv;	}
		QApplication *a = new QApplication(argc_, argv_);
		a->connect(a, SIGNAL(lastWindowClosed()), a, SLOT(quit()));
	}

	Wnd = new QMainWindow;	Wnd->resize(850,680);
	Wnd->setWindowTitle(title);
	scroll = new QScrollArea(Wnd);

	QMGL = new QMathGL(Wnd);
	popup = mglMakeMenu(Wnd, QMGL, tet, phi);
	QMGL->setPopup(popup);	QMGL->setGraph(this);
	QMGL->setDraw(draw, par);
	QMGL->appName = title;
	qApp->processEvents();
	scroll->setWidget(QMGL);
	Wnd->setCentralWidget(scroll);
	QMGL->refresh();
	if(!maximize)	Wnd->show();
	else	Wnd->showMaximized();
}
//-----------------------------------------------------------------------------
#include "xpm/alpha.xpm"
#include "xpm/arc.xpm"
#include "xpm/copy.xpm"
#include "xpm/curve.xpm"
#include "xpm/down_1.xpm"
#include "xpm/fileprint.xpm"
#include "xpm/left_1.xpm"
#include "xpm/light.xpm"
#include "xpm/line.xpm"
#include "xpm/mark_a.xpm"
#include "xpm/mark_d.xpm"
#include "xpm/mark_o.xpm"
#include "xpm/mark_s.xpm"
#include "xpm/right_1.xpm"
#include "xpm/next_sl.xpm"
#include "xpm/norm_1.xpm"
#include "xpm/ok.xpm"
#include "xpm/prev_sl.xpm"
#include "xpm/rotate.xpm"
#include "xpm/show_sl.xpm"
#include "xpm/text.xpm"
#include "xpm/polygon.xpm"
#include "xpm/zoom_1.xpm"
#include "xpm/zoom_in.xpm"
#include "xpm/zoom_out.xpm"
#include "xpm/up_1.xpm"
#include "xpm/stop.xpm"
#include "xpm/pause.xpm"
//-----------------------------------------------------------------------------
MGL_EXPORT QMenu *mglMakeMenu(QMainWindow *Wnd, QMathGL *QMGL, QSpinBox *&tet, QSpinBox *&phi)
{
	QAction *a;
	QMenu *o, *oo, *f;
	QToolBar *bb;

	QMenu *popup = new QMenu(Wnd);
	// file menu
	{
		f = o = Wnd->menuBar()->addMenu(_("File"));
		oo = new QMenu(_("Export as 2D ..."),Wnd);
		oo->addAction(_("PNG"), QMGL, SLOT(exportPNG()),Qt::ALT+Qt::Key_P);
		oo->addAction(_("solid PNG"), QMGL, SLOT(exportPNGs()),Qt::ALT+Qt::Key_F);
		oo->addAction(_("JPEG"), QMGL, SLOT(exportJPG()),Qt::ALT+Qt::Key_J);
		oo->addAction(_("bitmap EPS"), QMGL, SLOT(exportBPS()));
		oo->addAction(_("vector EPS"), QMGL, SLOT(exportEPS()),Qt::ALT+Qt::Key_E);
		oo->addAction(_("SVG"), QMGL, SLOT(exportSVG()),Qt::ALT+Qt::Key_S);
		oo->addAction(_("LaTeX"), QMGL, SLOT(exportTEX()),Qt::ALT+Qt::Key_L);
		o->addMenu(oo);		popup->addMenu(oo);
		oo = new QMenu(_("Export as 3D ..."),Wnd);
		oo->addAction(_("MGLD"), QMGL, SLOT(exportMGLD()),Qt::ALT+Qt::Key_M);
		oo->addAction(_("PRC"), QMGL, SLOT(exportPRC()),Qt::ALT+Qt::Key_D);
		oo->addAction(_("OBJ"), QMGL, SLOT(exportOBJ()),Qt::ALT+Qt::Key_O);
		oo->addAction(_("STL"), QMGL, SLOT(exportSTL()));
		oo->addAction(_("XYZ"), QMGL, SLOT(exportXYZ()));
//		oo->addAction(_("X3D"), QMGL, SLOT(exportX3D()),Qt::ALT+Qt::Key_X);
		o->addMenu(oo);		popup->addMenu(oo);

		o->addSeparator();
		a = new QAction(QPixmap(fileprint), _("Print graphics"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(print()));
		a->setToolTip(_("Open printer dialog and print graphics (Ctrl+P)"));
		a->setShortcut(Qt::CTRL+Qt::Key_P);	o->addAction(a);
		o->addSeparator();
		o->addAction(_("Close"), Wnd, SLOT(close()), Qt::CTRL+Qt::Key_W);
	}
	// graphics menu
	{
		bb = new QToolBar(_("Graphics"),Wnd);
		Wnd->addToolBar(Qt::TopToolBarArea, bb);
		o = Wnd->menuBar()->addMenu(_("Graphics"));
		a = new QAction(QPixmap(alpha_xpm), _("Alpha"), Wnd);
		a->setShortcut(Qt::ALT+Qt::Key_T);	a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(setAlpha(bool)));
		Wnd->connect(QMGL, SIGNAL(alphaChanged(bool)), a, SLOT(setChecked(bool)));
		a->setToolTip(_("Switch on/off transparency for the graphics (Alt+T)."));
		o->addAction(a);		bb->addAction(a);
		a = new QAction(QPixmap(light_xpm), _("Light"), Wnd);
		a->setShortcut(Qt::ALT+Qt::Key_L);	a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(setLight(bool)));
		Wnd->connect(QMGL, SIGNAL(lightChanged(bool)), a, SLOT(setChecked(bool)));
		a->setToolTip(_("Switch on/off lightning for the graphics (Alt+L)."));
		o->addAction(a);		bb->addAction(a);
		a = new QAction(QPixmap(rotate_xpm), _("Rotate by mouse"), Wnd);
		a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(setRotate(bool)));
		Wnd->connect(QMGL, SIGNAL(rotateChanged(bool)), a, SLOT(setChecked(bool)));
		a->setToolTip(_("Switch on/off mouse handling of the graphics\n(rotation, shifting, zooming and perspective)."));
		bb->addAction(a);
		a = new QAction(QPixmap(zoom_in_xpm), _("Zoom by mouse"), Wnd);
		a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(setZoom(bool)));
		Wnd->connect(QMGL, SIGNAL(zoomChanged(bool)), a, SLOT(setChecked(bool)));
		a->setToolTip(_("Switch on/off mouse zoom of selected region."));
		bb->addAction(a);
		o->addSeparator();
		a = new QAction(QPixmap(zoom_out_xpm), _("Restore"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(restore()));
		a->setToolTip(_("Restore default graphics rotation, zoom and perspective (Alt+Space)."));
		a->setShortcut(Qt::ALT+Qt::Key_Space);
		o->addAction(a);	bb->addAction(a);	popup->addAction(a);
		bb->addSeparator();
		o->addAction(a);	bb->addAction(a);	popup->addAction(a);
		a = new QAction(QPixmap(ok_xpm), _("Redraw"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(update()));
		a->setToolTip(_("Execute script and redraw graphics (F5)."));
		a->setShortcut(Qt::Key_F5);
		o->addAction(a);	bb->addAction(a);	popup->addAction(a);
		a = new QAction(QPixmap(stop_xpm), _("Stop"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(stop()));
		a->setToolTip(_("Ask to stop plot drawing (F7)."));
		a->setShortcut(Qt::Key_F7);
		a = new QAction(_("Adjust size"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(adjust()));
		a->setToolTip(_("Change canvas size to fill whole region (F6)."));
		a->setShortcut(Qt::Key_F6);		o->addAction(a);
		a = new QAction(QPixmap(copy_xpm), _("Copy plot"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(copy()));
		a->setToolTip(_("Copy graphics to clipboard (Ctrl+Shift+G)."));
		a->setShortcut(Qt::CTRL+Qt::SHIFT+Qt::Key_G);
		o->addAction(a);		bb->addAction(a);	popup->addAction(a);

		bb->addSeparator();
		oo = new QMenu(_("Primitives ..."),Wnd);
		a = new QAction(QPixmap(line_xpm), _("Add line"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addLine()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add line which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(arc_xpm), _("Add arc"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addArc()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add arc which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(curve_xpm), _("Add curve"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addCurve()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add curve which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(mark_s_xpm), _("Add rect"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addRect()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add rectangle which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(mark_d_xpm), _("Add rhombus"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addRhomb()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add rhombus which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(mark_o_xpm), _("Add ellipse"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addEllipse()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add ellipse which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(polygon_xpm), _("Add polygon"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addPolygon()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add polygon which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(mark_a_xpm), _("Add mark"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addMark()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add marker which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		a = new QAction(QPixmap(text_xpm), _("Add text"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(addText()));
		Wnd->connect(QMGL, SIGNAL(usePrimChanged(bool)), a, SLOT(setVisible(bool)));
		a->setToolTip(_("Add text which properties can be changed later by mouse."));
		bb->addAction(a);	oo->addAction(a);
		o->addMenu(oo);

		bb->addSeparator();
		tet = new QSpinBox(Wnd);	tet->setWrapping(true);
		bb->addWidget(tet);	tet->setRange(-180, 180);	tet->setSingleStep(10);
		Wnd->connect(tet, SIGNAL(valueChanged(int)), QMGL, SLOT(setTet(int)));
		Wnd->connect(QMGL, SIGNAL(tetChanged(int)), tet, SLOT(setValue(int)));
		tet->setToolTip(_("Set value of \\theta angle."));
		bb->addSeparator();
		phi = new QSpinBox(Wnd);	phi->setWrapping(true);
		bb->addWidget(phi);	phi->setRange(-180, 180);	phi->setSingleStep(10);
		Wnd->connect(phi, SIGNAL(valueChanged(int)), QMGL, SLOT(setPhi(int)));
		Wnd->connect(QMGL, SIGNAL(phiChanged(int)), phi, SLOT(setValue(int)));
		phi->setToolTip(_("Set value of \\phi angle."));
//	bb->addSeparator();
	}
	// zooming menu
	{
		oo = o->addMenu(_("Zoom/move"));
		bb = new QToolBar(_("Zoom graphics"),Wnd);
		Wnd->addToolBar(Qt::LeftToolBarArea, bb);
		a = new QAction(QPixmap(left_1_xpm), _("Move left"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(shiftLeft()));
		a->setToolTip(_("Move graphics left by 1/3 of its width."));
		bb->addAction(a);		oo->addAction(a);
		a = new QAction(QPixmap(up_1_xpm), _("Move up"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(shiftUp()));
		a->setToolTip(_("Move graphics up by 1/3 of its height."));
		bb->addAction(a);		oo->addAction(a);
		a = new QAction(QPixmap(zoom_1_xpm), _("Zoom in"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(zoomIn()));
		a->setToolTip(_("Zoom in graphics."));
		bb->addAction(a);		oo->addAction(a);
		a = new QAction(QPixmap(norm_1_xpm), _("Zoom out"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(zoomOut()));
		a->setToolTip(_("Zoom out graphics."));
		bb->addAction(a);		oo->addAction(a);
		a = new QAction(QPixmap(down_1_xpm), _("Move down"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(shiftDown()));
		a->setToolTip(_("Move graphics down 1/3 of its height."));
		bb->addAction(a);		oo->addAction(a);
		a = new QAction(QPixmap(right_1_xpm), _("Move right"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(shiftRight()));
		a->setToolTip(_("Move graphics right by 1/3 of its width."));
		bb->addAction(a);		oo->addAction(a);
	}
	// animation menu
	{
		o = Wnd->menuBar()->addMenu(_("Animation"));
		bb = new QToolBar(_("Animation"),Wnd);
		Wnd->addToolBar(Qt::LeftToolBarArea, bb);
		a = new QAction(QPixmap(next_sl_xpm), _("Next slide"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(nextSlide()));
		a->setToolTip(_("Show next slide (Ctrl+.)."));
		a->setShortcut(Qt::CTRL+Qt::Key_Period);	o->addAction(a);		bb->addAction(a);
		a = new QAction(QPixmap(show_sl_xpm), _("Slideshow"), Wnd);
		a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(animation(bool)));
		a->setToolTip(_("Run slideshow (CTRl+F5)."));
		a->setShortcut(Qt::CTRL+Qt::Key_F5);	o->addAction(a);		bb->addAction(a);
		a = new QAction(QPixmap(prev_sl_xpm), _("Prev slide"), Wnd);
		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(prevSlide()));
		a->setToolTip(_("Show previous slide (Ctrl+,)."));
		a->setShortcut(Qt::CTRL+Qt::Key_Comma);	o->addAction(a);		bb->addAction(a);
	}
#if MGL_HAVE_PTHR_WIDGET
	{
		bb = new QToolBar(_("Calculations"),Wnd);
		Wnd->addToolBar(Qt::LeftToolBarArea, bb);
		a = new QAction(QPixmap(pause_xpm), _("Pause calculation"), Wnd);
		a->setCheckable(true);
		Wnd->connect(a, SIGNAL(toggled(bool)), QMGL, SLOT(setPause(bool)));
		Wnd->connect(QMGL, SIGNAL(pauseChanged(bool)), a, SLOT(setChecked(bool)));
//		Wnd->connect(a, SIGNAL(triggered()), QMGL, SLOT(setPause()));
		a->setToolTip(_("Pause on/off external calculations"));
		f->addSeparator();	f->addAction(a);	bb->addAction(a);
	}
#endif

	Wnd->menuBar()->addSeparator();
	o = Wnd->menuBar()->addMenu(_("Help"));
	o->addAction(_("About"), QMGL, SLOT(about()));
	o->addAction(_("About Qt"), QMGL, SLOT(aboutQt()));
	return popup;
}
//-----------------------------------------------------------------------------
HMGL MGL_EXPORT mgl_create_graph_qt(int (*draw)(HMGL gr, void *p), const char *title, void *par, void (*load)(void *p))
{
	mglCanvasQT *g = new mglCanvasQT;
	g->Window(0,0,draw,title,par,load);
	return g;
}
MGL_EXPORT_PURE void* mgl_qt_widget(HMGL gr)
{
	mglCanvasQT *g = dynamic_cast<mglCanvasQT *>(gr);
	return g?g->QMGL:NULL;
}
int MGL_EXPORT mgl_qt_run()	{	return (qApp)?qApp->exec():-1;	}
//-----------------------------------------------------------------------------
uintptr_t MGL_EXPORT mgl_create_graph_qt_(const char *title, int l)
{
	char *s = new char[l+1];	memcpy(s,title,l);	s[l]=0;
	uintptr_t t = uintptr_t(mgl_create_graph_qt(0,s,0,0));
	delete []s;	return t;
}
int MGL_EXPORT mgl_qt_run_()	{	return mgl_qt_run();	}
//-----------------------------------------------------------------------------
