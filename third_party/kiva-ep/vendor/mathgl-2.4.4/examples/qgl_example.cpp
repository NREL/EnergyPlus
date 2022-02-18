/***************************************************************************
 * qt_example.cpp is part of Math Graphic Library
 * Copyright (C) 2007-2016 Alexey Balakin <mathgl.abalakin@gmail.ru>       *
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
#include "qgl_example.h"
#include <QApplication>
//#include <QtOpenGL>
//-----------------------------------------------------------------------------
MainWindow::MainWindow(QWidget *parent) : QGLWidget(parent)	{	gr=0;	}
//-----------------------------------------------------------------------------
MainWindow::~MainWindow()	{	if(gr)	delete gr;	}
//-----------------------------------------------------------------------------
void MainWindow::initializeGL()	// recreate instance of MathGL core
{
	if(gr)	delete gr;
	gr = new mglGraph(1);	// use '1' for argument to force OpenGL output in MathGL
}
//-----------------------------------------------------------------------------
void MainWindow::resizeGL(int w, int h) // standard resize replace
{
	QGLWidget::resizeGL(w, h);
	glViewport (0, 0, w, h);
}
//-----------------------------------------------------------------------------
void MainWindow::paintGL()	// main drawing function
{
	gr->Clf();	// clear previous OpenGL primitives
	gr->SubPlot(1,1,0);
	gr->Rotate(40,60);
	gr->Light(true);
	gr->AddLight(0,mglPoint(0,0,10),mglPoint(0,0,-1));
	gr->Axis();
	gr->Box();
	gr->FPlot("sin(pi*x)","i2");
	gr->FPlot("cos(pi*x)","|");
	gr->FSurf("cos(2*pi*(x^2+y^2))");
	gr->Finish();
	swapBuffers();	// show output on the screen
}
//-----------------------------------------------------------------------------
int main(int argc, char *argv[])	// create application
{
	mgl_textdomain(argv?argv[0]:NULL,"");
	QApplication a(argc, argv);
	MainWindow w;
	w.show();
	return a.exec();
}
//-----------------------------------------------------------------------------
