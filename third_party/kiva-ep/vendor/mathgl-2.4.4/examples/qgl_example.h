#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QGLWidget>
#include <mgl2/mgl.h>

class MainWindow : public QGLWidget
{
	Q_OBJECT
protected:
	mglGraph *gr;			// pointer to MathGL core class
	void resizeGL(int nWidth, int nHeight);	// Method called after each window resize
	void paintGL();			// Method to display the image on the screen
	void initializeGL();	// Method to initialize OpenGL
public:
	MainWindow(QWidget *parent = 0);
	~MainWindow();
};

#endif // MAINWINDOW_H
