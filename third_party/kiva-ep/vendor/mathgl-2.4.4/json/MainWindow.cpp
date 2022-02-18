#include "MainWindow.hpp"
#include "ui_MainWindow.h"
#include "mgl2/config.h"

#include <QWebFrame>
#include <QNetworkDiskCache>
#include <QDesktopServices>
//-----------------------------------------------------------------------------
int main(int argc, char *argv[])
{
	QApplication a(argc, argv);
	MainWindow w;
	w.show();
	return a.exec();
}
//-----------------------------------------------------------------------------
MainWindow::MainWindow(QWidget* const parent) : QMainWindow(parent), ui(new Ui::MainWindow)
{
	ui->setupUi(this);

	// configure webkit
	QWebSettings::globalSettings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::JavascriptEnabled, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::LocalContentCanAccessRemoteUrls, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::LocalContentCanAccessFileUrls, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::LocalContentCanAccessRemoteUrls, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::JavascriptCanOpenWindows, true);
	QWebSettings::globalSettings()->setAttribute(QWebSettings::JavascriptCanAccessClipboard, true);

	// create non-cached QNetworkAccessManager and assign to webview
	QNetworkAccessManager* manager = new QNetworkAccessManager(this);
	QNetworkDiskCache* diskCache = new QNetworkDiskCache();
#ifdef MGL_USE_QT5
	const QString location = QStandardPaths::writableLocation(QStandardPaths::CacheLocation);
#else
	const QString location = QDesktopServices::storageLocation(QDesktopServices::CacheLocation);
#endif
	diskCache->setCacheDirectory(location);
	diskCache->setMaximumCacheSize(0);
	manager->setCache(diskCache);
	ui->webView->page()->setNetworkAccessManager(manager);

	// inject backend object each time javascript object is cleared
	connect(ui->webView->page()->mainFrame(), SIGNAL(javaScriptWindowObjectCleared()), this, SLOT(injectBackendObject()));
	// set url to load
	ui->webView->load(QUrl(QString("file:///%1/../../json/%2").arg(qApp->applicationDirPath()).arg("index.html")));
}
//-----------------------------------------------------------------------------
void MainWindow::injectBackendObject()
{
	ui->webView->page()->mainFrame()->addToJavaScriptWindowObject("globalBackend", &_backend);
}
//-----------------------------------------------------------------------------
MainWindow::~MainWindow()	{	delete ui;	}
//-----------------------------------------------------------------------------
