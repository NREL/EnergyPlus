#ifdef WIN32
#include <windows.h>
#endif
#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif
#include "mgl2/opengl.h"
#include "mgl2/mgl_cf.h"
#include <algorithm>
//-----------------------------------------------------------------------------
/// Create mglGraph object in OpenGL mode.
HMGL MGL_EXPORT mgl_create_graph_gl()
{	return new mglCanvasGL;	}
/// Create mglGraph object in OpenGL mode.
uintptr_t MGL_EXPORT mgl_create_graph_gl_()
{	return uintptr_t(new mglCanvasGL);	}
//-----------------------------------------------------------------------------
mglCanvasGL::mglCanvasGL() : mglCanvas(1,1)
{	Clf();	Zoom(0,0,1,1);	set(MGL_FULL_CURV);	}
//-----------------------------------------------------------------------------
mglCanvasGL::~mglCanvasGL(){}
//-----------------------------------------------------------------------------
void set_pen(unsigned style, mreal width, mreal pos)
{
	if(style==0)	return;
	unsigned long pdef = style*0x100010001;
	pdef >>= long(32*pos)%32;	// NOTE try to bypass OpenGL limitations
	style = pdef & 0xffff;
	width *= 20;
	if(style!=0xffff)
	{
		glEnable(GL_LINE_STIPPLE);
		glLineStipple(int(width+0.5),style);
	}
	else	glDisable(GL_LINE_STIPPLE);
	if(width>1)	glLineWidth(width);	// NOTE bypass bug on some drivers, where width>1 must be
	else		glLineWidth(1);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Finish()
{
#if MGL_USE_DOUBLE
#define MGL_GL_TYPE	GL_DOUBLE
#else
#define MGL_GL_TYPE	GL_FLOAT
#endif

	// Try to add smoothing
	glEnable(GL_LINE_SMOOTH);
	glEnable(GL_POLYGON_SMOOTH);
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
	glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
	glEnable(GL_BLEND);
	if((Flag&3)==1)	glBlendFunc(GL_DST_COLOR, GL_ZERO);
	else if((Flag&3)==2) glBlendFunc(GL_SRC_ALPHA, GL_ONE);
	else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	if(Prm.size()>0)
	{
		PreparePrim(0);
/*		glVertexPointer(3, MGL_GL_TYPE, sizeof(mglPnt), &(Pnt[0].x));	// something wrong with arrays
		glNormalPointer(MGL_GL_TYPE, sizeof(mglPnt), &(Pnt[0].u));
		glColorPointer(4, MGL_GL_TYPE, sizeof(mglPnt), &(Pnt[0].r));
		glEnableClientState(GL_VERTEX_ARRAY);
		glEnableClientState(GL_NORMAL_ARRAY);
		glEnableClientState(GL_COLOR_ARRAY);*/

		int pdef=PDef;
		mreal ss=pPos, ww=PenWidth;
		mglPrim p;
		for(size_t i=0;i<Prm.size();i++)
		{
			p=GetPrm(i);	PDef=p.n3;	pPos=p.s;	PenWidth=p.w;
			long n1=p.n1, n2=p.n2, n3=p.n3, n4=p.n4;
			mglDrawReg d;	d.set(this,1,1,0);
			switch(p.type)
			{
/*			case 0:	mark_draw(Pnt[n1],n4,p.s,0);	break;
			case 1:	line_draw(n1,n2);	break;
			case 2:	trig_draw(n1,n2,n3);	break;
			case 3:	quad_draw(n1,n2,n3,n4);	break;*/
			case 0:	mark_draw(Pnt[n1],n4,p.s,&d);	break;
			case 1:	line_draw(Pnt[n1],Pnt[n2],&d);	break;
			case 2:	trig_draw(Pnt[n1],Pnt[n2],Pnt[n3],true,&d);	break;
			case 3:	quad_draw(Pnt[n1],Pnt[n2],Pnt[n3],Pnt[n4],&d);	break;
			case 4:	glyph_draw(p,&d);	break;
			}
		}
		PDef=pdef;	pPos=ss;	PenWidth=ww;
	}
	glFinish();
}
//-----------------------------------------------------------------------------
bool mglCanvasGL::Alpha(bool enable)
{
	if(enable)
	{
		set(MGL_ENABLE_ALPHA);
		glDisable(GL_DEPTH_TEST);
		glEnable(GL_ALPHA_TEST);
		glEnable(GL_BLEND);
		if((Flag&3)==1)	glBlendFunc(GL_DST_COLOR, GL_ZERO);
		else if((Flag&3)==2) glBlendFunc(GL_SRC_ALPHA, GL_ONE);
		else glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	}
	else
	{
		clr(MGL_ENABLE_ALPHA);
		glEnable(GL_DEPTH_TEST);
		glDisable(GL_ALPHA_TEST);
//		glDisable(GL_BLEND);
	}
	return mglCanvas::Alpha(enable);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::AddLight(int n,mglPoint r,mglPoint d,char cc, mreal br,mreal ap)
{
	mglColor c(cc);
	float amb[4],dif[4],spc[4], pos[4];
	bool inf = mgl_isnan(r.x);
	if(n<0 || n>7)	{	SetWarn(mglWarnLId,"AddLight");	return;	}
	if(c.Valid())
	{
		spc[0] = br*c.r;	spc[1] = br*c.g;	spc[2] = br*c.b;
		amb[0] = AmbBr*c.r;	amb[1] = AmbBr*c.g;	amb[2] = AmbBr*c.b;
	}
	else
	{
		spc[0] = spc[1] = spc[2] = br;
		amb[0] = amb[1] = amb[2] = AmbBr;
	}
	ap = 90-180*atan(fabs(ap))/M_PI;
	dif[0] = dif[1] = dif[2] = DifBr;
	dif[3] = amb[3] = spc[3] = 1.;
	if(inf)
	{	pos[0] = d.x;	pos[1] = d.y;	pos[2] = d.z;	pos[3] = 0;	}
	else
	{	pos[0] = r.x;	pos[1] = r.y;	pos[2] = r.z;	pos[3] = 1;	}

	glShadeModel(GL_SMOOTH);
	glLightfv(GL_LIGHT0+n, GL_AMBIENT, amb);
	glLightfv(GL_LIGHT0+n, GL_DIFFUSE, dif);
	glLightfv(GL_LIGHT0+n, GL_SPECULAR, spc);
	glLightfv(GL_LIGHT0+n, GL_POSITION, pos);
	if(!inf)
	{
//		float dir[4]={d.x, d.y, d.z, 0};
//		glLightfv(GL_LIGHT0+n, GL_SPOT_DIRECTION, dir);
//		glLightf(GL_LIGHT0+n, GL_SPOT_CUTOFF, ap);
	}
	glEnable(GL_LIGHT0+n);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Light(int n, bool enable)
{
	if(enable)	glEnable(GL_LIGHT0+n);
	else		glDisable(GL_LIGHT0+n);
}
//-----------------------------------------------------------------------------
bool mglCanvasGL::Light(bool enable)
{
	if(enable)	{	glEnable(GL_LIGHTING);	glEnable(GL_NORMALIZE);}
	else		{	glDisable(GL_LIGHTING);	glDisable(GL_NORMALIZE);	}
	return mglCanvas::Light(enable);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::LightScale(const mglMatrix *M)
{
	mglCanvas::LightScale(M);
	GLenum ll[8] = {GL_LIGHT0,GL_LIGHT1,GL_LIGHT2,GL_LIGHT3,GL_LIGHT4,
			GL_LIGHT5,GL_LIGHT6,GL_LIGHT7};
	float pos[4]={0,0,0,0};
	for(int i=0;i<8;i++)	// NOTE only global light is used in OpenGL mode
	{
		pos[0] = light[i].p.x;
		pos[1] = light[i].p.y;
		pos[2] = light[i].p.z;
		if(light[i].n)	glLightfv(ll[i], GL_POSITION, pos);
	}
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Zoom(mreal x1, mreal y1, mreal x2, mreal y2)
{
	glMatrixMode(GL_PROJECTION);//GL_PROJECTION GL_VIEWPORT GL_MODELVIEW
	glLoadIdentity();
	glScaled(x2-x1,y2-y1,1);
	glTranslated((x1+x2-1)/2,(y1+y2-1)/2,0);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::View(mreal TetX,mreal TetY,mreal TetZ)
{
	glMatrixMode(GL_PROJECTION);//GL_PROJECTION GL_VIEWPORT GL_MODELVIEW
	glRotated(TetX,1.,0.,0.);
	glRotated(TetY,0.,1.,0.);
	glRotated(TetZ,0.,0.,1.);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Fog(mreal d, mreal)
{
	if(d>0)
	{
		float back[4]={BDef[0]/255.f,BDef[1]/255.f,BDef[2]/255.f,BDef[3]/255.f};
		glFogf(GL_FOG_MODE,GL_EXP);
		glFogf(GL_FOG_DENSITY,5*d);
		glFogfv(GL_FOG_COLOR,back);
		glEnable(GL_FOG);
	}
	else	glDisable(GL_FOG);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Clf(mglColor Back)
{
	mglCanvas::Clf(Back);
	if(Back==NC)	Back.Set(BDef[0]/255.,BDef[1]/255.,BDef[2]/255.);
	gl_clf(Back);
}
//-----------------------------------------------------------------------------
void mglCanvasGL::Clf(const char *col)
{
	mglCanvas::Clf(col);
	gl_clf(mglColor(BDef[0]/255.,BDef[1]/255.,BDef[2]/255.));
}
//-----------------------------------------------------------------------------
void mglCanvasGL::gl_clf(mglColor Back)
{
	if(Back==NC)	Back = WC;
//	glDepthFunc(GL_LESS);
	glDepthFunc(GL_GREATER);
	glClearColor(Back.r,Back.g,Back.b,1.);
	glClearDepth(-10.);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_COLOR_MATERIAL);

	glMatrixMode(GL_MODELVIEW);//GL_MODELVIEW GL_VIEWPORT GL_PROJECTION
	glLoadIdentity();
	glScaled(2,2,2);
	glTranslated(-0.5,-0.5,-0.5);

//	float dif[4]={DifBr,DifBr,DifBr,1};
//	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, dif);
	float spc[4]={1,1,1,1};
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, spc);
	glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, DifBr);
}
//-----------------------------------------------------------------------------
/*void mglCanvasGL::EndFrame()
{
//	mglGraph::EndFrame();
	glEndList();
}
//-----------------------------------------------------------------------------
int mglCanvasGL::NewFrame()
{
	Clf();	Identity();
	glNewList(CurFrameId,GL_COMPILE);
	CurFrameId++;
	return CurFrameId-1;
}*/
//-----------------------------------------------------------------------------
unsigned char **mglCanvasGL::GetRGBLines(long &width, long &height, unsigned char *&f, bool alpha)
{
	long x, y, d = alpha ? 4:3;
	GLint w[4];
	glGetIntegerv(GL_VIEWPORT,w);
	x=w[0];	y=w[1];	width=w[2];	height=w[3];
	unsigned char **p;

	p = (unsigned char **)malloc(height * sizeof(unsigned char *));
	f = (unsigned char *) malloc(width*height * sizeof(unsigned char)*d);
	for(long i=0;i<height;i++)	p[i] = f+d*width*(height-1-i);
	glReadBuffer(GL_FRONT);
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glReadPixels(x, y, width, height, alpha ? GL_RGBA : GL_RGB, GL_UNSIGNED_BYTE, f);
	return p;
}
//-----------------------------------------------------------------------------
void mglCanvasGL::quad_draw(long k1, long k2, long k3, long k4)
{
	glBegin(GL_QUADS);
	glArrayElement(k1);	glArrayElement(k2);
	glArrayElement(k4);	glArrayElement(k3);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::trig_draw(long k1, long k2, long k3)
{
	glBegin(GL_TRIANGLES);
	glArrayElement(k1);	glArrayElement(k2);	glArrayElement(k3);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::line_draw(long k1, long k2)
{
	if(PDef==0)	return;
	set_pen(PDef,PenWidth, pPos);
	glBegin(GL_LINES);
	glArrayElement(k1);	glArrayElement(k2);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::quad_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, const mglPnt &p4, const mglDrawReg *)
{
	glBegin(GL_QUADS);
	glNormal3f(p1.u,p1.v,p1.w);	glColor4f(p1.r,p1.g,p1.b,p1.a);	glVertex3f(p1.x,p1.y,p1.z);
	glNormal3f(p2.u,p2.v,p2.w);	glColor4f(p2.r,p2.g,p2.b,p2.a);	glVertex3f(p2.x,p2.y,p2.z);
	glNormal3f(p4.u,p4.v,p4.w);	glColor4f(p4.r,p4.g,p4.b,p4.a);	glVertex3f(p4.x,p4.y,p4.z);
	glNormal3f(p3.u,p3.v,p3.w);	glColor4f(p3.r,p3.g,p3.b,p3.a);	glVertex3f(p3.x,p3.y,p3.z);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::trig_draw(const mglPnt &p1, const mglPnt &p2, const mglPnt &p3, bool, const mglDrawReg *)
{
	glBegin(GL_TRIANGLES);
	glNormal3f(p1.u,p1.v,p1.w);	glColor4f(p1.r,p1.g,p1.b,p1.a);	glVertex3f(p1.x,p1.y,p1.z);
	glNormal3f(p2.u,p2.v,p2.w);	glColor4f(p2.r,p2.g,p2.b,p2.a);	glVertex3f(p2.x,p2.y,p2.z);
	glNormal3f(p3.u,p3.v,p3.w);	glColor4f(p3.r,p3.g,p3.b,p3.a);	glVertex3f(p3.x,p3.y,p3.z);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::line_draw(const mglPnt &p1, const mglPnt &p2, const mglDrawReg *)
{
	if(PDef==0)	return;
	set_pen(PDef,PenWidth, pPos);
	glBegin(GL_LINES);
	glColor4f(p1.r,p1.g,p1.b,p1.a);	glVertex3f(p1.x,p1.y,p1.z);
	glColor4f(p2.r,p2.g,p2.b,p2.a);	glVertex3f(p2.x,p2.y,p2.z);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::pnt_draw(const mglPnt &p1, const mglDrawReg *)
{
	glBegin(GL_POINTS);
	glColor4f(p1.r,p1.g,p1.b,p1.a);	glVertex3f(p1.x,p1.y,p1.z);
	glEnd();
}
//-----------------------------------------------------------------------------
void mglCanvasGL::mark_draw(const mglPnt &q, char type, mreal size, mglDrawReg *d)
{
	mglPnt p0=q,p1=q,p2=q,p3=q;
	mreal ss=fabs(size);

	if(type=='.' || ss==0)
	{
		if(d)	d->PenWidth = ss?ss:sqrt(font_factor/400);
		pnt_draw(q,d);
	}
	else
	{
		if(d)
		{
			d->PDef = MGL_SOLID_MASK;	d->angle = 0;
			d->PenWidth*=fabs(50*size);
			if(d->PenWidth<1)	d->PenWidth=1;
		}
		if(!strchr("xsSoO",type))	ss *= 1.1;
		switch(type)
		{
		case 'P':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p3,d);	line_draw(p3,p0,d);
		case '+':
			p0.x = q.x-ss;	p0.y = q.y;	p1.x = q.x+ss;	p1.y = q.y;	line_draw(p0,p1,d);
			p2.x = q.x;	p2.y = q.y-ss;	p3.x = q.x;	p3.y = q.y+ss;	line_draw(p2,p3,d);
			break;
		case 'X':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p3,d);	line_draw(p3,p0,d);
		case 'x':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y+ss;	line_draw(p0,p1,d);
			p2.x = q.x+ss;	p2.y = q.y-ss;	p3.x = q.x-ss;	p3.y = q.y+ss;	line_draw(p2,p3,d);
			break;
		case 'S':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x-ss;	p1.y = q.y+ss;
			p2.x= q.x+ss;	p2.y= q.y+ss;	p3.x = q.x+ss;	p3.y = q.y-ss;
			quad_draw(p0,p1,p3,p2,d);
		case 's':
			p0.x = q.x-ss;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y-ss;
			p2.x = q.x+ss;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y+ss;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p3,d);	line_draw(p3,p0,d);
			break;
		case 'D':
			p0.x = q.x;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y;
			p2.x= q.x;	p2.y= q.y+ss;	p3.x = q.x-ss;	p3.y = q.y;
			quad_draw(p0,p1,p3,p2,d);
		case 'd':
			p0.x = q.x;	p0.y = q.y-ss;	p1.x = q.x+ss;	p1.y = q.y;
			p2.x = q.x;	p2.y = q.y+ss;	p3.x = q.x-ss;	p3.y = q.y;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p3,d);	line_draw(p3,p0,d);
			break;
		case 'Y':
			p1.x = q.x;	p1.y = q.y-ss;	line_draw(q,p1,d);
			p2.x = q.x-0.8*ss;	p2.y = q.y+0.6*ss;	line_draw(q,p2,d);
			p3.x = q.x+0.8*ss;	p3.y = q.y+0.6*ss;	line_draw(q,p3,d);
			break;
		case '*':
			p0.x = q.x-ss;		p0.y = q.y;
			p1.x = q.x+ss;		p1.y = q.y;	line_draw(p0,p1,d);
			p0.x = q.x-0.6*ss;	p0.y = q.y-0.8*ss;
			p1.x = q.x+0.6*ss;	p1.y = q.y+0.8*ss;	line_draw(p0,p1,d);
			p0.x = q.x-0.6*ss;	p0.y = q.y+0.8*ss;
			p1.x = q.x+0.6*ss;	p1.y = q.y-0.8*ss;	line_draw(p0,p1,d);
			break;
		case 'T':
			p0.x = q.x-ss;	p0.y = q.y-ss/2;
			p1.x = q.x+ss;	p1.y = q.y-ss/2;
			p2.x= q.x;		p2.y= q.y+ss;
			trig_draw(p0,p1,p2,false,d);
		case '^':
			p0.x = q.x-ss;	p0.y = q.y-ss/2;
			p1.x = q.x+ss;	p1.y = q.y-ss/2;
			p2.x= q.x;		p2.y= q.y+ss;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p0,d);	break;
		case 'V':
			p0.x = q.x-ss;	p0.y = q.y+ss/2;
			p1.x = q.x+ss;	p1.y = q.y+ss/2;
			p2.x= q.x;		p2.y= q.y-ss;
			trig_draw(p0,p1,p2,false,d);
		case 'v':
			p0.x = q.x-ss;	p0.y = q.y+ss/2;
			p1.x = q.x+ss;	p1.y = q.y+ss/2;
			p2.x= q.x;		p2.y= q.y-ss;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p0,d);	break;
		case 'L':
			p0.x = q.x+ss/2;	p0.y = q.y+ss;
			p1.x = q.x+ss/2;	p1.y = q.y-ss;
			p2.x= q.x-ss;		p2.y= q.y;
			trig_draw(p0,p1,p2,false,d);
		case '<':
			p0.x = q.x+ss/2;	p0.y = q.y+ss;
			p1.x = q.x+ss/2;	p1.y = q.y-ss;
			p2.x= q.x-ss;		p2.y= q.y;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p0,d);	break;
		case 'R':
			p0.x = q.x-ss/2;	p0.y = q.y+ss;
			p1.x = q.x-ss/2;	p1.y = q.y-ss;
			p2.x= q.x+ss;		p2.y= q.y;
			trig_draw(p0,p1,p2,false,d);
		case '>':
			p0.x = q.x-ss/2;	p0.y = q.y+ss;
			p1.x = q.x-ss/2;	p1.y = q.y-ss;
			p2.x= q.x+ss;		p2.y= q.y;
			line_draw(p0,p1,d);	line_draw(p1,p2,d);
			line_draw(p2,p0,d);	break;
		case 'O':
/*			for(long j=long(-ss);j<=long(ss);j++)	for(long i=long(-ss);i<=long(ss);i++)
			{
				long x=long(q.x)+i, y=long(q.y)+j;
				if(i*i+j*j>=ss*ss || !d || x<d->x1 || x>d->x2 || y<d->y1 || y>d->y2)	continue;
				if(cs[3])	pnt_plot(x,y,q.z+1,cs,d->ObjId);
			}*/
		case 'o':
			for(long i=0;i<=20;i++)	// TODO copy from mark_pix()?!
			{
				p0 = p1;	p1.x = q.x+ss*cos(i*M_PI/10);	p1.y = q.y+ss*sin(i*M_PI/10);
				if(i>0)	line_draw(p0,p1,d);
			}
			break;
		case 'C':
			pnt_draw(q,d);
			for(long i=0;i<=20;i++)
			{
				p0 = p1;	p1.x = q.x+ss*cos(i*M_PI/10);	p1.y = q.y+ss*sin(i*M_PI/10);
				if(i>0)	line_draw(p0,p1,d);
			}
			break;
		}
	}
}
//-----------------------------------------------------------------------------
void mglCanvasGL::glyph_fill(mreal phi, const mglPnt &pp, mreal f, const mglGlyph &g, const mglDrawReg *d)
{
	if(!g.trig || g.nt<=0)	return;
	const mreal co=cos(phi*M_PI/180), si=sin(phi*M_PI/180);
	mglPnt q0=pp, q1=pp, q2=pp;
	q0.u=q0.v=q1.u=q1.v=q2.u=q2.v=NAN;
	for(long ik=0;ik<g.nt;ik++)
	{
		long ii = 6*ik;	mreal x, y;
		x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
		q0.x = pp.x+(x*co+y*si)/2;	q0.y = pp.y+(y*co-x*si)/2;	ii+=2;
		x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
		q1.x = pp.x+(x*co+y*si)/2;	q1.y = pp.y+(y*co-x*si)/2;	ii+=2;
		x = pp.u+g.trig[ii]*f;	y = pp.v+g.trig[ii+1]*f;
		q2.x = pp.x+(x*co+y*si)/2;	q2.y = pp.y+(y*co-x*si)/2;
		trig_draw(q0,q1,q2,false,d);
	}
}
//-----------------------------------------------------------------------------
