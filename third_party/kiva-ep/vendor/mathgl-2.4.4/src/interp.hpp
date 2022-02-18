//-----------------------------------------------------------------------------
template <class Treal> Treal mglLineart(const Treal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z)
{
	if(!a || nx<1 || ny<1 || nz<1)	return 0;
	Treal b=0,dx,dy,dz,b1,b0;
	if(x<0 || y<0 || z<0 || x>nx-1 || y>ny-1 || z>nz-1)
		return 0;
	if(nz>1 && z!=floor(z))		// 3d interpolation
	{
		long kx=long(x), ky=long(y), kz=long(z);
		dx = x-mreal(kx);	dy = y-mreal(ky);	dz = z-mreal(kz);

		long i0 = kx+nx*(ky+ny*kz);
		b0 = a[i0]*(mreal(1)-dx-dy+dx*dy) + dx*(mreal(1)-dy)*a[i0+1] +
			dy*(mreal(1)-dx)*a[i0+nx] + dx*dy*a[i0+nx+1];
		i0 = kx+nx*(ky+ny*(kz+1));
		b1 = a[i0]*(mreal(1)-dx-dy+dx*dy) + dx*(mreal(1)-dy)*a[i0+1] +
			dy*(mreal(1)-dx)*a[i0+nx] + dx*dy*a[i0+nx+1];
		b = b0 + dz*(b1-b0);
	}
	else if(ny>1 && y!=floor(y))	// 2d interpolation
	{
		long kx=long(x), ky=long(y);
		dx = x-kx;	dy=y-ky;
		long i0 = kx+nx*ky;
		b = a[i0]*(mreal(1)-dx-dy+dx*dy) + dx*(mreal(1)-dy)*a[i0+1] +
			dy*(mreal(1)-dx)*a[i0+nx] + dx*dy*a[i0+nx+1];
	}
	else if(nx>1 && x!=floor(x))	// 1d interpolation
	{
		long kx = long(x);
		b = a[kx] + (x-kx)*(a[kx+1]-a[kx]);
	}
	else						// no interpolation
		b = a[long(x+nx*(y+ny*z))];
	return b;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mgl_spline3t(const Treal y[4], long n, mreal dx, Treal &dy)
{
	Treal d[3];
	d[0] = -(y[2]-mreal(4)*y[1]+mreal(3)*y[0])/mreal(2);
	d[1] = (y[2]-y[0])/mreal(2);
	d[2] = (y[3]-y[1])/mreal(2);

	Treal t0 = (y[2]+y[0])/mreal(2)-y[1];
	Treal t1 = (y[3]+y[1])/mreal(2)-y[2];
	Treal f0 = y[n], d0 = d[n], res = 0;
	if(n==1)
	{
		Treal df = y[2]-f0, d1 = d[2];
		Treal b3 = mreal(10)*df+t1-mreal(3)*t0-mreal(4)*d1-mreal(6)*d0;
		Treal b4 = mreal(-15)*df-mreal(2)*t1+mreal(3)*t0+mreal(7)*d1+mreal(8)*d0;
		Treal b5 = mreal(6)*df+t1-t0-mreal(3)*d1-mreal(3)*d0;
		dy = d0 + dx*(mreal(2)*t0+dx*(mreal(3)*b3+dx*(mreal(4)*b4+dx*mreal(5)*b5)));
//		d2y = mreal(2)*t0 + dx*(mreal(6)*b3+dx*(mreal(12)*b4+dx*mreal(20)*b5));	// 2nd derivative for future
		res = f0 + dx*(d0+dx*(t0+dx*(b3+dx*(b4+dx*b5))));
	}
	else if(n<1)
	{	res = f0 + dx*(d0+dx*t0);	dy = d0+dx*t0*mreal(2);	}
	else
	{	res = f0 + dx*(d0+dx*t1);	dy = d0+dx*t1*mreal(2);	}
	return res;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mgl_spline3st(const Treal y[4], long n, mreal dx)
{
	Treal d[3];
	d[0] = -(y[2]-mreal(4)*y[1]+mreal(3)*y[0])/mreal(2);
	d[1] = (y[2]-y[0])/mreal(2);
	d[2] = (y[3]-y[1])/mreal(2);

	Treal f0 = y[n], d0 = d[n], res;
	Treal t0 = (y[2]+y[0])/mreal(2)-y[1];
	Treal t1 = (y[3]+y[1])/mreal(2)-y[2];
	if(n==1)
	{
		Treal df = y[2]-f0, d1 = d[2];
		Treal b3 = mreal(10)*df+t1-mreal(3)*t0-mreal(4)*d1-mreal(6)*d0;
		Treal b4 = mreal(-15)*df-mreal(2)*t1+mreal(3)*t0+mreal(7)*d1+mreal(8)*d0;
		Treal b5 = mreal(6)*df+t1-t0-mreal(3)*d1-mreal(3)*d0;
		res = f0 + dx*(d0+dx*(t0+dx*(b3+dx*(b4+dx*b5))));
	}
	else	res = f0 + dx*(d0+dx*(n<1?t0:t1));
	return res;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mglSpline1t(const Treal *a, long nx, mreal x, Treal *dx=0)
{
	Treal r,d;
	if(nx>3)
	{
		long k = long(x);
		if(k>0 && k<nx-2)	r = mgl_spline3t<Treal>(a+k-1, 1, x-k, d);
		else if(k<1)		r = mgl_spline3t<Treal>(a, 0, x, d);
		else	r = mgl_spline3t<Treal>(a+nx-4, 2, x+2-nx, d);
	}
	else if(nx<2)	{	d=0;	r = a[0];	}
	else if(nx==2)	{	d=a[1]-a[0];	r = a[0]+(a[1]-a[0])*x;	}
	else	// nx==3
	{
		Treal b1=-(a[2]-mreal(4)*a[1]+mreal(3)*a[0])/mreal(2), b2=(a[2]-mreal(2)*a[1]+a[0])/mreal(2);
		d = b1+mreal(2)*b2*x;	r = a[0]+x*(b1+b2*x);
	}
	if(dx)	*dx=d;
	return r;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mglSpline1st(const Treal *a, long nx, mreal x)
{
	Treal r;
	if(nx>3)
	{
		long k = long(x);
		if(k>0 && k<nx-2)	r = mgl_spline3st<Treal>(a+k-1, 1, x-k);
		else if(k<1)		r = mgl_spline3st<Treal>(a, 0, x);
		else	r = mgl_spline3st<Treal>(a+nx-4, 2, x+2-nx);
	}
	else if(nx<2)	r = a[0];
	else if(nx==2)	r = a[0]+(a[1]-a[0])*x;
	else	// nx==3
	{
		Treal b1=-(a[2]-mreal(4)*a[1]+mreal(3)*a[0])/mreal(2), b2=(a[2]-mreal(2)*a[1]+a[0])/mreal(2);
		r = a[0]+x*(b1+b2*x);
	}
	return r;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mglSpline3t(const Treal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z, Treal *dx=0, Treal *dy=0, Treal *dz=0)
{
//	if(!a || nx<1 || ny<1 || nz<1)	return 0;	// NOTE remove this line because this should already checked
	Treal gx=0,gy=0,gz=0;
	x = x>0 ?(x<nx-1 ? x:nx-1):0;
	y = y>0 ?(y<ny-1 ? y:ny-1):0;
	z = z>0 ?(z<nz-1 ? z:nz-1):0;
	Treal b;
	if(nz>1)		// 3d interpolation
	{
		Treal tz[4], yz[4], xz[4];
		long kz=long(z)-1, mz, k=long(y)-1, m;
		if(nz>3)
		{	mz = 4;	kz = kz>=0?kz:0;
			if(kz>nz-4)	kz = nz-4;	}
		else	{	mz = nz;	kz=0;	}
		if(ny>3)
		{	m = 4;	k = k>=0?k:0;
			if(k>ny-4)	k = ny-4;	}
		else	{	m = ny;	k=0;	}
		for(long j=0;j<mz;j++)
		{
			Treal t[4], d[4];
			for(long i=0;i<m;i++)
				t[i] = mglSpline1t<Treal>(a+nx*(i+k+ny*(j+kz)),nx,x,d+i);
			tz[j] = mglSpline1t<Treal>(t,m,y-k,yz+j);
			xz[j] = mglSpline1t<Treal>(d,m,y-k,0);
		}
		b = mglSpline1t<Treal>(tz,mz,z-kz,&gz);
		gx = mglSpline1t<Treal>(xz,mz,z-kz,0);
		gy = mglSpline1t<Treal>(yz,mz,z-kz,0);
	}
	else if(ny>1)	// 2d interpolation
	{
		Treal t[4], d[4];
		long k = long(y)-1, m;
		if(ny>3)
		{	m = 4;	k = k>=0?k:0;	if(k>ny-4)	k = ny-4;	}
		else	{	m = ny;	k=0;	}
		for(long i=0;i<m;i++)
			t[i] = mglSpline1t<Treal>(a+nx*(i+k),nx,x,d+i);
		b = mglSpline1t<Treal>(t,m,y-k,&gy);
		gx = mglSpline1t<Treal>(d,m,y-k,0);
	}
	else	// 1d interpolation
		b = mglSpline1t<Treal>(a,nx,x,&gx);
	if(dx)	*dx=gx;
	if(dy)	*dy=gy;
	if(dz)	*dz=gz;
	return b;
}
//-----------------------------------------------------------------------------
template <class Treal> Treal mglSpline3st(const Treal *a, long nx, long ny, long nz, mreal x, mreal y, mreal z)
{
//	if(!a || nx<1 || ny<1 || nz<1)	return 0;	// NOTE remove this line because this should already checked
	x = x>0 ?(x<nx-1 ? x:nx-1):0;
	y = y>0 ?(y<ny-1 ? y:ny-1):0;
	z = z>0 ?(z<nz-1 ? z:nz-1):0;
	Treal b;
	if(nz>1)		// 3d interpolation
	{
		Treal tz[4], t[4];
		long kz=long(z)-1, mz, k=long(y)-1, m;
		if(nz>3)
		{	mz = 4;	kz = kz>=0?kz:0;
			if(kz>nz-4)	kz = nz-4;	}
		else	{	mz = nz;	kz=0;	}
		if(ny>3)
		{	m = 4;	k = k>=0?k:0;
			if(k>ny-4)	k = ny-4;	}
		else	{	m = ny;	k=0;	}
		for(long j=0;j<mz;j++)
		{
			for(long i=0;i<m;i++)
				t[i] = mglSpline1st<Treal>(a+nx*(i+k+ny*(j+kz)),nx,x);
			tz[j] = mglSpline1st<Treal>(t,m,y-k);
		}
		b = mglSpline1st<Treal>(tz,mz,z-kz);
	}
	else if(ny>1)	// 2d interpolation
	{
		Treal t[4];
		long k = long(y)-1, m;
		if(ny>3)
		{	m = 4;	k = k>=0?k:0;
			if(k>ny-4)	k = ny-4;	}
		else	{	m = ny;	k=0;	}
		for(long i=0;i<m;i++)
			t[i] = mglSpline1st<Treal>(a+nx*(i+k),nx,x);
		b = mglSpline1st<Treal>(t,m,y-k);
	}
	else	// 1d interpolation
		b = mglSpline1st<Treal>(a,nx,x);
	return b;
}
//-----------------------------------------------------------------------------
template <class Treal> void mgl_gspline_init(long n, const mreal *x, const Treal *v, Treal *c)
{	// c must have size 5*(n-1) !!!
//	if(n<2)	return;	// NOTE remove this line because this should already checked
	Treal *a = new Treal[n], *b = new Treal[n];
	for(long i=0;i<n-1;i++)	// basic coefficients
	{	c[5*i] = x[i+1]-x[i];	c[5*i+1] = v[i];	}
	// progonka
	a[0] = -0.5;	b[0] = mreal(1.5)*(v[1]-v[0])/(x[1]-x[0]);
	for(long i=1;i<n-1;i++)
	{
		mreal h0 = x[i]-x[i-1], h1 = x[i+1]-x[i];
		Treal r = mreal(1)/(2/h0+2/h1 + a[i-1]/h0);
		a[i] = - r/h1;
		b[i] = ((3/h0/h0)*(v[i]-v[i-1]) + (1/h1/h1)*(v[i+1]-v[i]) + a[i-1]/h0)*r;
	}
	b[n-1] = ( (6/(x[n-1]-x[n-2]))*(v[n-1]-v[n-2]) - mreal(2)*b[n-2] )/(mreal(4)+mreal(2)*a[n-2]);
	for(long i=n-2;i>=0;i--)	b[i] += a[i]*b[i+1];
	// no spline coefficients
	for(long i=0;i<n-1;i++)
	{
		c[5*i+2] = b[i];
		mreal h = 1/(x[i+1]-x[i]), h2 = h*h;
		c[5*i+3] = (3*h2)*(v[i+1]-v[i]) - (b[i+1]+b[i]+b[i])*h;
		c[5*i+4] = (2*h2*h)*(v[i]-v[i+1]) + (b[i+1]+b[i])*h2;
	}
	delete []a;	delete []b;
}
//-----------------------------------------------------------------------------
struct mglEqTxT
{
	std::vector<std::string> str;
	HAEX *eqC;
	HMEX *eqR;
	const char *var;

	mglEqTxT(const char *vars=0):eqC(0),eqR(0),var(vars)	{}
	~mglEqTxT()
	{
		if(eqR)	{	for(size_t i=0;i<str.size();i++)	mgl_delete_expr(eqR[i]);	delete []eqR;	}
		if(eqC)	{	for(size_t i=0;i<str.size();i++)	mgl_delete_cexpr(eqC[i]);	delete []eqC;	}
	}
	void FillStr(const char *eqs)
	{
		const char *f=eqs;
		while(1)
		{
			const char *g = strchr(f,';');
			if(g)	str.push_back(std::string(f,g-f));
			else	{	str.push_back(f);	break;	}
			f = g+1;
		}
	}
	void FillReal(const char *eqs)
	{
		FillStr(eqs);	size_t n = str.size();
		if(n==0)	return;
		eqR = new HMEX[n];
		for(size_t i=0;i<n;i++)	eqR[i] = mgl_create_expr(str[i].c_str());
	}
	void FillCmplx(const char *eqs)
	{
		FillStr(eqs);	size_t n = str.size();
		if(n==0)	return;
		eqC = new HAEX[n];
		for(size_t i=0;i<n;i++)	eqC[i] = mgl_create_cexpr(str[i].c_str());
	}
};
//-----------------------------------------------------------------------------
