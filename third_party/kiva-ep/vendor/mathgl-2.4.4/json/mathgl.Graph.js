/**
 * Graph - main MathGL class.
 */

/**
 * Constructor - create graph attached to specified canvas and working over provided backend.
 * @param backend {mathgl.Backend} implementation of backend interface
 * @param canvas {Canvas} canvas to plot graph on
 */
mathgl.Graph = function(canvas, backend) {
	this.__backend = backend;
	this.__canvas = canvas;
	this.__view = null;
	this.__geometry = null;
	// indicate whether rendering handlers are in the event queue
	this.__isDraftRenderingInScheduled = false;
	this.__isPreciseRenderingScheduled = false;
	// draft rendering finished timestamp
	this.__draftFinishedTimestamp = new Date();
	this.__backgroundFillStyle = '#EEEEFF';
	this.__preciseRenderingDelay = 700;

	this.__maxDraftPoints = 30000;
	this.__asp_scl=0;	// inertia of aspect scaling
	this.__fov = 0;		// perspective
	this.__x1 = 0;	this.__y1 = 0;	this.__z1 = 0;
	this.__x2 = 1;	this.__y2 = 1;	this.__z2 = 1;
	this.__activeTimeoutHandlers = [];

  // create view
  this.__view = new mathgl.View();
  // connect method which starts rendering to view object
  this.__view.setRenderLauncher(mathgl.bind(this.__renderStart, this));
  // connect pick point handler
  this.__view.setPickPointHandler(mathgl.bind(this.__pickPointHandler, this));
  // attach canvas to view
  this.__view.attachCanvas(this.__canvas);
};


/**
 * Method uses to wrap native JS setTimeout function to make possible deactivate active callbacks in destroy method
 *
 * @param func {Function} Callback function, will be executed after delay
 * @param delay {Number} Delay before callback call
 */
mathgl.Graph.prototype.__setTimeout = function(func, delay) {
	var me = this;
	var timeoutFunc = function() {
		func.call();
		var index = me.__activeTimeoutHandlers.indexOf(this.setTimeoutId);
		if (index > -1) {
			// remove timeout from activeTimeoutHandlers list
			me.__activeTimeoutHandlers.splice(index, 1);
		}
	}

	var timeoutId = setTimeout(mathgl.bind(timeoutFunc, timeoutFunc), delay);
	// keep timeout handler inside function
	timeoutFunc.setTimeoutId = timeoutId;
	this.__activeTimeoutHandlers.push(timeoutId);
};

/**
 * Load graph state from JSON string.
 * @param json {String} string in JSON format with previously saved state
 */
mathgl.Graph.prototype.load = function(json) {
	throw new Error("TODO");
}


/**
 * Save current graph state to JSON string.
 * @return {String} state serialized to JSON string
 */
mathgl.Graph.prototype.save = function() {
	throw new Error("TODO");
}


/**
 * background fill style setter
 * @param fillStyle something that will be accepted by canvas' 2d context as fill style, e.g. color, gradient, pattern.
 */
mathgl.Graph.prototype.setBackgroundFillStyle = function(fillStyle) {
	this.__backgroundFillStyle = fillStyle;
}


/** @return background fill style */
mathgl.Graph.prototype.backgroundFillStyle = function() {
	return this.__backgroundFillStyle;
}


/** called when user picks the point on the graph, point shall be somehow displayed/highlighted */
mathgl.Graph.prototype.__pickPointHandler = function(x, y) {
	var obj = this.__geometry;
	var xy = x*obj.width/this.__canvas.width + " " + y*obj.height/this.__canvas.height;
	// introduce zoom and view coomand for server side
	var zoom = "zoom "+(0.5-obj.pf/2)+" "+(0.5-obj.pf/2)+" "+(0.5+obj.pf/2)+" "+(0.5+obj.pf/2)+"\n";
	var view1 = "view 0 "+this.__view.__pitch*180/Math.PI+" 0"+"\n";
	var view2 = "view 0 0 "+(-this.__view.__yaw)*180/Math.PI+"\n";
	var persp = "perspective "+(-this.__fov)+"\n";
	// now ask server side for proper coordinates
	var res = this.__backend.coor(xy, zoom+view1+view2+persp+obj.mgl);
}


/** called when user shift axis range */
mathgl.Graph.prototype.shiftAxis = function(x, y, z) {
	var dx = x*(this.__x2-this.__x1), dy = y*(this.__y2-this.__y1), dz = z*(this.__z2-this.__z1)
	this.__x1 += dx; this.__x2 += dx;
	this.__y1 += dy; this.__y2 += dy;
	this.__z1 += dz; this.__z2 += dz;
	// introduce zoomaxis coomand for server side
	var zoom = "zoomaxis "+this.__x1+" "+this.__y1+" "+this.__z1+" "+this.__x2+" "+this.__y2+" "+this.__z2+"\n";

  this.loadGeometry(zoom + this.__geometry.mgl);
}


/** called when user shift axis range */
mathgl.Graph.prototype.zoomAxis = function(factor) {
	var d, c;
	d=(this.__x2-this.__x1)*factor/2; c=(this.__x2+this.__x1)/2;
	this.__x1 = c-d; this.__x2 = c+d;
	d=(this.__y2-this.__y1)*factor/2; c=(this.__y2+this.__y1)/2;
	this.__y1 = c-d; this.__y2 = c+d;
	d=(this.__z2-this.__z1)*factor/2; c=(this.__z2+this.__z1)/2;
	this.__z1 = c-d; this.__z2 = c+d;
	// introduce zoomaxis coomand for server side
	var zoom = "zoomaxis "+this.__x1+" "+this.__y1+" "+this.__z1+" "+this.__x2+" "+this.__y2+" "+this.__z2+"\n";

  this.loadGeometry(zoom + this.__geometry.mgl);
}


/** initiate the chains of rendering the geometry to the canvas */
mathgl.Graph.prototype.__renderStart = function() {
	// do nothing if processing is already started or no geometry
	if (!this.__isDraftRenderingInScheduled && this.__geometry) {
		// enqueue draft rendering step
		this.__isDraftRenderingInScheduled = true;
		this.__setTimeout(mathgl.bind(this.__renderDraft, this), 0);
	}
}


/** draft rendering */
mathgl.Graph.prototype.__renderDraft = function() {
	this.__drawMesh(false);
	this.__isDraftRenderingInScheduled = false;

	// enqueue precise rendering step
	if (!this.__isPreciseRenderingScheduled) {
		this.__isPreciseRenderingScheduled = true;
		this.__setTimeout(mathgl.bind(this.__renderPrecise, this), this.__preciseRenderingDelay);
	}
	this.__draftFinishedTimestamp = new Date();
}


/** precise rendering */
mathgl.Graph.prototype.__renderPrecise = function() {
	// do nothing if draft rendering is scheduled
	if (this.__isDraftRenderingInScheduled) {
		this.__isPreciseRenderingScheduled = false;
		return;
	}

	// check that enough time has passed since last occurance of draft rendering finished
	// rechedule pricese rendering if it is not
	var current = new Date();
	if (current - this.__draftFinishedTimestamp < this.__preciseRenderingDelay) {
		this.__setTimeout(mathgl.bind(this.__renderPrecise, this), this.__preciseRenderingDelay - (current - this.__draftFinishedTimestamp));
		return;
	}
	this.__drawMesh(true);
	this.__isPreciseRenderingScheduled = false;
}


/** fill canvas background */
mathgl.Graph.prototype.__drawBackground = function() {
	var c = this.__canvas.getContext("2d");
	var h = this.__canvas.height;
	var w = this.__canvas.width;
	c.fillStyle = this.__backgroundFillStyle;
	c.fillRect(0, 0 , w, h);
}


/** auxiliary function to draw mesh */
mathgl.Graph.prototype.__drawMesh = function(isPrecise) {
	var c = this.__canvas.getContext("2d");
	var m = this.__view.viewMatrix().inverse();
//	var vvv = $M([[1,0,0,1]]);
	var obj = this.__geometry;
	var h = this.__canvas.height;
	var fy = h / obj.height;
	var w = this.__canvas.width;
	var fx = w / obj.width;
//	var df=dx<dy?dx:dy;
	var dx, dy;
	if(fx<fy)	{	dx=fx;	dy=fx+(fy-fx)*this.__asp_scl;	}
	else		{	dy=fy;	dx=fy+(fx-fy)*this.__asp_scl;	}
	obj.pf = -m.e(4,3);
	obj.b = [dx*m.e(1,1),	dx*m.e(2,1),	dx*m.e(3,1),
			dy*m.e(1,2),	dy*m.e(2,2),	dy*m.e(3,2),
			m.e(1,3),		m.e(2,3),		m.e(3,3),
			w/2,			h/2,			obj.depth/2,
			fx,				fy,				1];
/*	obj.b = [dx*m.e(1,1),	dx*m.e(2,1),	dx*m.e(3,1),
			dy*m.e(1,2),	dy*m.e(2,2),	dy*m.e(3,2),
			m.e(1,3),		m.e(2,3),		m.e(3,3),
			w/2,			h/2,			obj.depth/2,
			dx,				dy,				1];	*/
	this.__drawBackground();

	if (!isPrecise) {
		obj.fast = 1;
		obj.good = 0;
		this.__mgl_draw_fast(obj,c,1);
	} else {
		obj.fast = 0;
		obj.good = 1;
		this.__mgl_draw_good(obj,c,1);
	}
}


/** perform fast drawing */
mathgl.Graph.prototype.__mgl_draw_fast = function(obj, ctx, skip) {
	if(obj.fast==0)	return;
	this.__mgl_prepare(obj,skip);	// update coordinates
	var di = 1 + Math.round(obj.nprim / this.__maxDraftPoints);
	// for each primitive skipping superfluous
	for(var i=0;i<obj.nprim;i ++)
	{
		var prim = obj.prim[i];
		var n1 = prim[1], nn = obj.pp[n1];
		if(prim[0]==1 || obj.pnts[n1][3]<0)
			this.__mgl_draw_prim(obj,ctx,prim,Math.abs(obj.b[12]));
		else if(obj.prim[i][0]<4 && i%di==0)
		{
			ctx.fillStyle = obj.prim[i][10];
			ctx.fillRect(nn[0], nn[1], 2, 2);
		}
	}
}


/** perform high-quality drawing */
mathgl.Graph.prototype.__mgl_draw_good = function(obj, ctx, skip) {
	obj.fast = 0;
	this.__mgl_prepare(obj,skip);	// update coordinates
//	var scl = 1/Math.abs(obj.z[1]-obj.z[0]);
	// NOTE: this valid only for current zoom/view. In general case it should be more complicated
	var s1 = Math.sqrt(obj.b[0]*obj.b[0]+obj.b[1]*obj.b[1]+obj.b[2]*obj.b[2]);
	var s2 = Math.abs(obj.b[12]);
	for(var i=0;i<obj.nprim;i++)	// for each primitive
	{
		var prim = obj.prim[i];
		var scl = s1*this.__mgl_pf(obj, prim[9]);
		if(obj.pnts[prim[1]][3]<0)	scl = s2;
		this.__mgl_draw_prim(obj,ctx,prim,scl);
	}
}


mathgl.Graph.prototype.__mgl_pf = function(obj, z) {
	//	return 1/obj.pf;
	return (1-this.__fov/1.37)/obj.pf/(1-this.__fov*z/obj.depth);	// TODO: check calc coordinates!!!
	//	return 1/(1+obj.pf*(1-z/obj.depth));
}


/** perform high-quality drawing */
mathgl.Graph.prototype.__mgl_draw_prim = function(obj, ctx, prim, scl) {
	var n1 = prim[1], n2 = prim[2];
	var n3 = prim[3], n4 = prim[4];
	var pp = obj.pp;
	var deg = Math.PI/180;  //0.017453293;

	ctx.strokeStyle = prim[10];
	ctx.fillStyle = prim[10];
	ctx.lineWidth = 1;
	switch(prim[0])		// draw it depending on its type
	{
	case 0: // marks
		ctx.lineWidth = prim[7]*prim[6]*5e-4;
		this.__mgl_draw_mark(ctx, pp[n1][0], pp[n1][1], n4, prim[6]/100, scl);
		break;
	case 1: // lines
		ctx.beginPath();
		ctx.moveTo(pp[n1][0],pp[n1][1]);
		ctx.lineTo(pp[n2][0],pp[n2][1]);
		ctx.lineWidth = prim[7]/100;
		ctx.stroke();	break;
	case 2: // triangles
		ctx.beginPath();
		ctx.moveTo(pp[n1][0],pp[n1][1]);
		ctx.lineTo(pp[n2][0],pp[n2][1]);
		ctx.lineTo(pp[n3][0],pp[n3][1]);
		ctx.closePath();	ctx.fill();	break;
	case 3: // quadrangles
		ctx.beginPath();
		ctx.moveTo(pp[n1][0],pp[n1][1]);
		ctx.lineTo(pp[n2][0],pp[n2][1]);
		ctx.lineTo(pp[n4][0],pp[n4][1]);
		ctx.lineTo(pp[n3][0],pp[n3][1]);
		ctx.closePath();
		// NOTE: look as alpha is disabled for lines
		// So, next code should be only for the case alpha=false
		if(prim[10].charAt(0)=='#')	ctx.stroke();
		ctx.fill();	break;
	case 4: // glyphs
		var t=prim[7]*deg/100;
		var xx=obj.coor[n2][2]/100,yy=-obj.coor[n2][3]/100,zz=obj.coor[n2][4]/100;
		var xc = obj.b[0]*xx + obj.b[1]*yy + obj.b[2]*zz;
		var yc = obj.b[3]*xx + obj.b[4]*yy + obj.b[5]*zz;
		var zc = obj.b[6]*xx + obj.b[7]*yy + obj.b[8]*zz;

		var dv = this.__mgl_pf(obj, pp[n1][2]);
		var cv = this.__fov*obj.pf/(1-this.__fov/1.37)/obj.depth;
		xc += (pp[n1][0]-obj.b[9])*zc*cv;//*dv;
		yc += (pp[n1][1]-obj.b[10])*zc*cv;//*dv;

		if(obj.pnts[n1][3]<0)	{	xc=xx;	yc=yy;	}
		var ll = xc*xc+yc*yc;
		if(ll < 1e-10)	break;
		if(ll<1e10 && t/deg<1e4)
		{
			t = Math.atan2(yc,xc);
			if(Math.abs(t)>Math.PI/2)	t += Math.PI;
		}
		else t=0;
		var c=Math.cos(t), s=Math.sin(t), d=prim[6]/200;

		var b=[d*c, d*s, d*s, -d*c, pp[n1][0],pp[n1][1]];
		var x=obj.coor[n2][0]*scl/100, y=obj.coor[n2][1]*scl/100, f=prim[8]*scl/1e5;
		if(n3&8)
		{
			if(!(n3&4))	this.__mgl_line_glyph(ctx, x,y, f,1,b);
			else this.__mgl_line_glyph(ctx, x,y, f,0,b);
		}
		else
		{
			if(!(n3&4)) this.__mgl_fill_glyph(ctx, x,y, f,obj.glfs[n4],b);
			else this.__mgl_wire_glyph(ctx, x,y, f,obj.glfs[n4],b);
		}
		break;
	}
}

/** change coordinates according current transformations, usually called internally by draw() */
mathgl.Graph.prototype.__mgl_prepare = function(obj, skip) {
	// fill transformation matrix
	if(!skip)
	{
		var dx = 1/Math.abs(obj.z[1]-obj.z[0]);
		var dy = 1/Math.abs(obj.z[3]-obj.z[2]);
		var cx=Math.cos(obj.tet*deg), sx=Math.sin(obj.tet*deg);	// tetx
		var cy=Math.cos(obj.phi*deg), sy=Math.sin(obj.phi*deg);	// tety
		var cz=Math.cos(obj.bet*deg), sz=Math.sin(obj.bet*deg);	// tetz
		obj.b = [dx*cx*cy, -dx*cy*sx, dx*sy,
				dy*(cx*sy*sz+cz*sx), dy*(cx*cz-sx*sy*sz), -dy*cy*sz,
				sx*sz-cx*cz*sy, cx*sz+cz*sx*sy, cy*cz,
				obj.width/2*(1+dx-obj.z[1]-obj.z[0])/dx,
				obj.height/2*(1+dy-obj.z[3]-obj.z[2])/dy, obj.depth/2, dx,dy,1];
	}
	// now transform points for found transformation matrix
	var b = obj.b, i;
	obj.pp = [];
	for(i=0;i<obj.npnts;i++)
	{
		var x = obj.pnts[i][0]-obj.width/2;
		var y = obj.pnts[i][1]-obj.height/2;
		var z = obj.pnts[i][2]-obj.depth/2;
		if(obj.pnts[i][3]>=0)	// TODO: check later when mglInPlot will be ready
			obj.pp[i] = [b[9]  + b[0]*x + b[1]*y + b[2]*z,
						b[10] + b[3]*x + b[4]*y + b[5]*z,
						b[11] + b[6]*x + b[7]*y + b[8]*z];
		else
			obj.pp[i] = [b[9]+b[12]*x,b[10]+b[13]*y,b[11]+b[14]*z];
	}
	if(obj.pf || this.__fov)	for(var i=0;i<obj.npnts;i++)	// perspective
	{	// NOTE: it is not supported for coordinate determining now
		var d = this.__mgl_pf(obj, obj.pp[i][2]);
		if(obj.pnts[i][3]>=0)	// TODO: check later when mglInPlot will be ready
		{
			obj.pp[i][0] = d*obj.pp[i][0] + (1-d)*obj.b[9];
			obj.pp[i][1] = d*obj.pp[i][1] + (1-d)*obj.b[10];
		}
	}
	// fill z-coordinates for primitives
	if(!obj.fast)	for(i=0;i<obj.nprim;i++)
	{
		var prim = obj.prim[i];
		var n1 = prim[1], n2 = prim[2], n3 = prim[3], n4 = prim[4];
		switch(prim[0])
		{
		case 1: // lines
			prim[9] = (obj.pp[n1][2]+obj.pp[n2][2])/2;	break;
		case 2: // triangles
			prim[9] = (obj.pp[n1][2]+obj.pp[n2][2]+obj.pp[n3][2])/3;	break;
		case 3: // quadrangles
			prim[9] = (obj.pp[n1][2]+obj.pp[n2][2]+obj.pp[n3][2]+obj.pp[n4][2])/4;	break;
		default:
			prim[9] = obj.pp[n1][2];	break;
		}
	}
	if(!obj.fast) // sort primitives according its z-coordinate
		obj.prim.sort(this.__mgl_cmp); // more accurate sorting
}


mathgl.Graph.prototype.__mgl_cmp = function(a,b) {
	var tt = [0,2,4,5, 1,3,6, 7];
	if(a[9]!=b[9])	return a[9] - b[9];
	if(a[0]!=b[0])	return tt[b[0]]-tt[a[0]];
	if(a[8]!=b[8])	return a[8] - b[8];
	return a[3]-b[3];
}


/**
 * Function for drawing markers of type st with given size at position {x,y}
 * Usually this function is called internally, but it can be called by user as well
 */
mathgl.Graph.prototype.__mgl_draw_mark = function(ctx,x,y,st,size,d) {
	if(size<=0) {	st = 46;	size=1; }
	var s = size*d;
	ctx.beginPath();
	switch(st)
	{
	case 111:	// 'o'
		ctx.arc(x,y,s,0,Math.PI*2);	 ctx.stroke();	break;
	case 79:	// 'O'
		ctx.arc(x,y,s,0,Math.PI*2);	 ctx.fill();	 break;
	case 67:	// 'C'
		ctx.arc(x,y,s,0,Math.PI*2);	 ctx.stroke();
		ctx.arc(x,y,0.1*s,0,Math.PI*2); ctx.fill();	 break;
	case 80:	// 'P'
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y-s);
		ctx.lineTo(x+s,y+s);	ctx.lineTo(x-s,y+s);	ctx.lineTo(x-s,y-s);
		ctx.moveTo(x-s,y);		ctx.lineTo(x+s,y);
		ctx.moveTo(x,y-s);		ctx.lineTo(x,y+s);
		ctx.stroke();	break;
	case 43:	// '+'
		ctx.moveTo(x-s,y);		ctx.lineTo(x+s,y);
		ctx.moveTo(x,y-s);		ctx.lineTo(x,y+s);
		ctx.stroke();	break;
	case 88:	// 'X'
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y-s);
		ctx.lineTo(x+s,y+s);	ctx.lineTo(x-s,y+s);	ctx.lineTo(x-s,y-s);
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y+s);
		ctx.moveTo(x+s,y-s);	ctx.lineTo(x-s,y+s);
		ctx.stroke();	break;
	case 120:	// 'x'
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y+s);
		ctx.moveTo(x+s,y-s);	ctx.lineTo(x-s,y+s);
		ctx.stroke();	break;
	case 115:	// 's'
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y-s);
		ctx.lineTo(x+s,y+s);	ctx.lineTo(x-s,y+s);
		ctx.closePath();		ctx.stroke();	break;
	case 83:	// 'S'
		ctx.moveTo(x-s,y-s);	ctx.lineTo(x+s,y-s);
		ctx.lineTo(x+s,y+s);	ctx.lineTo(x-s,y+s);
		ctx.closePath();		ctx.fill();	 break;
	case 100:	// 'd'
		ctx.moveTo(x-s,y);		ctx.lineTo(x,y-s);
		ctx.lineTo(x+s,y);		ctx.lineTo(x,y+s);
		ctx.closePath();		ctx.stroke();	break;
	case 68:	// 'D'
		ctx.moveTo(x-s,y);		ctx.lineTo(x,y-s);
		ctx.lineTo(x+s,y);		ctx.lineTo(x,y+s);
		ctx.closePath();		ctx.fill();	 break;
	case 42:	// '*'
		ctx.moveTo(x-s,y);		ctx.lineTo(x+s,y);
		ctx.moveTo(x-0.6*s,y-0.8*s);	ctx.lineTo(x+0.6*s,y+0.8*s);
		ctx.moveTo(x+0.6*s,y-0.8*s);	ctx.lineTo(x-0.6*s,y+0.8*s);
		ctx.stroke();	break;
	case 89:	// 'Y'
		ctx.moveTo(x,y-s);		ctx.lineTo(x,y);
		ctx.moveTo(x-0.8*s,y+0.6*s);	ctx.lineTo(x,y);
		ctx.moveTo(x+0.8*s,y+0.6*s);	ctx.lineTo(x,y);
		ctx.stroke();	break;
	case 86:	// 'T'
		ctx.moveTo(x-s,y-s/2);	ctx.lineTo(x+s,y-s/2);
		ctx.lineTo(x,y+s);		ctx.closePath();
		ctx.fill();	 break;
	case 118:	// '^'
		ctx.moveTo(x-s,y-s/2);	ctx.lineTo(x+s,y-s/2);
		ctx.lineTo(x,y+s);		ctx.closePath();
		ctx.stroke();	break;
	case 84:	// 'V'
		ctx.moveTo(x-s,y+s/2);	ctx.lineTo(x+s,y+s/2);
		ctx.lineTo(x,y-s);		ctx.closePath();
		ctx.fill();	 break;
	case 94:	// 'v'
		ctx.moveTo(x-s,y+s/2);	ctx.lineTo(x+s,y+s/2);
		ctx.lineTo(x,y-s);		ctx.closePath();
		ctx.stroke();	break;
	case 76:	// 'L'
		ctx.moveTo(x+s/2,y-s);	ctx.lineTo(x+s/2,y+s);
		ctx.lineTo(x-s,y);		ctx.closePath();
		ctx.fill();	 break;
	case 60:	// '<'
		ctx.moveTo(x+s/2,y-s);	ctx.lineTo(x+s/2,y+s);
		ctx.lineTo(x-s,y);		ctx.closePath();
		ctx.stroke();	break;
	case 82:	// 'R'
		ctx.moveTo(x-s/2,y-s);	ctx.lineTo(x-s/2,y+s);
		ctx.lineTo(x+s,y);		ctx.closePath();
		ctx.fill();	 break;
	case 62:	// '>'
		ctx.moveTo(x-s/2,y-s);	ctx.lineTo(x-s/2,y+s);
		ctx.lineTo(x+s,y);		ctx.closePath();
		ctx.stroke();	break;
//	case 46:	// '.'
	default:
		ctx.rect(x,y,1,1); ctx.fill();	 break;
	}
}


/** for internal use only */
mathgl.Graph.prototype.__mgl_fill_glyph = function(ctx, x,y, f,g,b) {
	var xx,yy,j;
	var np=0;	ctx.beginPath();
	for(j=0;j<g[0];j++)
	{
		xx = g[1][2*j]; yy = g[1][2*j+1];
		if(xx==16383 && yy==16383)
		{
			ctx.closePath();	np = 1;
		}
		else if(np)
		{
			xx = x+f*xx;	yy = y+f*yy;	np = 0;
			ctx.moveTo(b[4]+b[0]*xx+b[1]*yy, b[5]+b[2]*xx+b[3]*yy);
		}
		else
		{
			xx = x+f*xx;	yy = y+f*yy;
			ctx.lineTo(b[4]+b[0]*xx+b[1]*yy, b[5]+b[2]*xx+b[3]*yy);
		}
	}
	ctx.closePath();	ctx.fill('evenodd');
}


/** for internal use only */
mathgl.Graph.prototype.__mgl_wire_glyph = function(ctx, x,y, f,g,b) {
	var xx,yy,j;
	var np=0;	ctx.beginPath();
	for(j=0;j<g[0];j++)
	{
		xx = g[1][2*j]; yy = g[1][2*j+1];
		if(xx==16383 && yy==16383)
		{
			ctx.closePath();	np = 1;
		}
		else if(np)
		{
			xx = x+f*xx;	yy = y+f*yy;	np = 0;
			ctx.moveTo(b[4]+b[0]*xx+b[1]*yy, b[5]+b[2]*xx+b[3]*yy);
		}
		else
		{
			xx = x+f*xx;	yy = y+f*yy;
			ctx.lineTo(b[4]+b[0]*xx+b[1]*yy, b[5]+b[2]*xx+b[3]*yy);
		}
	}
	ctx.closePath();	ctx.stroke();
}


/** for internal use only */
mathgl.Graph.prototype.__mgl_line_glyph = function(ctx, x,y, f,solid,b) {
	var xx,yy,j,xs,ys;
	var dy = 0.004;
	ctx.moveTo(b[4]+b[0]*x+b[1]*(y-dy), b[5]+b[2]*x+b[3]*(y-dy));
	ctx.lineTo(b[4]+b[0]*x+b[1]*(y+dy), b[5]+b[2]*x+b[3]*(y+dy));
	ctx.lineTo(b[4]+b[0]*(x+f)+b[1]*(y+dy), b[5]+b[2]*(x+f)+b[3]*(y+dy));
	ctx.lineTo(b[4]+b[0]*(x+f)+b[1]*(y-dy), b[5]+b[2]*(x+f)+b[3]*(y-dy));
	ctx.closePath();
	if(solid)	ctx.fill();
	else		ctx.stroke();
}


/**
 * Move Left using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.moveLeft = function() {
	var b = this.__geometry.b;
	var f = 0.1/Math.sqrt(b[0]*b[0]+b[1]*b[1]+b[2]*b[2]);
	this.shiftAxis(f*b[0],f*b[1],f*b[2]);
}


/**
 * Move Right using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.moveRight = function() {
	var b = this.__geometry.b;
	var f = -0.1/Math.sqrt(b[0]*b[0]+b[1]*b[1]+b[2]*b[2]);
	this.shiftAxis(f*b[0],f*b[1],f*b[2]);
}


/**
 * Move Up using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.moveUp = function() {
	var b = this.__geometry.b;
	var f = -0.1/Math.sqrt(b[3]*b[3]+b[4]*b[4]+b[5]*b[5]);
	this.shiftAxis(-f*b[3],f*b[4],-f*b[5]);
}


/**
 * Move Down using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.moveDown = function() {
	var b = this.__geometry.b;
	var f = 0.1/Math.sqrt(b[3]*b[3]+b[4]*b[4]+b[5]*b[5]);
	this.shiftAxis(-f*b[3],f*b[4],-f*b[5]);
}


/**
 * Zoom In using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.zoomIn = function() {
	this.zoomAxis(1.1);
}


/**
 * Zoom Out using MGL 'zoomaxis' with geometry reload
 */
mathgl.Graph.prototype.zoomOut = function() {
	this.zoomAxis(1./1.1);
}


/**
 * @return mathgl.View instance
 */
mathgl.Graph.prototype.getView = function() {
	return this.__view;
}


/**
 * Build and load geometry into View
 * @param mgl {String} MGL script
 * @param completeCallback {Function|null} optional callback to notify completion
 */
mathgl.Graph.prototype.loadGeometry = function(mgl, completeCallback) {
  this.__backend.geometry(mgl, mathgl.bind(function(error, result) {
    if (!error) {
      this.__geometry = result;
      this.__geometry.mgl = mgl;
      this.redraw();

      if (typeof completeCallback === "function") {
        completeCallback();
      }
    }
  }, this));
}


/**
 * Force reload geometry
 * @param completeCallback {Function|null} optional callback to notify completion
 */
mathgl.Graph.prototype.reloadGeometry = function(completeCallback) {
  var mgl = (this.__geometry && this.__geometry.mgl) ? this.__geometry.mgl : "";
  this.loadGeometry(mgl, completeCallback);
}


/**
 * Force redraw view from current geometry
 */
mathgl.Graph.prototype.redraw = function() {
	this.__renderStart();
}


/**
 * Shutdown graph instance
 * Destroy view, cleanup all timers and references
 */
mathgl.Graph.prototype.destroy = function() {
	// clear active timeouts
	for (var i = 0, l = this.__activeTimeoutHandlers.length; i<l; i++) {
		var th = this.__activeTimeoutHandlers.pop();
		clearTimeout(th);
	}
	this.__view.destroy();
	this.__view = null;
	this.__backend = null;
	this.__canvas = null;
	this.__geometry = null;
}


/**
 * Export image
 * @param type {String} data url type (e.g. "image/png")
 * @return {String} HTML Data URL containing graph image
 **/
mathgl.Graph.prototype.toDataURL = function(type) {
	return this.__canvas.toDataURL(type);
}


/**
 * Set perspective angle of view
 * @param val {Number} degree of perspective in range 0...1 (0 - use default orthogonal projection)
 **/
mathgl.Graph.prototype.setPerspective = function(val) {
	this.__fov = val;
}


/**
 * Set maximal number of drawable points in draft mode
 **/
mathgl.Graph.prototype.setMaxDraftPoints = function(count) {
	this.__maxDraftPoints = count;
}


/**
 * Set delay between draft and precise rendering
 * @param delayMillisec {Integer} delay in milliseconds
 */
mathgl.Graph.prototype.setPreciseRenderingDelay = function(delayMillisec) {
	this.__preciseRenderingDelay = delayMillisec;
}
