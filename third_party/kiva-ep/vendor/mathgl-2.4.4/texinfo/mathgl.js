/***************************************************************************
 * mathgl.js is part of Math Graphic Library
 * Copyright (C) 2012 Alexey Balakin <balakin@appl.sci-nnov.ru>            *
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
var obj;
var ctx;
var cw,ch;
var deg = Math.PI/180;  //0.017453293;

var main = function()
{
	ctx = document.getElementById("canvas").getContext("2d");
	cw = document.getElementById("canvas").width;
	ch = document.getElementById("canvas").height;
	ctx.lineCap="round";	// global setting

	mgl_init("json/alpha.json");
//	mgl_init("json/alpha.jsonz");
	var t1 = new Date();
	mgl_draw_good(obj, ctx);
//	draw_fast(obj, ctx);
	var t2 = new Date();
	document.getElementById("time").innerHTML = "Drawing time is "+(t2.getTime()-t1.getTime())+" ms. Number of primitives is "+obj.nprim+". Canvas size is "+obj.width+"*"+obj.height+" points.";
};

var mglChange = function()
{
	var name = document.getElementById("select").value;
	mgl_init("json/"+name+".json");
	var t1 = new Date();
	ctx.clearRect(0,0,cw,ch);
	mgl_draw_good(obj, ctx);
//	draw_fast(obj, ctx);
	var t2 = new Date();
	document.getElementById("time").innerHTML = "Drawing time is "+(t2.getTime()-t1.getTime())+" ms. Number of primitives is "+obj.nprim;
}

// mouse handling functions
var mglMouseUp = function()
{	obj.button = 0;	obj.good = 0;
	ctx.clearRect(0,0,cw,ch);
	mgl_draw_good(obj, ctx);	}
var mglMouseDown = function(event)
{
	obj.good = 1;
	obj.mouseX = event.clientX;
	obj.mouseY = event.clientY;
	obj.button = event.button+1;
}
var mglMouseMove = function(event)
{
	var x = event.clientX-obj.mouseX;
	var y = event.clientY-obj.mouseY;
	switch(obj.button)
	{
	case 1: // rotate
		mgl_rotate_down(obj, y*180/ch);
		mgl_rotate_left(obj, x*180/cw);	break;
	case 2: // shift
		mgl_shift_down(obj, y/ch);
		mgl_shift_right(obj, x/cw);		break;
	case 3: // zoom
		mgl_zoom_in(obj, Math.pow(1.003,x));	break;
	 }
	if(obj.button)
	{
		obj.mouseX += x;	obj.mouseY += y;
		mgl_draw(obj, ctx);
	}
}
var mglMouseWheel = function(event)
{
//	var e = window.event;
	var d = event.wheelDelta? event.wheelDelta:event.detail*(-120);
	mgl_zoom_in(obj, Math.pow(1.002,d));
	mgl_draw(obj, ctx);
}
var mglRestore = function()
{
	mgl_restore(obj);
	ctx.clearRect(0,0,cw,ch);
	mgl_draw_good(obj,ctx);
}

// The function load data and set up rotation/zoom state
var mgl_init = function(name)
{
	// now obtain JSON data
	var req = new XMLHttpRequest(), txt;
	req.open( "GET", name, false );
	req.overrideMimeType('text\/plain; charset=x-user-defined');
/*	req.responseType = "arraybuffer";
	req.onload = function (oEvent) {
		var arrayBuffer = req.response; // Note: not oReq.responseText
		console.debug("arrayBuffer=",arrayBuffer);
		if (arrayBuffer) {
			var compressed = new Uint8Array(arrayBuffer);
			console.debug("compressed=",compressed);
//			var gunzip = new Zlib.Gunzip(compressed);
//			txt = gunzip.decompress();
			var inflate = new Zlib.Inflate(arrayBuffer);
			txt = inflate.decompress();
		}
	};*/
	req.send(null);
//	if(name[name.length-1]!='z')
		txt = req.responseText;
// 	else
// 	{
// console.debug("compressed=",req.responseText);
// 		var inflate = new Zlib.Inflate(req.responseText);
// 		txt = inflate.decompress();
// 	}
	obj = JSON.parse(txt);

	// copy original data for transformation
	obj.pp = new Array();
	for(var i=0;i<obj.npnts;i++)	// copy original data for transformation
		obj.pp[i] = [obj.pnts[i][0],obj.pnts[i][1],obj.pnts[i][2]];
	// and set other variables
	obj.pf = 0;		// perspective should be |pf|<1
	obj.z  = [0,1, 0,1];	// current zoom/shift
	obj.tet= 0;		// current tet-angle for rotation
	obj.phi= 0;		// current phi-angle for rotation
	obj.bet= 0;		// current beta-angle for rotation
	obj.button = 0;	// pressed mouse buttons (0-none, 1-left, 2-right)
	obj.mouseX=0;	obj.mouseY=0;
	obj.fast = 0;	obj.good = 0;
	obj.dx = cw/obj.width;
	obj.dy = ch/obj.height;
}

// Functions for rotation, shifting and zooming of the picture as whole
// Basically it redefine obj properties by user-friendly manner
var mgl_rotate_left = function(obj,val)	{	obj.tet += val; }
var mgl_rotate_right = function(obj,val){	obj.tet -= val; }
var mgl_rotate_up = function(obj,val)	{	obj.bet += val; }
var mgl_rotate_down = function(obj,val)	{	obj.bet -= val; }
var mgl_restore = function(obj)		// restore transformation state
{	obj.tet=obj.phi=obj.bet=0;	obj.z=[0,1,0,1];	obj.pf=0;	}
var mgl_perspective = function(obj,val) // add perspective, def.value=0.1
{
	var x = obj.pf/(1-obj.pf) + val;
	obj.pf = x/(1+x);	// let use this formula for "smooth" changing
}
var mgl_zoom_in = function(obj,factor)		// zoom in picture
{	if(factor)	mgl_zoom_out(obj,1/factor);	}
var mgl_zoom_out = function(obj,factor)	// zoom out picture
{
	var d, c;
	d=(obj.z[3]-obj.z[2])*factor/2; c=(obj.z[3]+obj.z[2])/2;
	obj.z[2] = c-d; obj.z[3] = c+d;
	d=(obj.z[1]-obj.z[0])*factor/2; c=(obj.z[1]+obj.z[0])/2;
	obj.z[0] = c-d; obj.z[1] = c+d;
}
var mgl_shift_up = function(obj,val)	// shift up
{	mgl_shift_down(obj,-val);	}
var mgl_shift_down = function(obj,val)	// shift down
{
	var d=val/(obj.z[3]-obj.z[2]);
	obj.z[2] -= d; obj.z[3] -= d;
}
var mgl_shift_left = function(obj,val)	// shift left
{	mgl_shift_right(obj,-val);	}
var mgl_shift_right = function(obj,val)	// shift right
{
	var d=val/(obj.z[1]-obj.z[0]);
	obj.z[0] -= d; obj.z[1] -= d;
}

// This function make drawing itself
var mgl_draw = function(obj, ctx)
{
	ctx.clearRect(0,0,cw,ch);
	if(obj.good==0)
	{
		obj.good = 1;
		setTimeout(function(){mgl_draw_good(obj, ctx);},300);
	}
	else if(obj.fast==0)
	{
		obj.fast = 1;
		setTimeout(function(){mgl_draw_fast(obj, ctx);},0);
	}
/*	var t1 = new Date();
	draw_good(obj, ctx);
//	draw_fast(obj, ctx);
	var t2 = new Date();
	console.debug("execution time is ", t2.getTime()-t1.getTime());*/
}


// This function make fast drawing
var mgl_draw_fast = function(obj, ctx, skip)
{
	if(obj.fast==0)	return;
	mgl_prepare(obj,skip);	// update coordinates
	// for each primitive skipping superfluous
	for(var i=0;i<obj.nprim;i ++)
	{
		var prim = obj.prim[i];
		var n1 = prim[1], nn = obj.pp[n1];
		if(prim[0]==1 || obj.pnts[n1][3]<0)
			mgl_draw_prim(obj,ctx,prim,Math.abs(obj.b[12]));
		else if(obj.prim[i][0]<4)
		{
			ctx.fillStyle = obj.prim[i][10];
			ctx.fillRect(nn[0], nn[1], 2, 2);
		}
	}
	obj.fast = 0;
}

// This function make high-quality drawing
var mgl_draw_good = function(obj, ctx, skip)
{
	obj.fast = 0;
	mgl_prepare(obj,skip);	// update coordinates
	// NOTE: this valid only for current zoom/view. In general case it should be more complicated
	var s1 = Math.sqrt(obj.b[0]*obj.b[0]+obj.b[1]*obj.b[1]+obj.b[2]*obj.b[2]), s2 = Math.abs(obj.b[12]);
	for(var i=0;i<obj.nprim;i++)	// for each primitive
	{
		var prim = obj.prim[i];
		mgl_draw_prim(obj,ctx,prim,obj.pnts[prim[1]][3]<0?s2:s1);
	}
	obj.good = 0;
}

/** perform high-quality drawing */
var mgl_draw_prim = function(obj, ctx, prim, scl)
{
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
		mgl_draw_mark(ctx, obj.pp[n1][0], obj.pp[n1][1], n4, prim[6]/100, scl);
		break;
	case 1: // lines
		ctx.beginPath();
		ctx.moveTo(obj.pp[n1][0],obj.pp[n1][1]);
		ctx.lineTo(obj.pp[n2][0],obj.pp[n2][1]);
		ctx.lineWidth = prim[7]/100;
		ctx.stroke();	break;
	case 2: // triangles
		ctx.beginPath();
		ctx.moveTo(obj.pp[n1][0],obj.pp[n1][1]);
		ctx.lineTo(obj.pp[n2][0],obj.pp[n2][1]);
		ctx.lineTo(obj.pp[n3][0],obj.pp[n3][1]);
		ctx.closePath();	ctx.fill();	break;
	case 3: // quadrangles
		ctx.beginPath();
		ctx.moveTo(obj.pp[n1][0],obj.pp[n1][1]);
		ctx.lineTo(obj.pp[n2][0],obj.pp[n2][1]);
		ctx.lineTo(obj.pp[n4][0],obj.pp[n4][1]);
		ctx.lineTo(obj.pp[n3][0],obj.pp[n3][1]);
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

		var dv = (1-obj.pf/1.37)/(1-obj.pf*obj.pp[i][2]/obj.depth);
		var cv = obj.pf/(1-obj.pf/1.37)/obj.depth;
		xc += (obj.pp[n1][0]-obj.b[9])*zc*cv;//*dv;
		yc += (obj.pp[n1][1]-obj.b[10])*zc*cv;//*dv;
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

		var b=[d*c, d*s, d*s, -d*c, obj.pp[n1][0],obj.pp[n1][1]];
		var x=obj.coor[n2][0]*scl/100, y=obj.coor[n2][1]*scl/100, f=prim[8]*scl/1e5;
		if(n3&8)
		{
			if(!(n3&4))	mgl_line_glyph(ctx, x,y, f,1,b);
			else	mgl_line_glyph(ctx, x,y, f,0,b);
		}
		else
		{
			if(!(n3&4)) mgl_fill_glyph(ctx, x,y, f,obj.glfs[n4],b);
			else	mgl_wire_glyph(ctx, x,y, f,obj.glfs[n4],b);
		}
		break;
	}
}

// This function change coordinates according current transformations
// Usually this Function is called internally by draw()
var mgl_prepare = function(obj, skip)
{
	// fill transformation matrix
	if(!skip)
	{
		var dx = 1/Math.abs(obj.z[1]-obj.z[0]);
		var dy = 1/Math.abs(obj.z[3]-obj.z[2]);
		var cx=Math.cos(obj.tet*deg), sx=Math.sin(obj.tet*deg);	// tetx
		var cy=Math.cos(obj.phi*deg), sy=Math.sin(obj.phi*deg);	// tety
		var cz=Math.cos(obj.bet*deg), sz=Math.sin(obj.bet*deg);	// tetz
		obj.b = [obj.dx*dx*cx*cy, -obj.dx*dx*cy*sx, obj.dx*dx*sy,
				obj.dy*dy*(cx*sy*sz+cz*sx), obj.dy*dy*(cx*cz-sx*sy*sz), -obj.dy*dy*cy*sz,
				sx*sz-cx*cz*sy, cx*sz+cz*sx*sy, cy*cz,
				cw/2*(1+dx-obj.z[1]-obj.z[0])/dx,
				ch/2*(1+dy-obj.z[3]-obj.z[2])/dy, obj.depth/2, obj.dx*dx,obj.dy*dy,1];
	}
	// now transform points for found transformation matrix
	var b = obj.b, i;
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
	if(obj.pf)	for(var i=0;i<obj.npnts;i++)	// perspective
	{	// NOTE: it is not supported for coordinate determining now
		var d = (1-obj.pf/1.37)/(1-obj.pf*obj.pp[i][2]/obj.depth);
		if(obj.pnts[i][3]>=0)	// TODO: check later when mglInPlot will be ready
		{
			obj.pp[i][0] = d*obj.pp[i][0] + (1-d)/2*obj.width;
			obj.pp[i][1] = d*obj.pp[i][1] + (1-d)/2*obj.height;
		}
	}
	// fill z-coordinates for primitives
	if(!obj.fast)
	{
		for(i=0;i<obj.nprim;i++)
		{
			var n1 = obj.prim[i][1], n2 = obj.prim[i][2], n3 = obj.prim[i][3], n4 = obj.prim[i][4];
			switch(obj.prim[i][0])
			{
			case 1: // lines
				obj.prim[i][9] = (obj.pp[n1][2]+obj.pp[n2][2])/2;	break;
			case 2: // triangles
				obj.prim[i][9] = (obj.pp[n1][2]+obj.pp[n2][2]+obj.pp[n3][2])/3;	break;
			case 3: // quadrangles
				obj.prim[i][9] = (obj.pp[n1][2]+obj.pp[n2][2]+obj.pp[n3][2]+obj.pp[n4][2])/4;	break;
			default:
				obj.prim[i][9] = obj.pp[n1][2];	break;
			}
		}
		obj.prim.sort(mgl_cmp);	// more accurate sorting
	}
}

var mgl_cmp = function(a,b)
{
	var tt = [0,2,4,5, 1,3,6, 7];
	if(a[9]!=b[9])	return a[9] - b[9];
	if(a[0]!=b[0])	return tt[b[0]]-tt[a[0]];
	if(a[8]!=b[8])	return a[8] - b[8];
	return a[3]-b[3];
}

// Function for drawing markers of type st with given size at position {x,y}
// Usually this function is called internally, but it can be called by user as well
var mgl_draw_mark = function(ctx,x,y,st,size,d)
{
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

// This function for internal use only!!!
var mgl_fill_glyph = function(ctx, x,y, f,g,b)
{
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
// This function for internal use only!!!
var mgl_wire_glyph = function(ctx, x,y, f,g,b)
{
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
// This function for internal use only!!!
var mgl_line_glyph = function(ctx, x,y, f,solid,b)
{
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
