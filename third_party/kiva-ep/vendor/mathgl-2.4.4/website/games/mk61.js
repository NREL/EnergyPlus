var x=0, y=0, z=0, t=0, b=0;	// stack
var reg = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];	// registers 0...9,A...F

var ret = [];		// return stack
var prog = [];		// program to be executed (codes only)
var desc = "";		// program description
var pos = 0;		// current execution position
var stop = false;	// flag to stop execution
var interval = 0;

var pressF = 0;		// key F is pressed or not
var pressK = 0;		// key K is pressed or not
var prgON = false;		// we in program mode
var degF = Math.PI/180;		// factor of degrees (1, Math.PI/200, Math.PI/180)
var num = "";		// number being typed
var up = false;		// need shift up before new number
var x0=0, y0=0;		// current graphical coordinates
var ctx = document.getElementById("myCanvas").getContext("2d");
// var cols = ['#000', '#FFF', '#F00', '#0F0', '#00F', '#0FF', '#F0F', '#FF0',
// 			'#ccc', '#888', '#F80', '#0F8', '#80F', '#8F0', '#08F', '#F08', 
// 			'#800', '#080', '#008', '#088', '#808', '#880', '#840', '#084',
// 			'#408', '#480', '#048', '#804'];
var cols = ['#00f', '#08f', '#0ff', '#0f8', '#0f0', '#8f0',
			'#ff0', '#f80', '#f00', '#f08', '#f0f', '#80f',
			'#00a', '#05a', '#0aa', '#0a5', '#0a0', '#5a0',
			'#aa0', '#a50', '#a00', '#a05', '#a0a', '#50a',
			'#fff', '#ccc', '#999', '#666', '#333', '#000'];
var ccol = 'black';	// current color

function setColor(v)
{
	var l=cols.length, c = Math.floor(v)%l;
	if(c<0)	c+=l;
	ccol = cols[c];
}
function fin()	{	up=true;	num="";	}
function checkUp()
{	if(up)	{	num="";	t=z;	z=y;	y=x;	up=false;	}	}
var cmds = [
	{id:"0",	exec:function(){ checkUp();	num = num+"0";	x = num*1;	}},	// hex = 0
	{id:"1",	exec:function(){ checkUp();	num = num+"1";	x = num*1;	}},
	{id:"2",	exec:function(){ checkUp();	num = num+"2";	x = num*1;	}},
	{id:"3",	exec:function(){ checkUp();	num = num+"3";	x = num*1;	}},
	{id:"4",	exec:function(){ checkUp();	num = num+"4";	x = num*1;	}},
	{id:"5",	exec:function(){ checkUp();	num = num+"5";	x = num*1;	}},
	{id:"6",	exec:function(){ checkUp();	num = num+"6";	x = num*1;	}},
	{id:"7",	exec:function(){ checkUp();	num = num+"7";	x = num*1;	}},
	{id:"8",	exec:function(){ checkUp();	num = num+"8";	x = num*1;	}},
	{id:"9",	exec:function(){ checkUp();	num = num+"9";	x = num*1;	}},
	{id:".",	exec:function(){ checkUp();	num = num+".";	x = num*1;	}},
	{id:"&pm;",	exec:function(){ if(num.indexOf("e+")>0)	num = num.replace("e+","e-");
						else if(num.indexOf("e-")>0)	num = num.replace("e-","e+");
						else	{	x = -x;	fin();	}	}},
	{id:"EE",	exec:function(){ if(num=="")	num = x.toString(10);
						if(num=="0")	num = "1";	// This is *feature* of MK-61
						if(num.search('e')<0)	num = num+"e+0";	up=false;	}},
	{id:"Cx",	exec:function(){ up=false;	num="";	x=0;	}},
	{id:"B&uarr;",exec:function(){ up=false;	num="";	t=z;	z=y;	y=x;	}},
	{id:"Bx",	exec:function(){ fin();	x=b;	}},

	{id:"+",	exec:function(){ fin();	x=y+x;	y=z;	z=t;	}},	// hex = 16
	{id:"&minus;",exec:function(){ fin();	x=y-x;	y=z;	z=t;	}},
	{id:"&lowast;",	exec:function(){ fin();	x=y*x;	y=z;	z=t;	}},
	{id:"/",	exec:function(){ fin();	x=y/x;	y=z;	z=t;	}},
	{id:"&harr;",exec:function(){ fin();	var bb=x;	x=y;	y=bb;	}},
	{id:"10<sup>x</sup>",exec:function(){ fin();	x=Math.pow(10,x);}},
	{id:"e<sup>x</sup>",	exec:function(){ fin();	x=Math.exp(x);	}},
	{id:"lg",	exec:function(){ fin();	x=Math.log(x)/Math.LN10;}},
	{id:"ln",	exec:function(){ fin();	x=Math.log(x);	}},
	{id:"asin",	exec:function(){ fin();	x=Math.asin(x)/degF;	}},
	{id:"acos",	exec:function(){ fin();	x=Math.acos(x)/degF;	}},
	{id:"atan",	exec:function(){ fin();	x=Math.atan(x)/degF;	}},
	{id:"sin",	exec:function(){ fin();	x=Math.sin(x*degF);	}},
	{id:"cos",	exec:function(){ fin();	x=Math.cos(x*degF);	}},
	{id:"tan",	exec:function(){ fin();	x=Math.tan(x*degF);	}},
	{id:"Y<sub>0</sub>",	exec:function(){ fin();	x=BesselY0(x);	}},

	{id:"&pi;",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=Math.PI;	}},				// hex = 32
	{id:"&radic;<SPAN STYLE='text-decoration:overline'>x</SPAN>",	exec:function(){ fin();	x=Math.sqrt(x);	}},
	{id:"x<sup>2</sup>",exec:function(){ fin();	x=x*x;	}},
	{id:"1/x",	exec:function(){ fin();	x=1/x;	}},
	{id:"x<sup>y</sup>",exec:function(){ fin();	x=Math.pow(x,y);	y=z;	z=t;}},
	{id:"&#8635;",	exec:function(){ fin();	var bb=x;	x=y;	y=z;	z=t;	t=bb;	}},
	{id:"B&darr;",exec:function(){ fin();	x=y;	y=z;	z=t;	}},
	{id:"sh",	exec:function(){ fin();	x=Math.sinh(x);	}},
	{id:"ch",	exec:function(){ fin();	x=Math.cosh(x);	}},
	{id:"th",	exec:function(){ fin();	x=Math.tanh(x);	}},
	{id:"erf",	exec:function(){ fin();	x=erf(x);	}},
	{id:"x!",	exec:function(){ fin();	x=fact(x);	}},
	{id:"ash",	exec:function(){ fin();	x=Math.asinh(x);	}},
	{id:"ach",	exec:function(){ fin();	x=Math.acosh(x);	}},
	{id:"ath",	exec:function(){ fin();	x=Math.atanh(x);	}},
	{id:"Y<sub>1</sub>",	exec:function(){ fin();	x=BesselY1(x);	}},

	{id:"col",	exec:function(){ fin();	setColor(x);	}},	// hex = 48
	{id:"|x|",	exec:function(){ fin();	x=Math.abs(x);	}},
	{id:"sign",	exec:function(){ fin();	if(x<0)	x=-1;	if(x>0)	x=1;}},
	{id:"cls",	exec:function(){ fin();	ctx.clearRect(0, 0, 400, 300);	x0=y0=0;	}},
	{id:"[x]",	exec:function(){ fin();	x=Math.floor(x);	}},
	{id:"{x}",	exec:function(){ fin();	x=x-Math.floor(x);	}},
	{id:"max",	exec:function(){ fin();	x=Math.max(x,y);	y=z;	z=t;	}},
	{id:"and",	exec:function(){ fin();	x=x&y;	y=z;	z=t;	}},
	{id:"or",	exec:function(){ fin();	x=x|y;	y=z;	z=t;	}},
	{id:"xor",	exec:function(){ fin();	x=x^y;	y=z;	z=t;	}},
	{id:"not",	exec:function(){ fin();	x=~x;	}},
	{id:"rnd",	exec:function(){ fin();	x=Math.random();	}},
	{id:"pnt",	exec:function(){ fin();	ctx.beginPath();	ctx.fillStyle = ccol;	ctx.rect(x,y,1,1);	ctx.fill();	x0=x;	y0=y;	}},
	{id:"line",	exec:function(){ fin();	ctx.beginPath();	ctx.strokeStyle=ccol;	ctx.moveTo(x0,y0);	ctx.lineTo(x,y);	ctx.stroke();	x0=x;	y0=y;	}},
	{id:"rect",	exec:function(){ fin();	ctx.beginPath();	ctx.fillStyle = ccol;	ctx.rect(x,y,z-x,t-y);	ctx.fill();	}},
	{id:"arc",	exec:function(){ fin();	ctx.beginPath();	ctx.strokeStyle=ccol;	ctx.arc(x0,y0,x,y*degF,z*degF);	ctx.stroke();	}},	// x=R. y=angl1, z=angl2

	{id:"MS0",	exec:function(){ fin();	reg[0]=x;	}},				// hex = 64
	{id:"MS1",	exec:function(){ fin();	reg[1]=x;	}},
	{id:"MS2",	exec:function(){ fin();	reg[2]=x;	}},
	{id:"MS3",	exec:function(){ fin();	reg[3]=x;	}},
	{id:"MS4",	exec:function(){ fin();	reg[4]=x;	}},
	{id:"MS5",	exec:function(){ fin();	reg[5]=x;	}},
	{id:"MS6",	exec:function(){ fin();	reg[6]=x;	}},
	{id:"MS7",	exec:function(){ fin();	reg[7]=x;	}},
	{id:"MS8",	exec:function(){ fin();	reg[8]=x;	}},
	{id:"MS9",	exec:function(){ fin();	reg[9]=x;	}},
	{id:"MSA",	exec:function(){ fin();	reg[10]=x;	}},
	{id:"MSB",	exec:function(){ fin();	reg[11]=x;	}},
	{id:"MSC",	exec:function(){ fin();	reg[12]=x;	}},
	{id:"MSD",	exec:function(){ fin();	reg[13]=x;	}},
	{id:"MSE",	exec:function(){ fin();	reg[14]=x;	}},
	{id:"MSF",	exec:function(){ fin();	reg[15]=x;	}},

	{id:"stop",	exec:function(){ fin();	stop=true;	}},					// hex = 80
	{id:"go",	exec:function(opt=-1){ fin();	pos=opt-1;	}},
	{id:"ret",	exec:function(){ fin();	pos=ret.length>0?ret.pop():0;	}},
	{id:"sub",	exec:function(opt=-1){ fin();	ret.push(pos+1);	pos=opt-1;	}},
	{id:"nop",	exec:function(){ fin();	}},
	{id:"J<sub>0</sub>",	exec:function(){ fin();	x=BesselJ0(x);	}},
	{id:"J<sub>1</sub>",	exec:function(){ fin();	x=BesselJ1(x);	}},
	{id:"X&ne;0",exec:function(opt=-1){ fin();	if(x==0)	pos=opt-1;	else	pos+=1;	}},	// !!!NOTE
	{id:"L2",	exec:function(opt=-1){ fin();	reg[2] -= 1;	if(reg[2]>0)	pos=opt-1;	else	pos+=1;	}},
	{id:"X&ge;0",exec:function(opt=-1){ fin();	if(x<0) 	pos=opt-1;	else	pos+=1;	}},	// !!!NOTE
	{id:"L3",	exec:function(opt=-1){ fin();	reg[3] -= 1;	if(reg[3]>0)	pos=opt-1;	else	pos+=1;	}},
	{id:"L1",	exec:function(opt=-1){ fin();	reg[1] -= 1;	if(reg[1]>0)	pos=opt-1;	else	pos+=1;	}},
	{id:"X&lt;0",exec:function(opt=-1){ fin();	if(x>=0)	pos=opt-1;	else	pos+=1;	}},	// !!!NOTE
	{id:"L0",	exec:function(opt=-1){ fin();	reg[0] -= 1;	if(reg[0]>0)	pos=opt-1;	else	pos+=1;	}},
	{id:"X=0",	exec:function(opt=-1){ fin();	if(x!=0)	pos=opt-1;	else	pos+=1;	}},	// !!!NOTE
	{id:"wait",	exec:function(){ fin();	setTimeout(function(){}, 1000);	}},

	{id:"MR0",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[0];	}},				// hex = 96
	{id:"MR1",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[1];	}},
	{id:"MR2",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[2];	}},
	{id:"MR3",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[3];	}},
	{id:"MR4",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[4];	}},
	{id:"MR5",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[5];	}},
	{id:"MR6",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[6];	}},
	{id:"MR7",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[7];	}},
	{id:"MR8",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[8];	}},
	{id:"MR9",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[9];	}},
	{id:"MRA",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[10];	}},
	{id:"MRB",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[11];	}},
	{id:"MRC",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[12];	}},
	{id:"MRD",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[13];	}},
	{id:"MRE",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[14];	}},
	{id:"MRF",	exec:function(){ fin();	t=z;	z=y;	y=x;	x=reg[15];	}},

	{id:"Kx&ne;00",	exec:function(){ fin();	if(x!=0)	pos=modifReg(0)-1;	}},		// hex = 112
	{id:"Kx&ne;01",	exec:function(){ fin();	if(x!=0)	pos=modifReg(1)-1;	}},
	{id:"Kx&ne;02",	exec:function(){ fin();	if(x!=0)	pos=modifReg(2)-1;	}},
	{id:"Kx&ne;03",	exec:function(){ fin();	if(x!=0)	pos=modifReg(3)-1;	}},
	{id:"Kx&ne;04",	exec:function(){ fin();	if(x!=0)	pos=modifReg(4)-1;	}},
	{id:"Kx&ne;05",	exec:function(){ fin();	if(x!=0)	pos=modifReg(5)-1;	}},
	{id:"Kx&ne;06",	exec:function(){ fin();	if(x!=0)	pos=modifReg(6)-1;	}},
	{id:"Kx&ne;07",	exec:function(){ fin();	if(x!=0)	pos=modifReg(7)-1;	}},
	{id:"Kx&ne;08",	exec:function(){ fin();	if(x!=0)	pos=modifReg(8)-1;	}},
	{id:"Kx&ne;09",	exec:function(){ fin();	if(x!=0)	pos=modifReg(9)-1;	}},
	{id:"Kx&ne;0A",	exec:function(){ fin();	if(x!=0)	pos=modifReg(10)-1;	}},
	{id:"Kx&ne;0B",	exec:function(){ fin();	if(x!=0)	pos=modifReg(11)-1;	}},
	{id:"Kx&ne;0C",	exec:function(){ fin();	if(x!=0)	pos=modifReg(12)-1;	}},
	{id:"Kx&ne;0D",	exec:function(){ fin();	if(x!=0)	pos=modifReg(13)-1;	}},
	{id:"Kx&ne;0E",	exec:function(){ fin();	if(x!=0)	pos=modifReg(14)-1;	}},
	{id:"Kx&ne;0F",	exec:function(){ fin();	if(x!=0)	pos=modifReg(15)-1;	}},

	{id:"Kgo0",	exec:function(){ fin();	pos=modifReg(0)-1;	}},					// hex = 128
	{id:"Kgo1",	exec:function(){ fin();	pos=modifReg(1)-1;	}},
	{id:"Kgo2",	exec:function(){ fin();	pos=modifReg(2)-1;	}},
	{id:"Kgo3",	exec:function(){ fin();	pos=modifReg(3)-1;	}},
	{id:"Kgo4",	exec:function(){ fin();	pos=modifReg(4)-1;	}},
	{id:"Kgo5",	exec:function(){ fin();	pos=modifReg(5)-1;	}},
	{id:"Kgo6",	exec:function(){ fin();	pos=modifReg(6)-1;	}},
	{id:"Kgo7",	exec:function(){ fin();	pos=modifReg(7)-1;	}},
	{id:"Kgo8",	exec:function(){ fin();	pos=modifReg(8)-1;	}},
	{id:"Kgo9",	exec:function(){ fin();	pos=modifReg(9)-1;	}},
	{id:"KgoA",	exec:function(){ fin();	pos=modifReg(10)-1;	}},
	{id:"KgoB",	exec:function(){ fin();	pos=modifReg(11)-1;	}},
	{id:"KgoC",	exec:function(){ fin();	pos=modifReg(12)-1;	}},
	{id:"KgoD",	exec:function(){ fin();	pos=modifReg(13)-1;	}},
	{id:"KgoE",	exec:function(){ fin();	pos=modifReg(14)-1;	}},
	{id:"KgoF",	exec:function(){ fin();	pos=modifReg(15)-1;	}},

	{id:"Kx&ge;00",	exec:function(){ fin();	if(x>=0)	pos=modifReg(0)-1;	}},		// hex = 144
	{id:"Kx&ge;01",	exec:function(){ fin();	if(x>=0)	pos=modifReg(1)-1;	}},
	{id:"Kx&ge;02",	exec:function(){ fin();	if(x>=0)	pos=modifReg(2)-1;	}},
	{id:"Kx&ge;03",	exec:function(){ fin();	if(x>=0)	pos=modifReg(3)-1;	}},
	{id:"Kx&ge;04",	exec:function(){ fin();	if(x>=0)	pos=modifReg(4)-1;	}},
	{id:"Kx&ge;05",	exec:function(){ fin();	if(x>=0)	pos=modifReg(5)-1;	}},
	{id:"Kx&ge;06",	exec:function(){ fin();	if(x>=0)	pos=modifReg(6)-1;	}},
	{id:"Kx&ge;07",	exec:function(){ fin();	if(x>=0)	pos=modifReg(7)-1;	}},
	{id:"Kx&ge;08",	exec:function(){ fin();	if(x>=0)	pos=modifReg(8)-1;	}},
	{id:"Kx&ge;09",	exec:function(){ fin();	if(x>=0)	pos=modifReg(9)-1;	}},
	{id:"Kx&ge;0A",	exec:function(){ fin();	if(x>=0)	pos=modifReg(10)-1;	}},
	{id:"Kx&ge;0B",	exec:function(){ fin();	if(x>=0)	pos=modifReg(11)-1;	}},
	{id:"Kx&ge;0C",	exec:function(){ fin();	if(x>=0)	pos=modifReg(12)-1;	}},
	{id:"Kx&ge;0D",	exec:function(){ fin();	if(x>=0)	pos=modifReg(13)-1;	}},
	{id:"Kx&ge;0E",	exec:function(){ fin();	if(x>=0)	pos=modifReg(14)-1;	}},
	{id:"Kx&ge;0F",	exec:function(){ fin();	if(x>=0)	pos=modifReg(15)-1;	}},

	{id:"Ksub0",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(0)-1;	}},	// hex = 160
	{id:"Ksub1",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(1)-1;	}},
	{id:"Ksub2",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(2)-1;	}},
	{id:"Ksub3",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(3)-1;	}},
	{id:"Ksub4",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(4)-1;	}},
	{id:"Ksub5",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(5)-1;	}},
	{id:"Ksub6",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(6)-1;	}},
	{id:"Ksub7",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(7)-1;	}},
	{id:"Ksub8",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(8)-1;	}},
	{id:"Ksub9",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(9)-1;	}},
	{id:"KsubA",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(10)-1;	}},
	{id:"KsubB",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(11)-1;	}},
	{id:"KsubC",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(12)-1;	}},
	{id:"KsubD",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(13)-1;	}},
	{id:"KsubE",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(14)-1;	}},
	{id:"KsubF",	exec:function(){ fin();	ret.push(pos);	pos=modifReg(15)-1;	}},

	{id:"KMS0",	exec:function(){ fin();	var r=modifReg(0)%16;	if(r<0)	r+=16;	reg[r]=x;	}},	// hex = 176
	{id:"KMS1",	exec:function(){ fin();	var r=modifReg(1)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS2",	exec:function(){ fin();	var r=modifReg(2)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS3",	exec:function(){ fin();	var r=modifReg(3)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS4",	exec:function(){ fin();	var r=modifReg(4)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS5",	exec:function(){ fin();	var r=modifReg(5)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS6",	exec:function(){ fin();	var r=modifReg(6)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS7",	exec:function(){ fin();	var r=modifReg(7)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS8",	exec:function(){ fin();	var r=modifReg(8)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMS9",	exec:function(){ fin();	var r=modifReg(9)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSA",	exec:function(){ fin();	var r=modifReg(10)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSB",	exec:function(){ fin();	var r=modifReg(11)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSC",	exec:function(){ fin();	var r=modifReg(12)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSD",	exec:function(){ fin();	var r=modifReg(13)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSE",	exec:function(){ fin();	var r=modifReg(14)%16;	if(r<0)	r+=16;	reg[r]=x;	}},
	{id:"KMSF",	exec:function(){ fin();	var r=modifReg(15)%16;	if(r<0)	r+=16;	reg[r]=x;	}},

	{id:"Kx&lt;00",	exec:function(){ fin();	if(x<0)	pos=modifReg(0)-1;	}},		// hex = 192
	{id:"Kx&lt;01",	exec:function(){ fin();	if(x<0)	pos=modifReg(1)-1;	}},
	{id:"Kx&lt;02",	exec:function(){ fin();	if(x<0)	pos=modifReg(2)-1;	}},
	{id:"Kx&lt;03",	exec:function(){ fin();	if(x<0)	pos=modifReg(3)-1;	}},
	{id:"Kx&lt;04",	exec:function(){ fin();	if(x<0)	pos=modifReg(4)-1;	}},
	{id:"Kx&lt;05",	exec:function(){ fin();	if(x<0)	pos=modifReg(5)-1;	}},
	{id:"Kx&lt;06",	exec:function(){ fin();	if(x<0)	pos=modifReg(6)-1;	}},
	{id:"Kx&lt;07",	exec:function(){ fin();	if(x<0)	pos=modifReg(7)-1;	}},
	{id:"Kx&lt;08",	exec:function(){ fin();	if(x<0)	pos=modifReg(8)-1;	}},
	{id:"Kx&lt;09",	exec:function(){ fin();	if(x<0)	pos=modifReg(9)-1;	}},
	{id:"Kx&lt;0A",	exec:function(){ fin();	if(x<0)	pos=modifReg(10)-1;	}},
	{id:"Kx&lt;0B",	exec:function(){ fin();	if(x<0)	pos=modifReg(11)-1;	}},
	{id:"Kx&lt;0C",	exec:function(){ fin();	if(x<0)	pos=modifReg(12)-1;	}},
	{id:"Kx&lt;0D",	exec:function(){ fin();	if(x<0)	pos=modifReg(13)-1;	}},
	{id:"Kx&lt;0E",	exec:function(){ fin();	if(x<0)	pos=modifReg(14)-1;	}},
	{id:"Kx&lt;0F",	exec:function(){ fin();	if(x<0)	pos=modifReg(15)-1;	}},

	{id:"KMR0",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(0)%16;	if(r<0)	r+=16;	x=reg[r];	}},	// hex = 208
	{id:"KMR1",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(1)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR2",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(2)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR3",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(3)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR4",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(4)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR5",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(5)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR6",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(6)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR7",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(7)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR8",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(8)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMR9",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(9)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRA",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(10)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRB",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(11)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRC",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(12)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRD",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(13)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRE",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(14)%16;	if(r<0)	r+=16;	x=reg[r];	}},
	{id:"KMRF",	exec:function(){ fin();	t=z;	z=y;	y=x;	var r=modifReg(15)%16;	if(r<0)	r+=16;	x=reg[r];	}},

	{id:"Kx=00",	exec:function(){ fin();	if(x==0)	pos=modifReg(0)-1;	}},		// hex = 224
	{id:"Kx=01",	exec:function(){ fin();	if(x==0)	pos=modifReg(1)-1;	}},
	{id:"Kx=02",	exec:function(){ fin();	if(x==0)	pos=modifReg(2)-1;	}},
	{id:"Kx=03",	exec:function(){ fin();	if(x==0)	pos=modifReg(3)-1;	}},
	{id:"Kx=04",	exec:function(){ fin();	if(x==0)	pos=modifReg(4)-1;	}},
	{id:"Kx=05",	exec:function(){ fin();	if(x==0)	pos=modifReg(5)-1;	}},
	{id:"Kx=06",	exec:function(){ fin();	if(x==0)	pos=modifReg(6)-1;	}},
	{id:"Kx=07",	exec:function(){ fin();	if(x==0)	pos=modifReg(7)-1;	}},
	{id:"Kx=08",	exec:function(){ fin();	if(x==0)	pos=modifReg(8)-1;	}},
	{id:"Kx=09",	exec:function(){ fin();	if(x==0)	pos=modifReg(9)-1;	}},
	{id:"Kx=0A",	exec:function(){ fin();	if(x==0)	pos=modifReg(10)-1;	}},
	{id:"Kx=0B",	exec:function(){ fin();	if(x==0)	pos=modifReg(11)-1;	}},
	{id:"Kx=0C",	exec:function(){ fin();	if(x==0)	pos=modifReg(12)-1;	}},
	{id:"Kx=0D",	exec:function(){ fin();	if(x==0)	pos=modifReg(13)-1;	}},
	{id:"Kx=0E",	exec:function(){ fin();	if(x==0)	pos=modifReg(14)-1;	}},
	{id:"Kx=0F",	exec:function(){ fin();	if(x==0)	pos=modifReg(15)-1;	}},

	{id:"M+0",	exec:function(){ fin();	reg[0]+=x;	}},	// hex = 240
	{id:"M+1",	exec:function(){ fin();	reg[1]+=x;	}},
	{id:"M+2",	exec:function(){ fin();	reg[2]+=x;	}},
	{id:"M+3",	exec:function(){ fin();	reg[3]+=x;	}},
	{id:"M+4",	exec:function(){ fin();	reg[4]+=x;	}},
	{id:"M+5",	exec:function(){ fin();	reg[5]+=x;	}},
	{id:"M+6",	exec:function(){ fin();	reg[6]+=x;	}},
	{id:"M+7",	exec:function(){ fin();	reg[7]+=x;	}},
	{id:"M+8",	exec:function(){ fin();	reg[8]+=x;	}},
	{id:"M+9",	exec:function(){ fin();	reg[9]+=x;	}},
	{id:"M+A",	exec:function(){ fin();	reg[10]+=x;	}},
	{id:"M+B",	exec:function(){ fin();	reg[11]+=x;	}},
	{id:"M+C",	exec:function(){ fin();	reg[12]+=x;	}},
	{id:"M+D",	exec:function(){ fin();	reg[13]+=x;	}},
	{id:"M+E",	exec:function(){ fin();	reg[14]+=x;	}},
	{id:"M+F",	exec:function(){ fin();	reg[15]+=x;	}}];

var keyR = [-3,-3,-3,-3,-3,-3,	0x7,0x8,0x9,0xf,-3,-3,	0x4,0x5,0x6,0xe,-3,-3,	0x1,0x2,0x3,0xd,-3,-3,	0x0,0xa,0xb,0xc,-3,0xff];
var keyN = [-1,-2,-0x60,-0x40,-0xf0,0x22,	0x07,0x08,0x09,0x11,0x12,0x23,	0x04,0x05,0x06,0x10,0x13,0x24,	0x01,0x02,0x03,0x14,0x0e,0x0f,	0x00,0x0a,0x0b,0x0c,0x26,0x0d];
var keyF = [-1,-2,0x5d,0x5b,0x58,0x5a,	0x51,0x53,0x5c,0x5e,0x59,0x57,	0x50,0x52,0x37,0x38,0x39,0x3a,	0x1c,0x1d,0x1e,0x16,0x15,0x20,	0x19,0x1a,0x1b,0x18,0x17,0x21];
var keyK = [-1,-2,-0xd0,-0xb0,0x5f,0x25,	-0x80,-0xa0,-0xc0,-0xe0,-0x90,-0x70,	0x35,0x34,0x31,0x55,0x56,0x2a,	0x27,0x28,0x29,0x1f,0x2f,0x2b,	0x2c,0x2d,0x2e,0x32,0x3b,0x36];
var keyFK = [-1,-2,-3,-3,-3,0x54,	-3,-3,-3,-3,-3,-3,	-3,-3,-3,-3,-3,-3,	-3,-3,-3,-3,-3,-3,	0x3c,0x3d,0x3e,0x3f,0x30,0x33];

var keyCur = keyN;
var needR = 0;

function degree(kind)
{
	if(kind==0)
	{	document.getElementById("degR").checked=true;	document.getElementById("degG").checked=false;	document.getElementById("degD").checked=false;	degF=1;	}
	if(kind==1)
	{	document.getElementById("degR").checked=false;	document.getElementById("degG").checked=true;	document.getElementById("degD").checked=false;	degF=Math.PI/200;	}
	if(kind==2)
	{	document.getElementById("degR").checked=false;	document.getElementById("degG").checked=false;	document.getElementById("degD").checked=true;	degF=Math.PI/180;	}
}

function save()
{
	localStorage.setItem("regMK61",reg);
	localStorage.setItem("progMK61",prog);
	localStorage.setItem("descMK61",desc);
}

function toArray(str)
{	return str.split(",").map(function(item){return parseInt(item,10);});	}

function init()
{
	if (localStorage.regMK61)	reg = toArray(localStorage.regMK61);
	else	localStorage.regMK61 = reg;
	if (localStorage.progMK61)	prog = toArray(localStorage.progMK61);
	else	localStorage.progMK61 = prog;
	if (localStorage.descMK61)	desc = localStorage.descMK61;
	else	localStorage.descMK61 = desc;

	degree(0);	updateKeys();
}

function updateProg()
{
//	if(prgON)
	{
		var tbl = "<tr><th></th>";
		for(var i=0;i<10;i++)	tbl += "<th>"+i+"</th>";
		tbl += "</tr>";
		for(var i=0;i<prog.length;i++)
		{
			if(i%10==0)	tbl += "<tr><td>#"+Math.floor(i/10).toString(16)+i%10+":</td>";
			tbl += "<td onclick='pos="+i+";'";
			if(pos==i)	tbl += "style='background-color:cyan'";
			tbl += ">"+cmds[prog[i]].id+" [<i>#"+Math.floor(prog[i]/10).toString(16)+prog[i]%10+"</i>]</td>";
			if(i%10==9)	tbl += "</tr>";
		}
		document.getElementById("prog").innerHTML = tbl+"</tr>";
	}
}

function updateRegs()
{
	if(num=="")	document.getElementById("regX").value = x;
	else	document.getElementById("regX").value = num;
	document.getElementById("regY").value = y;
	document.getElementById("regZ").value = z;
	document.getElementById("regT").value = t;

	document.getElementById("reg").innerHTML = "<p>Bx = "+b+"</p><p>Registers: "+reg+"</p>";
	//"<p>Return stack:"+ret+"</p><p>pos = "+pos+"</p>";
}

function updateKeys()
{
	document.getElementById("prg").checked=prgON;
	if(needR!=0)
	{
// 		if(needR<0)
// 			document.getElementById("status").innerHTML = "Enter register";
// 		else if(needR>=0x200)
// 			document.getElementById("status").innerHTML = "Enter first digit of address";
// 		else
// 			document.getElementById("status").innerHTML = "Enter second digit of address";
		keyCur = keyR;
	}
	else if(pressF==0 && pressK==0)	keyCur = keyN;
	else if(pressF==1 && pressK==0)	keyCur = keyF;
	else if(pressF==0 && pressK==1)	keyCur = keyK;
	else if(pressF==1 && pressK==1)	keyCur = keyFK;
// 	if(needR==0)	document.getElementById("status").innerHTML = "";

	for(var i=0;i<30;i++)
	{
		var but = document.getElementById("button"+i);
		var stl = "color:#000;background:#f9f9f9";
		var key = keyCur[i];	but.disabled = false;
		if(key==-3)	{	but.disabled = true;	but.innerHTML="";	}
		else if(key==-1)
		{	but.innerHTML = "F";	stl = pressF>0?"color:#850;background:#fc8":"color:#fa0;background:#f9f9f9";	}
		else if(key==-2)
		{	but.innerHTML = "K";	stl = pressK>0?"color:#008;background:#88f":"color:#00f;background:#f9f9f9";	}
		else if(needR!=0)
		{
			if(key<16)
				but.innerHTML = key.toString(16).toUpperCase();
			else
			{
				stl = "color:#f00;background:#f9f9f9";
				but.innerHTML = "&times;";
			}
		}
		else if(key<0)
		{
			stl = "color:#f0f;background:#f9f9f9";
			but.innerHTML = cmds[-key].id.slice(0,-1);
		}
		else
		{
			const grph = [48,60,61,62,63,51];
			const exeC = [81,83,87,88,89,90,91,92,93,94];
			if(grph.includes(key))	stl = "color:#0d0;background:#f9f9f9";
			if(exeC.includes(key))	stl = "color:#880;background:#f9f9f9";
			if(key==0x0d)	stl = "color:#F00;background:#f9f9f9";
			but.innerHTML = cmds[key].id;
		}
		but.style = "width:55px;height:55px;border-radius:10px;font-size:15pt;padding:0px;"+stl;
	}
	updateRegs();
	updateProg();
}

function handleKey(key)
{
	const exeC = [81,83,87,88,89,90,91,92,93,94];
	if(prgON)
	{
		if(pos<0 || pos>=160)	pos = 0;
		if(pos>prog.length)	pos=prog.length;
		prog[pos] = key;	pos++;
		if(exeC.includes(key))
		{	needR = 0x200;	updateKeys();	}
	}
	else if(!prgON)	if(!exeC.includes(key))
	{
		var bb = x;
		cmds[key].exec();
		if(num=="")	b = bb;
	}
	if(pressF!=0 || pressK!=0)
	{	pressF = pressK = 0;	updateKeys();	}
	updateRegs();
	updateProg();
}

function press(but)
{
	var key = keyCur[but];
	if(key==-3)	return;	// nothing to do
	else if(key==-1)	// key F
	{	pressF = 1-pressF;	updateKeys();	}
	else if(key==-2)	// key K
	{	pressK = 1-pressK;	updateKeys();	}
	else if(key<0)		// need RegId
	{	needR = key;	updateKeys();	}
	else if(needR>0)	// input RegId
	{
		// use "nop" for prev cmd if canceled
		if(key>16)	{	pos--;	prog[pos]=84;	needR=0;	updateKeys();	}
		else if(needR>=0x200)	{	needR+=key-0x100;	}
		else	// NOTE: for compatibility reason the pseudo-hex notation is used!!!
		{	prog[pos] = 10*(needR%16)+key;	pos++;	needR=0;	updateKeys();	}
	}
	else if(needR<0)	// input RegId
	{
		if(key<16)	key -= needR;
		needR=0;	updateKeys();	handleKey(key);
	}
	else	handleKey(key);
}

function stopRun()
{
	document.getElementById("butRun").innerHTML = "Run";
	stop=false;	if(interval) clearInterval(interval);	interval=0;
}

function posDec()
{
	pos--;	if(pos<0)	pos = prog.length-1;
	updateProg();
}

function posInc()
{
	pos++;	if(pos>prog.length)	pos = 0;
	updateProg();
}

function posUp()
{
	pos-=10;	if(pos<0)	pos = 0;
	updateProg();
}

function posDwn()
{
	pos+=10;	if(pos>prog.length)	pos = prog.length-1;
	updateProg();
}

function posDel()
{
	if(pos>=0 && pos<prog.length)	prog.splice(pos,1);
	else if(prog.length>0)	prog.pop();
	updateProg();
}

function posIns()
{
	if(pos>=0 && pos<prog.length)	prog.splice(pos,0,84);
	else	prog.push(84);
	updateProg();
}

function run()
{
	if(interval)	{	stopRun();	return;	}
	if(pos<0 || pos>=prog.length)	pos=0;
	stop = false;
	interval = setInterval(step, 20);
	document.getElementById("butRun").innerHTML = "Stop";
}

function step()
{
	if(pos<0 || pos>=prog.length || stop || x-x!=0)	{	stopRun();	return;	}
	var kod = prog[pos], opt = pos+1<prog.length?prog[pos+1]:0;
	var bb = x;
	cmds[kod].exec(opt);
	if(num=="")	b = bb;
	pos++;
	if(pos>=160)	pos=0;
	updateRegs();
	updateProg();
}

function modifReg(rr)
{
	if(rr<8) reg[rr] += (rr<4) ? -1:1;
	reg[rr] = Math.floor(reg[rr]);
	return reg[rr];
}

function fact(x)
{
	var r = Math.exp(lgamma(x+1));	// this is good but approximate result
	if(x==Math.floor(x))	r = Math.floor(r+0.5);
	return r;
}

function lgamma(x)
{
	const cof=[76.18009172947146,-86.50532032941677,	24.01409824083091,-1.231739572450155,	0.1208650973866179e-2,-0.5395239384953e-5];
	var tmp = x+5.5 - (x+0.5)*Math.log(x+5.5);
	var ser = 1.000000000190015;
	for(var j=0;j<=5;j++) ser += cof[j]/(x+j+1);
	return Math.log(2.5066282746310005*ser/x) -tmp;
}

function erf(x)		// интеграл вероятности с точностью 1.5*10^-7 !!!
{
	var s=1;
	if(x<0) { s = -1; x = -x; }
	var t=1/(1+0.32759*x);
	var res=0.254829592*t;
	res -= 0.284496736*t*t;
	res += 1.42413741*t*t*t;
	res -= 1.453152027*Math.pow(t,4);
	res += 1.061405429*Math.pow(t,5);
	res *= Math.exp(-x*x);
	return s*(1-res);
}

function BesselJ0(x) {
	var ax,z,xx,y,ans,ans1,ans2;
	ax = Math.abs(x);
	if (ax < 8.0) {
		y = x*x;
		ans1 = 57568490574.0+y*(-13362590354.0+y*(651619640.7+y*(-11214424.18+y*(77392.33017+y*(-184.9052456)))));
		ans2 = 57568490411.0+y*(1029532985.0+y*(9494680.718+y*(59272.64853+y*(267.8532712+y*1.0))));
		ans = ans1/ans2;
	} else {
		z = 8.0/ax;
		y = z*z;
		xx = ax-0.785398164;
		ans1 = 1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3+y*(-0.6911147651e-5+y*(0.7621095161e-6 - y*0.934935152e-7)));
		ans = Math.sqrt(0.636619772 / ax)*(Math.cos(xx)*ans1 - z*Math.sin(xx)*ans2);
	}
	return ans;
}

function BesselJ1(x) {
	var ax,z,xx,y,ans,ans1,ans2;
	ax = Math.abs(x);
	if (ax < 8.0) {
		y=x*x;
		ans1 = x*(72362614232.0+y*(-7895059235.0+y*(242396853.1+y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
		ans2 = 144725228442.0+y*(2300535178.0+y*(18583304.74+y*(99447.43394+y*(376.9991397+y*1.0))));
		ans = ans1/ans2;
	} else {
		z=8.0/ax;
		y=z*z;
		xx=ax-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.2002690873e-3+y*(0.8449199096e-5+y*(-0.88228987e-6+y*0.105787412e-6)));
		ans=Math.sqrt(0.636619772/ax)*(Math.cos(xx)*ans1-z*Math.sin(xx)*ans2);
		if (x < 0.0) ans = -ans;
	}
	return ans;	
}

function BesselY0(x) {
	var z, xx, y, ans, ans1, ans2;
	if (x < 8.0)  {
		y=x*x;
		ans1 = -2957821389.0+y*(7062834065.0+y*(-512359803.6+y*(10879881.29+y*(-86327.92757+y*228.4622733))));
		ans2 = 40076544269.0+y*(745249964.8+y*(7189466.438+y*(47447.26470+y*(226.1030244+y*1.0))));
		ans = (ans1/ans2)+0.636619772*BesselJ0(x)*Math.log(x);
	} else {
		z=8.0/x;
		y=z*z;
		xx=x-0.785398164;
		ans1 = 1.0+y*(-0.1098628627e-2+y*(0.2734510407e-4+y*(-0.2073370639e-5+y*0.2093887211e-6)));
		ans2 = -0.1562499995e-1+y*(0.1430488765e-3+y*(-0.6911147651e-5+y*(0.7621095161e-6+y*(-0.934945152e-7))));
		ans = Math.sqrt(0.636619772/x)*(Math.sin(xx)+ans1+z*Math.cos(xx)*ans2);
	}
	return ans;
}

function BesselY1(x) {
	var z, xx, y, ans, ans1, ans2;
	if (x < 8.0) { 
		y=x*x
		ans1 = x*(-0.4900604943e13+y*(0.1275274390e13+y*(-0.5153438139e11+y*(0.7349264551e9+y*(-0.4237922726e7+y*0.8511937935e4)))));
		ans2 = 0.2499580570e14+y*(0.4244419664e12+y*(0.3733650367e10+y*(0.2245904002e8+y*(0.1020426050e6+y*(0.3549632885e3+y)))));
		ans = (ans1/ans2)+0.636619772*(BesselJ1(x)*Math.log(x)-1.0/x);
	} else {
		z=8.0/x;
		y=z*z;
		xx=x-2.356194491;
		ans1=1.0+y*(0.183105e-2+y*(-0.3516396496e-4+y*(0.2457520174e-5+y*(-0.240337019e-6))));
		ans2=0.04687499995+y*(-0.202690873e-3+y*(0.8449199096e-5+y*(-0.88228987e-6+y*0.10578e-6)));
		ans=Math.sqrt(0.636619772/x)*(Math.sin(xx)*ans1+z*Math.cos(xx)*ans2);
	}
	return ans;
}
