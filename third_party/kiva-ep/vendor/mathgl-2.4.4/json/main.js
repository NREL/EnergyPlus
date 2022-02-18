var graph;

/**
 * Entry function.
 */
var main = function() {
  // get canvas to work over from the DOM
  var canvas = document.getElementById("canvas");

  // get backend injected by from C++ side just to illustrate where it came from
  // in normal web application it shall be implemented using some transport protocol
  var backend = new mathgl.WebkitBackend();

  // instantiate mathgl graph instance connecting it with canvas and backend
  graph = new mathgl.Graph(canvas, backend);
  graph.setBackgroundFillStyle("#F0F0F0");
  // initialize it by some sample MGL script
  graph.loadGeometry(makeSampleScript(),null);
  graph.setPerspective(0.5);
}


//var script = "rotate 40 60:fsurf 'sin(2*pi*x*y)'\nbox:axis:fplot 'sin(2*pi*t)' 'cos(2*pi*t)' '2*t-1' 'm2o'";
//var script = "box:axis:fplot 'sin(pi*x)'";
//var script = "rotate 10 20: box:axis:fsurf 'sin(pi*x*y)'";

var makeSampleScript = function() {
//	var mgl = "fsurf 'x' 'rgb';value 4";

	var mgl = "origintick off\n";
	mgl += "title 'qqq' '@k':ranges -2 2 -2 2 -2 2:colorbar '>'\n"   // NOTE: Ranges MUST BE specified for correctly work of zoomaxis feature
	mgl += "facenum 50:";
//	mgl += "origin 0 0 0:axis :xlabel 'xxxx':ylabel 'yyyy':zlabel 'zzzz':"
	mgl += "axis :xlabel 'xxxx':ylabel 'yyyy':zlabel 'zzzz':"
	mgl += "box:fplot 'sin(x^2)'\n";   // This is just for testing zoomaxis features
	mgl += "text 0 0 'aaa'";
	return mgl;
}
