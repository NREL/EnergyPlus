/**
 * Webkit backend. Backend base on the object injected by QWebView.
 */

/** constructor */
mathgl.WebkitBackend = function() {}

/** inherit from mathgl.Backend interface */
mathgl.WebkitBackend.prototype = new mathgl.Backend();


/**
 * Request for geometry data for given MGL script.
 * @param mgl {String} MGL script to build geometry
 * @param callback {Function} receiver of geometry must be a function(error, result)
 */
mathgl.WebkitBackend.prototype.geometry = function(mgl, callback) {
	var geometryData = globalBackend.geometry(mgl);

	/*
	var zlib = require('zlib');
	zlib.unzip(geometryData, function(err, buffer) {
		if (!err)	{	geometryData = buffer;	 });
	 */
	
	var obj = JSON.parse(geometryData);

  // simulate async reply
  setTimeout(function() {
    callback(null, obj);
  }, 0);
}
