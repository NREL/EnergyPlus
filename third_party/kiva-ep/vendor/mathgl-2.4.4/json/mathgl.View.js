/**
 * Standard 3D view - camera flying along the surface of a sphere.
 */


/** @constructor */
mathgl.View = function() {
	this.__canvas = null;
	this.__renderLauncherFunc = null;
	this.__pickPointHandlerFunc = null;
	this.__onCameraChanged = null;
	this.__onMouseMoveFunc = mathgl.bind(this.__onMouseMove, this);
	this.__onMouseDownFunc = mathgl.bind(this.__onMouseDown, this);
	this.__onMouseUpFunc = mathgl.bind(this.__onMouseUp, this);
	this.__onMouseOutFunc = mathgl.bind(this.__onMouseOut, this);
	this.__onDblClickFunc = mathgl.bind(this.__onDblClick, this);
	this.__onMouseWheelFunc = mathgl.bind(this.__onMouseWheel, this);

	// mouse state
	this.__isMouseDown = false;
	this.__mouseX = -1;
	this.__mouseY = -1;

	// current camera position
	this.__distance = 1.0;
	this.__pitch = 0;
	this.__yaw = 0;
	this.__theta = 0;
	// by default pitch rotation is unlimited
	this.__limitedPitchRotation = false;
}


/**
 * Load view state from JSON string.
 * @param json {String} string in JSON format with previously saved state
 */
mathgl.View.prototype.load = function(json) {
	throw new Error("not implemented");
}


/**
 * Save view state to JSON string.
 * @return {String} view state serialized to JSON string
 */
mathgl.View.prototype.save = function() {
	throw new Error("not implemented");
}


/**
 * Attach given canvas to this view (Bind current view with given canvas). View instance will start procesing
 * canvas events since it is attached to it.
 * @param canvas {Canvas} canvas to attach
 */
mathgl.View.prototype.attachCanvas = function(canvas) {
	// remember canvas
	this.__canvas = canvas;
	// connect mouse events
	this.__canvas.addEventListener("mousemove", this.__onMouseMoveFunc, false);
	this.__canvas.addEventListener("mousedown", this.__onMouseDownFunc, false);
	this.__canvas.addEventListener("mouseup",   this.__onMouseUpFunc, false);
	this.__canvas.addEventListener("mouseout",  this.__onMouseOutFunc, false);
	this.__canvas.addEventListener("dblclick",  this.__onDblClickFunc, false);
//	this.__canvas.addEventListener("mousewheel",  this.__onMouseWheelFunc, false);
	if ("onwheel" in document.createElement("div")) {
		this.__canvas.addEventListener("wheel",  this.__onMouseWheelFunc, false);
	} else {
		this.__canvas.addEventListener("mousewheel",  this.__onMouseWheelFunc, false);
	}

	// initiate redraw
	this.__renderLauncherFunc();
}


/** Detach any previously attached canvas from this view instance. */
mathgl.View.prototype.detachCanvas = function() {
	// disconnect mouse events
	this.__canvas.removeEventListener("mousemove", this.__onMouseMoveFunc, false);
	this.__canvas.removeEventListener("mousedown", this.__onMouseDownFunc, false);
	this.__canvas.removeEventListener("mouseup", this.__onMouseUpFunc, false);
	this.__canvas.removeEventListener("mouseout", this.__onMouseOutFunc, false);
	this.__canvas.removeEventListener("dblclick", this.__onDblClickFunc, false);
//	this.__canvas.removeEventListener("mousewheel",  this.__onMouseWheelFunc, false);
	if ("onwheel" in document.createElement("div")) {
		this.__canvas.removeEventListener("wheel",  this.__onMouseWheelFunc, false);
	} else {
		this.__canvas.removeEventListener("mousewheel",  this.__onMouseWheelFunc, false);
	}

	// drop canvas
	this.__canvas = null;
}


/**
 * Set render launcher - callback function which is invoked when graph shall be redrawn (after some user actions).
 * @param renderLauncherFunc {Function} callback function which will be invoked when graph shall be redrawn
 */
mathgl.View.prototype.setRenderLauncher = function(renderLauncherFunc) {
	this.__renderLauncherFunc = renderLauncherFunc;
}


/**
 * Set pick point handler - callback function which is invoked when user request to highlight or show coordinates of point picked.
 * @param pickPointHandler {Function} callback function which will be invoked when user pick the point
 */
mathgl.View.prototype.setPickPointHandler = function(pickPointHandlerFunc) {
	this.__pickPointHandlerFunc = pickPointHandlerFunc;
}


/**
 * Get current view view matrix.
 * @return {Matrix} view matrix
 */
mathgl.View.prototype.viewMatrix = function() {
	var d = this.__distance;
	var cp = Math.cos(this.__pitch);
	var sp = Math.sin(this.__pitch);
	var cy = Math.cos(this.__yaw);
	var sy = Math.sin(this.__yaw);
	var ct = Math.cos(this.__theta);
	var st = Math.sin(this.__theta);
	var lh = true; // coordinate system is left handed

	var distanceMatrix = $M([[1, 0,  0, 0],
							[0, 1,  0, 0],
							[0, 0,  1, 0],
							[0, 0, d, 1]]);
	var pitchMatrix = $M(  [[1,              0,              0, 0],
							[0,  cp           ,  lh ? sp : -sp, 0],
							[0,  lh ? -sp : sp,             cp, 0],
							[0,              0,              0, 1]]);
	var yawMatrix = $M([[ cy, 0, -sy, 0],
						[  0, 1,   0, 0],
						[ sy, 0,  cy, 0],
						[  0, 0,   0, 1]]);
	var viewMatrix = $M([[ ct,-st, 0, 0],
						[  st, ct, 0, 0],
						[  0,  0,  1, 0],
						[  0,  0,  0, 1]]);
	viewMatrix = viewMatrix.x(distanceMatrix);
	viewMatrix = viewMatrix.x(pitchMatrix);
	viewMatrix = viewMatrix.x(yawMatrix);
	return viewMatrix;
}

/** TODO: add function returning current view area in world (model) coordinates */
mathgl.View.prototype.viewArea = function() {
	throw new Error("not implemented");
}


mathgl.View.prototype.__onMouseMove = function(e) {
	if (this.__isMouseDown) {
		var x = e.layerX;
		var y = e.layerY;
		this.__yaw += 0.5 * (this.__mouseX - x) * Math.PI / 180;
		this.__pitch += 0.5 * (y - this.__mouseY) * Math.PI / 180;
		this.__mouseX = x;
		this.__mouseY = y;
		if (this.__limitedPitchRotation) {
			this.__pitch = Math.min(this.__pitch, 0.999*Math.PI/2);
			this.__pitch = Math.max(this.__pitch, -0.999*Math.PI/2);
		} else {
			if (this.__pitch > 63) this.__pitch -= 20*Math.PI;
			if (this.__pitch < -63) this.__pitch += 20*Math.PI;
		}

		if(this.__yaw > 63)	this.__yaw -= 20*Math.PI;
		if(this.__yaw <-63)	this.__yaw += 20*Math.PI;
// 		this.__yaw = Math.min(this.__yaw, Math.PI);
// 		this.__yaw = Math.max(this.__yaw, -Math.PI);

    this.__notifyCameraChanged();
		this.__renderLauncherFunc();
	}
}


mathgl.View.prototype.__onMouseDown = function(e) {
	this.__mouseX = e.layerX;
	this.__mouseY = e.layerY;
	this.__isMouseDown = true;
}


mathgl.View.prototype.__onMouseUp = function() {
	this.__isMouseDown = false;
}


mathgl.View.prototype.__onMouseOut = function() {
	this.__isMouseDown = false;
}


mathgl.View.prototype.__onDblClick = function(e) {
	this.__pickPointHandlerFunc(e.layerX, e.layerY);
}


mathgl.View.prototype.__onMouseWheel = function(e) {
	this.__isMouseDown = false;
//	this.__distance -= 0.1 * e.wheelDelta / 120;

	var wheelSensivity = 0.1;
	if (e.wheelDelta !== undefined) {
		// handle deprecated 'mousewheel' event (old webkit in chrome and safari)
		// wheelDelta is always 120 and inverted to standard deltaY
		var direction = e.wheelDelta >= 0 ? -1 : 1;
		this.__distance += -1. * wheelSensivity * direction;
	} else if (e.deltaY !== undefined) {
		// handle standard W3C DOM Level 3 'wheel' event
		var direction = e.deltaY >= 0 ? -1 : 1;
		this.__distance += wheelSensivity * direction;
	}

	this.__distance = Math.min(this.__distance, 10.0);
	this.__distance = Math.max(this.__distance, 0.2);
  this.__notifyCameraChanged();
	this.__renderLauncherFunc();
}

mathgl.View.prototype.getViewpoint = function() {
	return { 
		distance : this.__distance, 
		pitch : this.__pitch, 
		yaw : this.__yaw
	}; 
} 

mathgl.View.prototype.setViewpoint = function(distance, pitch, yaw) { 
	this.__distance = distance; 
	this.__pitch = pitch; 
	this.__yaw = yaw; 
	this.__renderLauncherFunc(); 
} 

mathgl.View.prototype.destroy = function() { 
	this.__renderLauncherFunc = null; 
	this.__pickPointHandlerFunc = null; 
	this.detachCanvas(); 
} 

mathgl.View.prototype.setCameraEventHandler = function(handler) { 
	this.__onCameraChanged = handler; 
}

mathgl.View.prototype.__notifyCameraChanged = function() {
	if(this.__onCameraChanged) {
		this.__onCameraChanged(this.getViewpoint());
	}
}

/**
 * Limit the pitch control rotation to +-90 degrees
 * @param limitedPitch {Boolean} enable or disable limit
 */
mathgl.View.prototype.setLimitedPitchRotation = function(limitedPitch) {
	this.__limitedPitchRotation = limitedPitch;
}
