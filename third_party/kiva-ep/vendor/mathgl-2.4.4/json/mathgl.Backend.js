/**
 * Backend interface. The only responsibility its successors is to return geometry object by given MGL script.
 */

/** constructor */
mathgl.Backend = function() {}

/**
 * Request for geometry data for given MGL script.
 * @param mgl {String} MGL script to build geometry
 * @param callback {Function} receiver of geometry must be a function(error, result)
 */
mathgl.Backend.prototype.geometry = function(mgl, callback) { throw new Error("abstract method invoked"); }
