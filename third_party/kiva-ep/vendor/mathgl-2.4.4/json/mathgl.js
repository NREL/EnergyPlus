/** main mathgl namespace */
var mathgl = {
  version: '0.1.0'
}


/**
 * Auxiliary functions.
 */

/** trivial bind implementation */
mathgl.bind = function(func, context) {
  return function() {
    func.apply(context, arguments);
  };
}


/** clone */
mathgl.clone = function(obj) {
  if (obj === null || typeof(obj) != 'object') {
    return obj;
  }
  var temp = new obj.constructor();
  for (var key in obj) {
    temp[key] = mathgl.clone(obj[key]);
  }
  return temp;
}
