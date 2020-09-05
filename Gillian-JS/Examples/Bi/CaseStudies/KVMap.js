'use strict';

/*
  @id a_Map
*/
function a_Map () {
  /* Annotation */ isObject(this);
	this._contents = {};
	return this;
}

/*
  @id b_validKey
*/
function b_validKey (key) {
	return (typeof(key) === "string")
}

/*
  @id c_mapGet
*/
function mapGet (o, k) {

  /* Annotation */ isObject(o);

  if (b_validKey(k)) {
      /* Annotation */ isObject(o._contents);
	    if (o._contents.hasOwnProperty(k)) {
	    	var result = o._contents[k];
	        return result
	    } else { return null }
	} else
	throw new Error("Invalid Key")
}

/*
  @id d_mapPut
*/
function d_mapPut (o, k, v) {

  /* Annotation */ isObject(o);

  if (b_validKey(k)) {
     /* Annotation */ isObject(o._contents);
    o._contents[k] = v;
    return v;
  } else
    throw new Error("Invalid Key")
}