
// hypot may be not supported on old browsers and IE
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot
Math.hypot = Math.hypot || function() {
  var y = 0;
  var length = arguments.length;

  for (var i = 0; i < length; i++) {
    if (arguments[i] === Infinity || arguments[i] === -Infinity) {
      return Infinity;
    }
    y += arguments[i] * arguments[i];
  }
  return Math.sqrt(y);
};


// ---------------------------------------------------------------------------------------- //
// --- Polyfills for partially missing math functions ------------------------------------- //
// ---------------------------------------------------------------------------------------- //

Math.hypot = Math.hypot || function() {
  var y = 0;
  var length = arguments.length;

  for (var i = 0; i < length; i++) {
    if (arguments[i] === Infinity || arguments[i] === -Infinity) {
      return Infinity;
    }
    y += arguments[i] * arguments[i];
  }
  return Math.sqrt(y);
};
Math.tanh = Math.tanh || function(x){
    var a = Math.exp(+x), b = Math.exp(-x);
    return a == Infinity ? 1 : b == Infinity ? -1 : (a - b) / (a + b);
};
Math.atanh = Math.atanh || function(x) {
  return Math.log((1+x)/(1-x)) / 2;
};
Math.acosh = Math.acosh || function(x) {
  return Math.log(x + Math.sqrt(x * x - 1));
};
Math.asinh = Math.asinh || function(x) {
  if (x === -Infinity) {
    return x;
  } else {
    return Math.log(x + Math.sqrt(x * x + 1));
  }
};
Math.cosh = Math.cosh || function(x) {
  var y = Math.exp(x);
  return (y + 1 / y) / 2;
};
Math.sinh = Math.sinh || function(x) {
  var y = Math.exp(x);
  return (y - 1 / y) / 2;
};

// ---------------------------------------------------------------------------------------- //
// --- Polyfills for partially missing typed array functions ------------------------------ //
// ---------------------------------------------------------------------------------------- //

(function () {

function polyfill_map(q) {
  if (!q.prototype.map) {
    q.prototype.map = function(f) {
      var y = new this.constructor(this.length);
      for(var i = 0; i < this.length; i++) {
        y[i] = f(this[i],i,this);
      }
      return y;
    };
  }
}
function polyfill_fill(q) {
  if (!q.prototype.fill) {
    q.prototype.fill = function(val, start, end) {
      start = start === undefined ? 0 : (start < 0 ? this.length - start : start);
      end = end === undefined ? this.length : (end < 0 ? this.length - end : end);
      for(var i = start; i < end; i++) {
        this[i] = val;
      }
      return this;
    };
  }
}
function polyfill_reduce(q) {
  if (!q.prototype.reduce) {
    q.prototype.reduce = function(f, y0) {
      var i0 = y0 === undefined ? 1 : 0,
          y  = i0 === 1 ? this[0] : y0;
      for(var i = i0; i < this.length; i++) {
        y = f(y,this[i],i,this);
      }
      return y;
    };
  }
}
function polyfill_slice(q) {
  if (!q.prototype.slice) {
    q.prototype.slice = function(start, end) {
      start = start === undefined ? 0 : (start < 0 ? this.length - start : start);
      end = end === undefined ? this.length : (end < 0 ? this.length - end : end);
      var xview = this.subarray(start, end),
          y = new xview.constructor(xview.length);
      y.set(xview);
      return y;
    };
  }
}
function polyfill_every(q) {
  if (!q.prototype.every) {
    q.prototype.every = function(f) {
      if (this.length === 0) return true;
      for(var i = 0; i < this.length; i++) {
        if (!f(this[i],i,this)) { return false; }
      }
      return true;
    };
  }
}
function polyfill_some(q) {
  if (!q.prototype.some) {
    q.prototype.some = function(f) {
      if (this.length === 0) return false;
      for(var i = 0; i < this.length; i++) {
        if (f(this[i],i,this)) { return true; }
      }
      return false;
    };
  }
}


var methods = [polyfill_map, polyfill_fill, polyfill_reduce, polyfill_slice, polyfill_every, polyfill_some];

if (typeof Int8Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Int8Array); }
}
if (typeof Uint8Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Uint8Array); }
}
if (typeof Uint8ClampedArray !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Uint8ClampedArray); }
}
if (typeof Int16Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Int16Array); }
}
if (typeof Uint16Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Uint16Array); }
}
if (typeof Int32Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Int32Array); }
}
if (typeof Uint32Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Uint32Array); }
}
if (typeof Float32Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Float32Array); }
}
if (typeof Float64Array !== 'undefined') {
    for (var i = methods.length; i--;) { methods[i](Float64Array); }
}

}());

// ---------------------------------------------------------------------------------------- //


function h$easytensor_transpose(n, mat) {
    'use strict';
    var nmat = new mat.constructor(mat.length),
        m = Math.round(mat.length / n);
    for(var i = 0; i < n; i++) {
        for(var j = 0; j < m; j++) {
            nmat[i*m+j] = mat[j*n+i];
        }
    }
    return nmat;
}

function h$easytensor_eyeFloat32(n) {
    'use strict';
    var mat = new Float32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeFloat64(n) {
    'use strict';
    var mat = new Float64Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeInt8(n) {
    'use strict';
    var mat = new Int8Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeInt16(n) {
    'use strict';
    var mat = new Int16Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeInt32(n) {
    'use strict';
    var mat = new Int32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeUint8(n) {
    'use strict';
    var mat = new Uint8Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeUint8Clamped(n) {
    'use strict';
    var mat = new Uint8ClampedArray(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeUint16(n) {
    'use strict';
    var mat = new Uint16Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}
function h$easytensor_eyeUint32(n) {
    'use strict';
    var mat = new Uint32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=1;}
    return mat;
}


function h$easytensor_diagFloat32(n,x) {
    'use strict';
    'use strict';
    var mat = new Float32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagFloat64(n,x) {
    'use strict';
    var mat = new Float64Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagInt8(n,x) {
    'use strict';
    var mat = new Int8Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagInt16(n,x) {
    'use strict';
    var mat = new Int16Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagInt32(n,x) {
    'use strict';
    var mat = new Int32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagUint8(n,x) {
    'use strict';
    var mat = new Uint8Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagUint8Clamped(n,x) {
    'use strict';
    var mat = new Uint8ClampedArray(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagUint16(n,x) {
    'use strict';
    var mat = new Uint16Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}
function h$easytensor_diagUint32(n,x) {
    'use strict';
    var mat = new Uint32Array(n*n);
    for(var i = 0; i < n*n; i += n + 1){mat[i]=x;}
    return mat;
}

function h$easytensor_trace(mat, n) {
    'use strict';
    var r = 0;
    for(var i = 0; i < n*n; i += n + 1){r+=mat[i];}
    return r;
}


function h$easytensor_det(mat, n) {
    'use strict';
    switch (n) {
    case 1:
        return mat[0];
    case 2:
        return h$easytensor_detJSMat2(mat);
    case 3:
        return h$easytensor_detJSMat3(mat);
    case 4:
        return h$easytensor_detJSMat4(mat);
    default:
        throw "Determinant for n = " + n + " is not implemented or does not make sense.";
    }
}

function h$easytensor_detJSMat2(mat) {
    'use strict';
    return (mat[0]*mat[3] - mat[1]*mat[2]);
}

function h$easytensor_detJSMat3(mat) {
    'use strict';
    return (
          mat[0]*(mat[4]*mat[8]-mat[5]*mat[7])
        - mat[1]*(mat[3]*mat[8]-mat[5]*mat[6])
        + mat[2]*(mat[3]*mat[7]-mat[4]*mat[6])
    );
}

function h$easytensor_detJSMat4(mat) {
    'use strict';
    var n11 = mat[ 0 ], n12 = mat[ 4 ], n13 = mat[ 8 ], n14 = mat[ 12 ];
    var n21 = mat[ 1 ], n22 = mat[ 5 ], n23 = mat[ 9 ], n24 = mat[ 13 ];
    var n31 = mat[ 2 ], n32 = mat[ 6 ], n33 = mat[ 10 ], n34 = mat[ 14 ];
    var n41 = mat[ 3 ], n42 = mat[ 7 ], n43 = mat[ 11 ], n44 = mat[ 15 ];

    return (
        n41 * (
        + n14 * n23 * n32
         - n13 * n24 * n32
         - n14 * n22 * n33
         + n12 * n24 * n33
         + n13 * n22 * n34
         - n12 * n23 * n34
        ) +
        n42 * (
        + n11 * n23 * n34
         - n11 * n24 * n33
         + n14 * n21 * n33
         - n13 * n21 * n34
         + n13 * n24 * n31
         - n14 * n23 * n31
        ) +
        n43 * (
        + n11 * n24 * n32
         - n11 * n22 * n34
         - n14 * n21 * n32
         + n12 * n21 * n34
         + n14 * n22 * n31
         - n12 * n24 * n31
        ) +
        n44 * (
        - n13 * n22 * n31
         - n11 * n23 * n32
         + n11 * n22 * n33
         + n13 * n21 * n32
         - n12 * n21 * n33
         + n12 * n23 * n31
        )
    );
}




function h$easytensor_inverse(mat, n) {
    'use strict';
    switch (n) {
    case 1:
        return 1 / mat[0];
    case 2:
        return h$easytensor_inverseJSM2(mat);
    case 3:
        return h$easytensor_inverseJSM3(mat);
    case 4:
        return h$easytensor_inverseJSM4(mat);
    default:
        throw "Inverse for n = " + n + " is not implemented or does not make sense.";
    }
}


function h$easytensor_inverseJSM4(mat) {
    'use strict';
    var rez = new mat.constructor(16);
    rez[0]  = mat[13]*(mat[ 6]*mat[11]-mat[10]*mat[ 7])+mat[ 9]*(mat[14]*mat[ 7]-mat[ 6]*mat[15])+mat[ 5]*(mat[10]*mat[15]-mat[14]*mat[11]);
    rez[4]  = mat[12]*(mat[10]*mat[ 7]-mat[ 6]*mat[11])+mat[ 8]*(mat[ 6]*mat[15]-mat[14]*mat[ 7])+mat[ 4]*(mat[14]*mat[11]-mat[10]*mat[15]);
    rez[8]  = mat[12]*(mat[ 5]*mat[11]-mat[ 9]*mat[ 7])+mat[ 8]*(mat[13]*mat[ 7]-mat[ 5]*mat[15])+mat[ 4]*(mat[ 9]*mat[15]-mat[13]*mat[11]);
    rez[12] = mat[12]*(mat[ 9]*mat[ 6]-mat[ 5]*mat[10])+mat[ 8]*(mat[ 5]*mat[14]-mat[13]*mat[ 6])+mat[ 4]*(mat[13]*mat[10]-mat[ 9]*mat[14]);
    rez[1]  = mat[13]*(mat[10]*mat[ 3]-mat[ 2]*mat[11])+mat[ 9]*(mat[ 2]*mat[15]-mat[14]*mat[ 3])+mat[ 1]*(mat[14]*mat[11]-mat[10]*mat[15]);
    rez[5]  = mat[12]*(mat[ 2]*mat[11]-mat[10]*mat[ 3])+mat[ 8]*(mat[14]*mat[ 3]-mat[ 2]*mat[15])+mat[ 0]*(mat[10]*mat[15]-mat[14]*mat[11]);
    rez[9]  = mat[12]*(mat[ 9]*mat[ 3]-mat[ 1]*mat[11])+mat[ 8]*(mat[ 1]*mat[15]-mat[13]*mat[ 3])+mat[ 0]*(mat[13]*mat[11]-mat[ 9]*mat[15]);
    rez[13] = mat[12]*(mat[ 1]*mat[10]-mat[ 9]*mat[ 2])+mat[ 8]*(mat[13]*mat[ 2]-mat[ 1]*mat[14])+mat[ 0]*(mat[ 9]*mat[14]-mat[13]*mat[10]);
    rez[2]  = mat[13]*(mat[ 2]*mat[ 7]-mat[ 6]*mat[ 3])+mat[ 5]*(mat[14]*mat[ 3]-mat[ 2]*mat[15])+mat[ 1]*(mat[ 6]*mat[15]-mat[14]*mat[ 7]);
    rez[6]  = mat[12]*(mat[ 6]*mat[ 3]-mat[ 2]*mat[ 7])+mat[ 4]*(mat[ 2]*mat[15]-mat[14]*mat[ 3])+mat[ 0]*(mat[14]*mat[ 7]-mat[ 6]*mat[15]);
    rez[10] = mat[12]*(mat[ 1]*mat[ 7]-mat[ 5]*mat[ 3])+mat[ 4]*(mat[13]*mat[ 3]-mat[ 1]*mat[15])+mat[ 0]*(mat[ 5]*mat[15]-mat[13]*mat[ 7]);
    rez[14] = mat[12]*(mat[ 5]*mat[ 2]-mat[ 1]*mat[ 6])+mat[ 4]*(mat[ 1]*mat[14]-mat[13]*mat[ 2])+mat[ 0]*(mat[13]*mat[ 6]-mat[ 5]*mat[14]);
    rez[3]  = mat[ 9]*(mat[ 6]*mat[ 3]-mat[ 2]*mat[ 7])+mat[ 5]*(mat[ 2]*mat[11]-mat[10]*mat[ 3])+mat[ 1]*(mat[10]*mat[ 7]-mat[ 6]*mat[11]);
    rez[7]  = mat[ 8]*(mat[ 2]*mat[ 7]-mat[ 6]*mat[ 3])+mat[ 4]*(mat[10]*mat[ 3]-mat[ 2]*mat[11])+mat[ 0]*(mat[ 6]*mat[11]-mat[10]*mat[ 7]);
    rez[11] = mat[ 8]*(mat[ 5]*mat[ 3]-mat[ 1]*mat[ 7])+mat[ 4]*(mat[ 1]*mat[11]-mat[ 9]*mat[ 3])+mat[ 0]*(mat[ 9]*mat[ 7]-mat[ 5]*mat[11]);
    rez[15] = mat[ 8]*(mat[ 1]*mat[ 6]-mat[ 5]*mat[ 2])+mat[ 4]*(mat[ 9]*mat[ 2]-mat[ 1]*mat[10])+mat[ 0]*(mat[ 5]*mat[10]-mat[ 9]*mat[ 6]);
    var det = mat[ 0]*rez[ 0] + mat[ 1]*rez[ 4] + mat[ 2]*rez[ 8] + mat[3]*rez[12];
    if (det === 0) {
        return undefined;
    } else {
        for(var i = 0; i < 16; i++) {rez[i] /= det;}
        return rez;
    }
}

function h$easytensor_inverseJSM3(mat) {
    'use strict';
    var rez = new mat.constructor(9);
    rez[0] = mat[4]*mat[8] - mat[7]*mat[5];
    rez[3] = mat[6]*mat[5] - mat[3]*mat[8];
    rez[6] = mat[3]*mat[7] - mat[6]*mat[4];
    rez[1] = mat[7]*mat[2] - mat[1]*mat[8];
    rez[4] = mat[0]*mat[8] - mat[6]*mat[2];
    rez[7] = mat[6]*mat[1] - mat[0]*mat[7];
    rez[2] = mat[1]*mat[5] - mat[4]*mat[2];
    rez[5] = mat[3]*mat[2] - mat[0]*mat[5];
    rez[8] = mat[0]*mat[4] - mat[3]*mat[1];
    var det = mat[0]*rez[0] + mat[1]*rez[3] + mat[2]*rez[6];
    if (det === 0) {
        return undefined;
    } else {
        for(var i = 0; i < 9; i++) {rez[i] /= det;}
        return rez;
    }
}

function h$easytensor_inverseJSM2(mat) {
    'use strict';
    var det = mat[0]*mat[3] - mat[1]*mat[2];
    if (det === 0) {
        return undefined;
    }
    var rez = new mat.constructor(4);
    rez[0] = mat[3]/det;
    rez[2] = -mat[1]/det;
    rez[1] = -mat[2]/det;
    rez[3] = mat[0]/det;
    return rez;
}


function h$easytensor_contract(n,m,k,lhs,rhs) {
    'use strict';
    var t, rez = new lhs.constructor(n*k);
    for(var i = 0; i < n; i++) {
        for(var j = 0; j < k; j++) {
            t = 0;
            for(var l = 0; l < m; l++) {
                t += lhs[i+l*n]*rhs[l+j*m];
            }
            rez[i+j*n] = t;
        }
    }
    return rez;
}


function h$easytensor_dot(lhs, rhs) {
    'use strict';
    return lhs.reduce(function (r, e, i) { return r + e*rhs[i];}, 0);
}


