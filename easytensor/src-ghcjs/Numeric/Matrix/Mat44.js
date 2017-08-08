function h$easytensor_m4fromHom(v) {
    'use strict';
    var r = new v.slice(0,3), t = v[3];
    if (t !== 0) {
      r[0] /= t; r[1] /= t; r[2] /= t;
    }
    return r;
}


function h$easytensor_m4translate(v) {
    'use strict';
    m = new v.constructor(16);
    m.set(v, 12);
    m[0] = 1;
    m[5] = 1;
    m[10] = 1;
    m[15] = 1;
    return m;
}

function h$easytensor_m4rotateX(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ 1, 0, 0, 0
           , 0, c, s, 0
           , 0,-s, c, 0
           , 0, 0, 0, 1];
}

function h$easytensor_m4rotateY(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ c, 0,-s, 0
           , 0, 1, 0, 0
           , s, 0, c, 0
           , 0, 0, 0, 1];
}

function h$easytensor_m4rotateZ(a) {
    'use strict';
    var c = Math.cos(a), s = Math.sin(a);
    return [ c, s, 0, 0
           ,-s, c, 0, 0
           , 0, 0, 1, 0
           , 0, 0, 0, 1];
}

function h$easytensor_m4rotate(vec, a) {
    'use strict';
    var c = Math.cos(a);
    var s = Math.sin(a);
    var c1 = 1 - c;
    var x = vec[0],  y = vec[1],  z = vec[2];
    return [   c + c1*x*x, c1*x*y + s*z, c1*x*z - s*y, 0
           , c1*x*y - s*z,   c + c1*y*y, c1*y*z + s*x, 0
           , c1*x*z + s*y, c1*y*z - s*x,  c  + c1*z*z, 0
           , 0, 0, 0, 1];
}

function h$easytensor_m4rotateEuler(x, y, z) {
    'use strict';
    var cx = Math.cos(x), sx = Math.sin(x), cy = Math.cos(y), sy = Math.sin(y), cz = Math.cos(z), sz = Math.sin(z);
    return [            cy*cz,           -cy*sz,     sy, 0
           , cx*sz + sx*sy*cz, cx*cz - sx*sy*sz, -sx*cy, 0
           , sx*sz - cx*sy*cz, sx*cz + cx*sy*sz,  cx*cy, 0
           , 0, 0, 0, 1];
}

function h$easytensor_m4lookAt(up,camera,point) {
    'use strict';
    var zDir = minusJSVec(camera,point);
    var t = Math.hypot.apply(null,zDir);
    zDir = zDir.map(function (e){return e / t;});
    var xDir = cross(up,zDir);
    t = Math.hypot.apply(null,xDir);
    xDir = xDir.map(function (e){return e / t;});
    var yDir = cross(zDir,xDir);
    return [ xDir[0], yDir[0], zDir[0], 0
           , xDir[1], yDir[1], zDir[1], 0
           , xDir[2], yDir[2], zDir[2], 0
           , - h$easytensor_dot(xDir,camera), - h$easytensor_dot(yDir,camera), - h$easytensor_dot(zDir,camera), 1
           ];
}

function h$easytensor_m4perspective(n, f, fovy, aspect) {
    'use strict';
    var h2 = n*Math.tan(fovy/2);
    var w2 = aspect*h2;
    return [ n/w2, 0, 0, 0
           , 0, n/h2, 0, 0
           , 0, 0, (n+f)/(n-f),-1
           , 0, 0, 2*n*f/(n-f), 0 ];
}

function h$easytensor_m4orthogonal(n, f, w, h) {
    'use strict';
    return [ 2/w,  0,          0,  0
           ,  0, 2/h,          0,  0
           ,  0,   0,    2/(n-f),  0
           ,  0,   0, (n+f)/(n-f), 1 ];
}

