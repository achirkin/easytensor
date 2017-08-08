function h$easytensor_rotScale(quat, vec) {
    'use strict';
    var i = quat[0], j = quat[1], k = quat[2], t = quat[3];
    var x = vec[0], y = vec[1], z = vec[2];
    var l = t*t - i*i - j*j - k*k;
    var d = 2*(i*x + j*y + k*z);
    t *= 2;
    return [ l*x + d*i + t*(z*j - y*k)
           , l*y + d*j + t*(x*k - z*i)
           , l*z + d*k + t*(y*i - x*j)
           ];
}

function h$easytensor_qArg(quat) {
    'use strict';
    return Math.atan2( Math.hypot(quat[0],quat[1],quat[2]) , quat[3] ) * 2 ;
}

function h$easytensor_getRotScale(a, b) {
    'use strict';
    if (b[0] === 0 && b[1] === 0 && b[2] === 0) { return [0,0,0,0];}
    if (a[0] === 0 && a[1] === 0 && a[2] === 0) { return [Infinity,Infinity,Infinity,Infinity];}
    var t = cross(a, b);
    var ma = Math.hypot(a[0],a[1],a[2]);
    var mb = Math.hypot(b[0],b[1],b[2]);
    var dot = a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
    if (t[0] === 0 && t[1] === 0 && t[2] === 0) {
        if (dot > 0) {return [0,0,0,Math.sqrt(mb/ma)];}
        else         {return [0,0,Math.sqrt(mb/ma),0];}
    }
    var c = Math.sqrt(ma*mb + dot);
    ma *= Math.SQRT2;
    return [ (a[1]*b[2] - a[2]*b[1])/(ma*c)
           , (a[2]*b[0] - a[0]*b[2])/(ma*c)
           , (a[0]*b[1] - a[1]*b[0])/(ma*c)
           , c/ma
           ];
}

function h$easytensor_axisRotation(axis, a) {
    'use strict';
    if (axis[0] === 0 && axis[1] === 0 && axis[2] === 0) { return [0,0,0,1];}
    var c = Math.cos(a*0.5), s = Math.sin(a*0.5) / Math.hypot(axis[0],axis[1],axis[2]);
    return [ axis[0]*s, axis[1]*s, axis[2]*s, c];
}

function h$easytensor_qfromMatrix33(m) {
    'use strict';
    var d = Math.cbrt(
          m[0]*(m[4]*m[8]-m[5]*m[7])
        - m[1]*(m[3]*m[8]-m[5]*m[6])
        + m[2]*(m[3]*m[7]-m[4]*m[6]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[4] - m[8] )) * Math.sign(m[5] - m[7]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] + m[4] - m[8] )) * Math.sign(m[6] - m[2]) * 0.5
           , Math.sqrt(Math.max( 0, d - m[0] - m[4] + m[8] )) * Math.sign(m[1] - m[3]) * 0.5
           , Math.sqrt(Math.max( 0, d + m[0] + m[4] + m[8] )) * 0.5 ];
}

function h$easytensor_qfromMatrix44(m) {
    'use strict';
    var d = Math.cbrt(
          m[0]*(m[5]*m[10]-m[6]*m[9])
        - m[1]*(m[4]*m[10]-m[6]*m[8])
        + m[2]*(m[4]*m[ 9]-m[5]*m[8]));
    return [ Math.sqrt(Math.max( 0, d + m[0] - m[5] - m[10] )) * Math.sign(m[6] - m[9]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] + m[5] - m[10] )) * Math.sign(m[8] - m[2]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d - m[0] - m[5] + m[10] )) * Math.sign(m[1] - m[4]) * 0.5 / m[15]
           , Math.sqrt(Math.max( 0, d + m[0] + m[5] + m[10] )) * 0.5 / m[15] ];
}

function h$easytensor_qtoMatrix33(quat) {
    'use strict';
    var x = quat[0], y = quat[1], z = quat[2], w = quat[3];
    var w2 = w*w;
    if (x === 0 && y === 0 && z === 0) {
        return [w2,0,0
               ,0,w2,0
               ,0,0,w2];
    }
    var x2 = x*x, y2 = y*y, z2 = z*z;
    var l2 = x2+y2+z2+w2;
    return [ l2 - 2*(z2 + y2),    2*(x*y + z*w),    2*(x*z - y*w)
           ,    2*(x*y - z*w), l2 - 2*(z2 + x2),    2*(y*z + x*w)
           ,    2*(x*z + y*w),    2*(y*z - x*w), l2 - 2*(y2 + x2) ];
}

function h$easytensor_qtoMatrix44(quat) {
    'use strict';
    var x = quat[0], y = quat[1], z = quat[2], w = quat[3];
    var w2 = w*w;
    if (x === 0 && y === 0 && z === 0) {
        return [w2,0,0,0
               ,0,w2,0,0
               ,0,0,w2,0
               ,0,0,0,1];
    }
    var x2 = x*x, y2 = y*y, z2 = z*z;
    var l2 = x2+y2+z2+w2;
    return [ l2 - 2*(z2 + y2),    2*(x*y + z*w),    2*(x*z - y*w), 0
           ,    2*(x*y - z*w), l2 - 2*(z2 + x2),    2*(y*z + x*w), 0
           ,    2*(x*z + y*w),    2*(y*z - x*w), l2 - 2*(y2 + x2), 0
           , 0, 0, 0, 1];
}

function h$easytensor_qrecip(q) {
    'use strict';
    var c = -1 / (q[0]*q[0] + q[1]*q[1] + q[2]*q[2] + q[3]*q[3]);
    return [q[0]*c,q[1]*c,q[2]*c,-q[3]*c];
}

function h$easytensor_qexp(q) {
    'use strict';
    var mv = Math.hypot(q[0],q[1],q[2]), et = Math.exp(q[3]);
    if(mv === 0) {return [0,0,0,et];}
    var l = et * Math.sin(mv) / mv;
    return [q[0]*l,q[1]*l,q[2]*l,et*Math.cos(mv)];
}
function h$easytensor_qlog(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {
      if(q[3] >= 0){
        return [0,0,0,Math.log(q[3])];
      } else {
        return [Math.PI,0,0,Math.log(-q[3])];
      }
    }
    var mq = Math.sqrt(mv + q[3]*q[3]);
    mv = Math.sqrt(mv);
    var l = Math.atan2( mv, q[3] ) / mv;
    return [q[0]*l,q[1]*l,q[2]*l,Math.log(mq)];
}
function h$easytensor_qsqrt(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {
      if(q[3] >= 0){
        return [0,0,0,Math.sqrt(q[3])];
      } else {
        return [Math.sqrt(-q[3]),0,0,0];
      }
    }
    var l = Math.sqrt(mv + q[3]*q[3]);
    var l2 = Math.sqrt(l);
    var tq = q[3] / (l * 2);
    var sina = Math.sqrt(0.5 - tq) * l2 / Math.sqrt(mv);
    return [q[0]*sina,q[1]*sina,q[2]*sina,Math.sqrt(0.5 + tq) * l2];
}

// A good tutorial on complex number trigonometric functions is available here
//  http://www.milefoot.com/math/complex/functionsofi.htm
// I extend it to complex numbers by replacing complex i with quaternion vector ijk

function h$easytensor_qsin(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.sin(q[3])];}
    mv = Math.sqrt(mv);
    var l = Math.cos(q[3]) * Math.sinh(mv) / mv;
    return [q[0]*l,q[1]*l,q[2]*l, Math.sin(q[3])*Math.cosh(mv)];
}
function h$easytensor_qcos(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.cos(q[3])];}
    mv = Math.sqrt(mv);
    var l = - Math.sin(q[3]) * Math.sinh(mv) / mv;
    return [q[0]*l,q[1]*l,q[2]*l, Math.cos(q[3])*Math.cosh(mv)];
}
function h$easytensor_qtan(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.tan(q[3])];}
    mv = Math.sqrt(mv);
    var chv = Math.cosh(mv), shv = Math.sinh(mv), ct = Math.cos(q[3]), st = Math.sin(q[3]);
    var cq = 1 / (ct*ct*chv*chv + st*st*shv*shv);
    var l = chv * shv * cq / mv;
    return [q[0]*l,q[1]*l,q[2]*l, ct * st * cq];
}
function h$easytensor_qsinh(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.sinh(q[3])];}
    mv = Math.sqrt(mv);
    var l = Math.cosh(q[3]) * Math.sin(mv) / mv;
    return [q[0]*l,q[1]*l,q[2]*l, Math.sinh(q[3])*Math.cos(mv)];
}
function h$easytensor_qcosh(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.cosh(q[3])];}
    mv = Math.sqrt(mv);
    var l = Math.sinh(q[3]) * Math.sin(mv) / mv;
    return [q[0]*l,q[1]*l,q[2]*l, Math.cosh(q[3])*Math.cos(mv)];
}
function h$easytensor_qtanh(q) {
    'use strict';
    var mv = q[0]*q[0] + q[1]*q[1] + q[2]*q[2];
    if(mv === 0) {return [0,0,0,Math.tanh(q[3])];}
    mv = Math.sqrt(mv);
    var cv = Math.cos(mv), sv = Math.sin(mv), cht = Math.cosh(q[3]), sht = Math.sinh(q[3]);
    var cq = 1 / (cht*cht*cv*cv + sht*sht*sv*sv);
    var l = cv * sv * cq / mv;
    return [q[0]*l,q[1]*l,q[2]*l, cht * sht * cq];
}
