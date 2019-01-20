"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
function add(vector, other) {
    for (let i = 0; i < vector.length; i++) {
        vector[i] = vector[i] + other[i];
    }
    return vector;
}
exports.add = add;
function sub(vector, other) {
    for (let i = 0; i < vector.length; i++) {
        vector[i] = vector[i] - other[i];
    }
    return vector;
}
exports.sub = sub;
function div(vector, scalar) {
    for (let i = 0; i < vector.length; i++) {
        vector[i] /= scalar;
    }
    return vector;
}
exports.div = div;
function mul(vector, scalar) {
    for (let i = 0; i < vector.length; i++) {
        vector[i] *= scalar;
    }
    return vector;
}
exports.mul = mul;
function mirror(vector) {
    for (let i = 0; i < vector.length; i++) {
        vector[i] = -vector[i];
    }
    return vector;
}
exports.mirror = mirror;
function rotate(vector) {
    const oldx = vector[0];
    vector[0] = Math.cos(vector[0]) - Math.sin(vector[1]);
    vector[1] = Math.sin(oldx) + Math.cos(vector[1]);
    return vector;
}
exports.rotate = rotate;
