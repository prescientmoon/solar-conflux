# Ultra32
A vector package using the fast float32array


# Get started:

To get started, install ultra32:
```
npm install ultra32 --save
```

A ultra32 vector is just a Float32Array.

```
const vec = new Float32Array([0,0]);
```

You can perform multiple operations:
```
add(vec,new Float32Array([1,2]));
mul(vec,3);
mirror(vec);

console.log(vec); //Float32Array [ -3, -6 ]
```
