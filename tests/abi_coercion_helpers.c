// Companion C file for abi_coerced_struct_shapes.cragtest.

typedef struct { float x, y, z; } V3;
typedef struct { long *p; long n; } FatPtr;
typedef struct { int a, b, c, d; } Quad;
typedef struct { int a; long b; } IntLong;
typedef struct { float x, y; int i; } FFI;
typedef struct { int a, b, c; } III;
typedef struct { long a; double b; } LongDouble;
typedef struct { unsigned char a, b, c; } B3;
typedef struct { short a, b, c; } S3;

#define BAD (-1)

V3 abi_v3(V3 v) {
    if (v.x != 1.0f || v.y != 2.0f || v.z != 3.0f)
        return (V3){BAD, BAD, BAD};
    return (V3){v.z * 10.0f, v.x * 10.0f, v.y * 10.0f};
}

FatPtr abi_fatptr(FatPtr f) {
    if (f.n != 5 || *f.p != 7)
        return (FatPtr){0, BAD};
    return (FatPtr){f.p, *f.p + f.n};
}

Quad abi_quad(Quad q) {
    if (q.a != 1 || q.b != 2 || q.c != 3 || q.d != 4)
        return (Quad){BAD, BAD, BAD, BAD};
    return (Quad){q.d, q.c, q.b, q.a};
}

IntLong abi_intlong(IntLong v) {
    if (v.a != 11 || v.b != 22)
        return (IntLong){BAD, BAD};
    return (IntLong){v.a * 2, v.b * 2};
}

FFI abi_ffi(FFI v) {
    if (v.x != 1.5f || v.y != 2.5f || v.i != 3)
        return (FFI){BAD, BAD, BAD};
    return (FFI){v.y, v.x, v.i * 100};
}

III abi_iii(III v) {
    if (v.a != 1 || v.b != 2 || v.c != 3)
        return (III){BAD, BAD, BAD};
    return (III){v.c, v.b, v.a};
}

LongDouble abi_longdouble(LongDouble v) {
    if (v.a != 40 || v.b != 0.5)
        return (LongDouble){BAD, BAD};
    return (LongDouble){v.a + 2, v.b * 4.0};
}

B3 abi_b3(B3 v) {
    if (v.a != 10 || v.b != 20 || v.c != 30)
        return (B3){0, 0, 0};
    return (B3){v.c, v.b, v.a};
}

S3 abi_s3(S3 v) {
    if (v.a != 100 || v.b != 200 || v.c != 300)
        return (S3){BAD, BAD, BAD};
    return (S3){v.c, v.b, v.a};
}

double abi_mixed(III a, double d, V3 b, long l, FFI c) {
    return (a.a + a.b + a.c) + d + (b.x + b.y + b.z) + l + (c.x + c.y + c.i);
}
