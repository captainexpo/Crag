typedef struct { unsigned char r, g, b, a; } Color; // 4 bytes, INTEGER class

int sum_color(Color c) {
    return (int)c.r + (int)c.g + (int)c.b + (int)c.a;
}

typedef struct { float x, y; } V2; // 8 bytes, all-SSE class

V2 make_v2(float a, float b) {
    V2 v;
    v.x = a * 2.0f;
    v.y = b * 3.0f;
    return v;
}

typedef struct { unsigned int id; int w, h, mip, fmt; } Tex;
typedef struct { int a, b, c; Tex tex; void *p1, *p2; } Big;

Big make_big(int seed) {
    Big v;
    v.a = seed + 1;
    v.b = seed + 2;
    v.c = seed + 3;
    v.tex.id = (unsigned int)(seed + 10);
    v.tex.w = seed + 11;
    v.tex.h = seed + 12;
    v.tex.mip = seed + 13;
    v.tex.fmt = seed + 14;
    v.p1 = (void *)(long)(seed + 100);
    v.p2 = (void *)(long)(seed + 200);
    return v;
}

int read_big_tex_h(Big v) {
    return v.tex.h;
}

int combine_v2_and_color(V2 pos, Color tint) {
    return (int)pos.x + (int)pos.y + (int)tint.r + (int)tint.g + (int)tint.b + (int)tint.a;
}

int combine_big_v2_color(Big big, int extra, V2 pos, Color tint) {
    return big.tex.h + extra + (int)pos.x + (int)pos.y + (int)tint.r + (int)tint.a;
}
