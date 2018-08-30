static class Util {
    static float cross2(Edge e0, Edge e1) {
        return cross2(e0.p0, e0.p1, e1.p0, e1.p1);
    }

    static float cross2(Point a0, Point a1, Point b0, Point b1) {
        return cross2(a1.x - a0.x, a1.y - a0.y, b1.x - b0.x, b1.y - b0.y);
    }

    static float cross2(float x0, float y0, float x1, float y1) {
        return x1*y0 - x0*y1;
    }
}
