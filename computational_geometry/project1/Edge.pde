class Edge{
    Point p0, p1;
    int id;

    Edge(Point p0, Point p1, int id) {
        this.p0 = p0;
        this.p1 = p1;
        this.id = id;
    }

    void draw() {
        line(p0.p.x, p0.p.y, p1.p.x, p1.p.y);
    }

    float slope() {
        return (this.p1.getY() - this.p0.getY())/(this.p1.getX() - this.p0.getX());
    }

    float minX() {
        return min(this.p0.getX(), this.p1.getX());
    }

    float maxX() {
        return max(this.p0.getX(), this.p1.getX());
    }

    float minY() {
        return min(this.p0.getY(), this.p1.getY());
    }

    float maxY() {
        return max(this.p0.getY(), this.p1.getY());
    }

    float whichSide(Point p) {
        return PVector.sub(this.p1.p, this.p0.p).cross(PVector.sub(p.p, this.p0.p)).z;
    }

    boolean intersectionTest(Edge other) {
        return this.intersectionPoint(other) != null;
    }


    Point intersectionPoint(Edge other) {
        // TODO: Implement A Method To Find The Edge Intersection Point
        // Care should be taken to make the implementation CORRECT. Speed doesn't matter.

        if (this.slope() == other.slope())
          return null;

        float side0 = this.whichSide(other.p0);
        float side1 = this.whichSide(other.p1);
        if (side0 > 0 && side1 > 0 || side0 < 0 && side1 < 0)
          return null;

        float side2 = other.whichSide(this.p0);
        float side3 = other.whichSide(this.p1);
        if (side2 > 0 && side3 > 0 || side2 < 0 && side3 < 0)
          return null;

        PVector D = PVector.sub(this.p1.p, this.p0.p).normalize();
        PVector Q = PVector.sub(other.p1.p, other.p0.p).normalize();
        PVector R = PVector.sub(other.p0.p, this.p0.p);

        Float u = R.cross(D).mag() / D.cross(Q).mag();
        if (u.isNaN() || u.isInfinite())
          return null;

        // float t = (R.x + Q.x*u)/D.x;
        // if (t.isNaN || t.isInfinite()) {
        //     t = (R.y + Q.y*u)/D.y;
        //     if (t.isNaN || t.isInfinite())
        //         return null;
        // }

        PVector p = PVector.add(other.p0.p, PVector.mult(Q, u));

        return new Point(p);
    }

    Point optimizedIntersectionPoint(Edge other) {
        // TODO: Implement A Faster Method To Find The Edge Intersection Point
        // The result should be correct, but SPEED MATTERS.

        return this.intersectionPoint(other);
    }
}
