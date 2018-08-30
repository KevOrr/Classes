class Edge{

    Point p0, p1;

    Edge(Point p0, Point p1 ) {
        this.p0 = p0;
        this.p1 = p1;
    }

    void draw(){
        stroke(#000000);
        strokeWeight(2);
        line(p0.x, p0.y, p1.x, p1.y);
    }
}
