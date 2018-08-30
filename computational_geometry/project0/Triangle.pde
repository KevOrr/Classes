class Triangle {

    Point p0,p1,p2;

    Triangle(Point p0, Point p1, Point p2 ){
        this.p0 = p0;
        this.p1 = p1;
        this.p2 = p2;
    }

    void draw(){
        if (this.ccw())
            fill(#ff6666);
        else
            fill(#66ff66);

        noStroke();
        triangle(this.p0.x, this.p0.y,
                 this.p1.x, this.p1.y,
                 this.p2.x, this.p2.y);
    }

    // check if a triangle is oriented counterclockwise
    boolean ccw(){
        return Util.cross2(this.p0, this.p1, this.p0, this.p2) < 0;
    }

    // check if a triangle is oriented clockwise
    boolean cw(){
        return Util.cross2(this.p0, this.p1, this.p0, this.p2) < 0;
    }
}
