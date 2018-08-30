ArrayList<Point> points = new ArrayList<Point>();
ArrayList<Edge> edges = new ArrayList<Edge>();
ArrayList<Triangle> triangles = new ArrayList<Triangle>();


void setup(){
    size(800, 800, P3D);
    frameRate(30);
}


void draw(){
    background(255);

    // code to convert the display into a RIGHT HAND coordinate system
    // remove if you're ok using a left hand coordinate system
    translate( 0, height, 0);
    scale( 1, -1, 1 );

    for (int i=0; i < points.size(); i++) {
        if (i/3 < triangles.size() && i % 3 == 0)
            triangles.get(i / 3).draw();
        if (i < edges.size())
            edges.get(i).draw();
        points.get(i).draw();
    }
}


void mousePressed(){
    int mouseXRHC = mouseX;
    int mouseYRHC = height-mouseY;

    Point p = new Point(mouseXRHC, mouseYRHC, Integer.toString(points.size() + 1));
    points.add(p);
    if (points.size() >= 2) {
        edges.add(new Edge(points.get(points.size() - 2), points.get(points.size() - 1)));
        if (points.size() % 3 == 0)
            triangles.add(new Triangle(points.get(points.size() - 3), points.get(points.size() - 2), points.get(points.size() - 1)));
    }
}

void textRHC( int s, float x, float y ){
    textRHC( Integer.toString(s), x, y );
}

void textRHC( String s, float x, float y ){
    pushMatrix();
    translate(x,y);
    scale(1,-1,1);
    text( s, 0, 0 );
    popMatrix();
}
