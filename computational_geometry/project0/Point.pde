class Point {

   float x, y;
   String label = "";

   Point( float x, float y, String label ){
       this.x = x;
       this.y = y;
       this.label = label;
   }

   void draw(){
       ellipseMode(RADIUS);
       noStroke();
       fill(#000000);
       ellipse(this.x, this.y, 3, 3);

       stroke(#000000);
       textRHC(this.label, this.x + 5, this.y - 10);
   }
}
