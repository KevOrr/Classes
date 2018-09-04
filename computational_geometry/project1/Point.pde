class Point {
   public float x, y;

   public Point(float x, float y){
     this.x = x;
     this.y = y;
   }

   public Point(PVector p){
     this.x = p.x;
     this.y = p.y;
   }

   public void draw(){
     ellipseMode(CENTER);
     ellipse(p.x, p.y, 10, 10);
   }

   public float distance(Point o){
     return Math.sqrt(Math.pow(this.x - o.x, 2) + Math.pow(this.y - o.y))
   }

   public String toString(){
     return String.format("Point(%f, %f)", this.x, this.y);
   }
}
