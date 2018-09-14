import java.util.*;

static class Event implements Comparable<Event> {
    public static int START = 1;
    public static int END = 2;

    public float y;
    public int type;
    public Edge l;

    public Event(float y, int type, Edge l) {
        this.y = y;
        this.type = type;
        this.l = l;
    }

    @Override
    public int compareTo(Event other) {
        int result = Float.compareTo(this.y, other.y);
        if (result == 0)
            return Int.compareTo(this.type, other.type);
        return result;
    }
}

class Intervals {
    public Intervals left = null;
    public Intervals right = null;
    public ArrayList<Edge> here;
    public float x;

    public Intervals(ArrayList<Edge> edges) {
        float minX = FLOAT_MAX;
        float maxX = FLOAT_MIN;

        for (Edge e : edges) {
            minX = min(minX, e.e.minX());
            maxX = max(maxX, e.e.maxX());
        }

        this(edges, minX, maxX);
    }

    private Intervals(ArrayList<Edge> edges, float minX, float maxX) {
        ArrayList<Edges> left = new ArrayList<Edges>();
        ArrayList<Edges> right = new ArrayList<Edges>();
        this.here = new ArrayList<Edge>();

        float center = (minX + maxX) / 2.0;

        for (Edge e : edges) {
            if (e.p0.getX() < center && e.p1.getX() < center)
                left.add(e);
            else if (e.p0.getX() > center && e.p1.getX() > center)
                right.add(e);
            else
                this.here.add(e);
        }

        this.x = center;
        if (left.size() > 0)
            this.left = new Intervals(left, minX, center);
        if (right.size() > 0)
            this.right = new Intervals(right, center, maxX);
    }

    public ArrayList<Edges> find(float minX, float maxX) {
        if (minX < this.x)
            if (this.left != null)
                return this.left.find(minX, maxX);
            else
                return null
        else if (this.x < maxX)
            if (this.right != null)
                return this.right.find(minX, maxX);
            else
                return null
        else {
            ArrayList<Edges> result = new ArrayList<Edges>();
            for (Edge e : input_edges)
                if (e.minX() >= minX && e.maxX() <= maxX)
                    result.add(e);
            return result;
        }
    }
}

public static void NaiveLineSegmentSetIntersection
    (ArrayList<Edge> input_edges,
     ArrayList<Point> output_intersections)
{
    output_intersections.clear();

    for (int i=0; i<input_edges.size(); i++) {
        for (int j=i+1; j<input_edges.size(); j++) {
            Point p = input_edges.get(i).intersectionPoint(input_edges.get(j));
            if (p != null)
                output_intersections.add(p);
        }
    }

}

public static float crossY(Edge e, float y) {
    float alpha = (y - e.p0.getY())/(e.p1.getY() - e.p0.getY());
    return (1 - alpha)*e.p0.getX() + alpha*e.p1.getX();
}

public static int insertLine(ArrayList<Point> active, Edge e, float eventX, float sweepY) {
    int a = 0;
    int b = active.size();

    while (a < b) {
        int center = (a+b)/2;
        float centerCrossY = crossY(active.get(center), sweepY);
        if (centerCrossY <= eventX)
            a = center + 1;
        else if (eventX < centerCrossY)
            b = center;
    }

    assert a == b : "You screwed up binary search, probably off-by-one";

    active.add(a, e);
    return a;
}

public static void checkIntersect


public static void OptimizedLineSegmentSetIntersection
    (ArrayList<Edge> input_edges,
     ArrayList<Point> output_intersections)
{
    output_intersections.clear();

    TreeSet<Event> events = new TreeSet<Event>();
    for (Edge e : input_edges) {
        events.add(new Event(e.minY(), Event.START, e));
        events.add(new Event(e.maxY(), Event.END, e));
    }

    // Collections.sort(events, Comparator.comparing((Event e) -> e.y).reversed());
    Intervals intervals = new Intervals(input_edges);
    ArrayList<Point> active = new ArrayList();

    for (Event e : events) {
        if (e.type == Event.START) {
            float x;
            if (e.p0.getY() == e.y)
                x = e.po.getX();
            else if (e.p1.getY() == e.y)
                x = e.p1.getY();
            else
                throw new IllegalStateException("This shouldn't have happened");

            int insertionPoint = insertLine(active, e.l, x, e.y);

            if (insertionPoint > 0) {
                
            }
        }
    }

}
