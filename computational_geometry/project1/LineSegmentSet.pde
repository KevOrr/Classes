import java.util.*;

static class Event implements Comparable<Event> {
    public static int START = 1;
    public static int END = 2;
    public static int INTERSECT = 3;

    public float y;
    public int type;
    public Edge e1, e2;

    public Event(float y, int type, Edge e1, Edge e2) {
        this.y = y;
        this.type = type;
        this.e1 = e1;
        this.e2 = e2;
    }

    @Override
    public int compareTo(Event other) {
        int result = new Float(this.y).compareTo(other.y);
        if (result == 0)
            return new Integer(this.type).compareTo(other.type);
        return result;
    }
}

static class Intervals {
    public Intervals left = null;
    public Intervals right = null;
    public ArrayList<Edge> here;
    public float x;

    public Intervals(ArrayList<Edge> edges) {
        this(edges, getMinX(edges), getMaxX(edges));
    }

    private static float getMinX(ArrayList<Edge> edges) {
        float minX = Float.MAX_VALUE;
        for (Edge e : edges)
            minX = min(minX, e.minX());
        return minX;
    }

    private static float getMaxX(ArrayList<Edge> edges) {
        float maxX = Float.MIN_VALUE;
        for (Edge e : edges)
            maxX = max(maxX, e.maxX());
        return maxX;
    }

    private Intervals(ArrayList<Edge> edges, float minX, float maxX) {
        ArrayList<Edge> left = new ArrayList<Edge>();
        ArrayList<Edge> right = new ArrayList<Edge>();
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

    public ArrayList<Edge> find(float minX, float maxX) {
        if (minX < this.x)
            if (this.left != null)
                return this.left.find(minX, maxX);
            else
                return null;
        else if (this.x < maxX)
            if (this.right != null)
                return this.right.find(minX, maxX);
            else
                return null;
        else {
            ArrayList<Edge> result = new ArrayList<Edge>();
            for (Edge e : here)
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

public static int insertLine(ArrayList<Edge> active, Edge e, float eventX, float sweepY) {
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

// public static void checkIntersect


public static void OptimizedLineSegmentSetIntersection
    (ArrayList<Edge> input_edges,
     ArrayList<Point> output_intersections)
{
    output_intersections.clear();

    PriorityQueue<Event> events = new PriorityQueue<Event>();
    for (Edge e : input_edges) {
        events.add(new Event(e.minY(), Event.START, e, null));
        events.add(new Event(e.maxY(), Event.END, e, null));
    }

    // Collections.sort(events, Comparator.comparing((Event e) -> e.y).reversed());
    Intervals intervals = new Intervals(input_edges);
    ArrayList<Edge> active = new ArrayList();

    while (events.size() > 0) {
        Event e = events.poll();
        if (e.type == Event.START) {
            float x;
            if (e.e1.p0.getY() == e.y)
                x = e.e1.p0.getX();
            else if (e.e1.p1.getY() == e.y)
                x = e.e1.p1.getY();
            else
                throw new IllegalStateException("This shouldn't have happened");

            int insertionPoint = insertLine(active, e.e1, x, e.y);

            if (insertionPoint > 0) {
                Edge e1 = active.get(insertionPoint - 1);
                Edge e2 = active.get(insertionPoint);
                Point p = e1.intersectionPoint(e2);
                if (p != null) {
                    output_intersections.add(p);
                    events.add(new Event(p.getY(), Event.INTERSECT, e1, e2));
                }
            }

            if (insertionPoint < active.size()) {
                Edge e1 = active.get(insertionPoint);
                Edge e2 = active.get(insertionPoint + 1);
                Point p = e1.intersectionPoint(e2);
                if (p != null) {
                    output_intersections.add(p);
                    events.add(new Event(p.getY(), Event.INTERSECT, e1, e2));
                }
            }
        } else if (e.type == Event.END) {
            active.removeIf(new Predicate<Edge>() {
                    @Override
                    public boolean evaluate(Edge edge) {
                        return edge == e.e1;
                    }
                });
        } else if (e.type == Event.INTERSECTION) {
            int leftIdx = 0;
            for (leftIdx=0; leftIdx<active.size()-1; leftIdx++)
                if (active.get(leftIdx) == e.e1)
                    break;
            leftIdx--;

            assert leftIdx >= 0 && leftIdx < active.size();
            assert active.get(leftIdx + 1) == e.e2;

            Collections.swap(active, leftIdx, leftIdx + 1);
        }
    }
}
