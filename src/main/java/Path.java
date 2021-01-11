import java.util.ArrayList;
import java.util.Collections;

public class Path {

    private Graph G;
    private int source;
    private int destination;
    private boolean[] visited;
    private int[] pre;


    private boolean dfs(int v, int parent) {
        visited[v] = true;
        pre[v] = parent;
        if(v == destination) return true;
        for (int w : G.adj(v)) if (!visited[w]) if(dfs(w, v)) return true;
        return false;
    }

    public Path(Graph G, int source, int destination) {

        G.validateVertex(source);
        G.validateVertex(destination);

        this.G = G;
        this.source = source;
        this.destination = destination;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        pre = new int[G.V()];

        dfs(source, source);
    }

    public boolean isConnected() {
        G.validateVertex(destination);
        return visited[destination];
    }

    public ArrayList<Integer> path() {
        ArrayList<Integer> res = new ArrayList<>();
        if (!isConnected()) return res;
        else {
            while (!(destination == source)) {
                res.add(destination);
                destination = pre[destination];
            }
            res.add(source);
            Collections.reverse(res);
            return res;
        }
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        Path ssPath = new Path(g, 0,6);
        System.out.println("0 -> 6: " + ssPath.path());
    }
}

