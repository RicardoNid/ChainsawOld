import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class SingleSourcePath {

    private Graph G;
    private int source;
    private boolean[] visited;
    private int[] pre;


    private void dfs(int v, int parent) {
        visited[v] = true;
        pre[v] = parent;
        for (int w : G.adj(v)) if (!visited[w]) dfs(w, v);
    }

    public SingleSourcePath(Graph G, int source) {

        G.validateVertex(source);

        this.G = G;
        this.source = source;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        pre = new int[G.V()];

        dfs(source, source);
    }

    public boolean isConnectedTo(int destination) {
        G.validateVertex(destination);
        return visited[destination];
    }

    public ArrayList<Integer> path(int destination) {
        ArrayList<Integer> res = new ArrayList<>();
        if (!isConnectedTo(destination)) return res;
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
        SingleSourcePath ssPath = new SingleSourcePath(g, 0);
        System.out.println("0 -> 6: " + ssPath.path(6));
        System.out.println("0 -> 5: " + ssPath.path(5));
    }
}

