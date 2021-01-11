

import java.util.Queue;
import java.util.ArrayList;
import java.util.LinkedList;

public class GraphBFS {

    private Graph G;
    private boolean[] visited;

    private ArrayList<Integer> order = new ArrayList<>();

    public GraphBFS(Graph G) { // design complexity = O(V+E)
        this.G = G;
        visited = new boolean[G.V()];

        for (int v = 0; v < G.V(); v++) {
            if (!visited[v]) bfs(v);
        }
    }

    public Iterable<Integer> order() {
        return order;
    }

    public void bfs(int source) {
        Queue<Integer> queue = new LinkedList<>(); // design Queue是interface,要为它挑选具体实现
        queue.add(source);
        visited[source] = true;
        while (!queue.isEmpty()) {
            int v = queue.remove();
            order.add(v);

            for (int w : G.adj(v)) {
                if (!visited[w]) {
                    queue.add(w);
                    visited[w] = true;
                }
            }
        }
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        GraphBFS graphBFS = new GraphBFS(g);
        System.out.println("BFS Order : " + graphBFS.order());
    }
}
