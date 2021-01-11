import java.util.*;

public class SingleSourcePathBFS {

    private Graph G;
    private boolean[] visited;
    private int[] pre;
    private int[] dis;
    private int source;

    public SingleSourcePathBFS(Graph G, int source) { // design complexity = O(V+E)
        this.G = G;
        this.source = source;
        visited = new boolean[G.V()];
        pre = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            pre[v] = -1;
        }
        dis = new int[G.V()];
        for (int v = 0; v < G.V(); v++) {
            dis[v] = -1;
        }

        bfs(source);
    }

    public void bfs(int source) {
        Queue<Integer> queue = new LinkedList<>(); // design Queue是interface,要为它挑选具体实现
        queue.add(source);
        visited[source] = true;
        pre[source] = source;
        dis[source] = 0;
        while (!queue.isEmpty()) {
            int v = queue.remove();

            for (int w : G.adj(v)) {
                if (!visited[w]) {
                    queue.add(w);
                    visited[w] = true;
                    pre[w] = v;
                    dis[w] = dis[v] + 1;
                }
            }
        }
    }

    public boolean isConnectedTo(int destination) {
        G.validateVertex(destination);
        return visited[destination];
    }

    public Iterable<Integer> path(int destination) {
        ArrayList<Integer> res = new ArrayList<>();
        if (!isConnectedTo(destination)) return res;
        else {
            while (destination != source) {
                res.add(destination);
                destination = pre[destination];
            }
            res.add(source);
            Collections.reverse(res);
            return res;
        }
    }

    public int dis(int destination){
        return dis[destination];
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        SingleSourcePathBFS singleSourcePathBFS = new SingleSourcePathBFS(g, 0);
        System.out.println("0 -> 6 : " + singleSourcePathBFS.path(6));
        System.out.println("distance : " + singleSourcePathBFS.dis(6));
    }
}
