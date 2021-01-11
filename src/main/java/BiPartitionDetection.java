import java.util.ArrayList;

public class BiPartitionDetection {

    private Graph G;
    private boolean[] visited;
    private int[] colors;
    private boolean isBipartite = true;

    private boolean dfs(int v, int color) {
        visited[v] = true;
        colors[v] = color;
        for (int w : G.adj(v)) {
            if (!visited[w]) {
                if (!dfs(w, 1 - color)) return false;
            } else if (colors[w] == colors[v]) return false;
        }
        return true;
    }

    public boolean isBipartite() {
        return isBipartite;
    }

    public BiPartitionDetection(Graph G) {
        this.G = G;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        colors = new int[G.V()];
        for (int i = 0; i < G.V(); i++) {
            colors[i] = -1;
        }
        for (int v = 0; v < G.V(); v++) {
            if (!visited[v]) {
                if (!dfs(v, 0)) {
                    isBipartite = false;
                    break;
                }
            }
        }
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        BiPartitionDetection biPartitionDetection = new BiPartitionDetection(g);
        System.out.println(biPartitionDetection.isBipartite);

    }

}

