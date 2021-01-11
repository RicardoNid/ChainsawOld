import java.util.ArrayList;

public class CycleDetection {

    private Graph G;
    private boolean[] visited;
    private boolean hasCycle;

    private boolean dfs(int v, int parent) {
        visited[v] = true;
        for (int w : G.adj(v)) {
            if (!visited[w]) {
                if (dfs(w, v)) return true;
            } else if (w != parent) return true;
        }
        return false;
    }

    public CycleDetection(Graph G) {
        this.G = G;
        visited = new boolean[G.V()]; // design 默认初始值为false?
        for (int v = 0; v < G.V(); v++) {
            if (!visited[v]) {
                if(dfs(v, v)){
                    hasCycle = true;
                    break;
                }
            }
        }
    }

    public boolean hasCycle() {
        return hasCycle;
    }

    public static void main(String[] args) {
        Graph g = new Graph("src/g.txt");
        CycleDetection cycleDetection = new CycleDetection(g);
        System.out.println(cycleDetection.hasCycle);
    }

}

