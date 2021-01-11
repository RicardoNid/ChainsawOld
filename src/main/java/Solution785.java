class Solution785 {

    private boolean[] visited;
    private int[] colors;
    private int[][] graph;

    private boolean dfs(int current, int color) {
        visited[current] = true;
        colors[current] = color;

        for (int neightbor : graph[current]) {
            if (!visited[neightbor]) {
                if (!dfs(neightbor, 1 - color)) return false;
            } else if (colors[neightbor] == colors[current]) return false;
        }
        return true;
    }

    public boolean isBipartite(int[][] graph) {

        int V = graph.length;
        visited = new boolean[V];
        colors = new int[V];
        this.graph = graph;
        for (boolean visit : visited) {
            visit = false;
        }
        for (int color : colors) {
            color = -1;
        }

        for (int v = 0; v < V; v++) {
            if (!visited[v]) {
                if (!dfs(v, 0)) return false;
            }
        }
        return true;
    }
}