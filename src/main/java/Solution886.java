import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

class Solution886 {

    ArrayList<Integer>[] grpah;
    Map<Integer, Integer> colors;

    public boolean possibleBipartition(int N, int[][] dislikes) {
        // 初始化空图
        grpah = new ArrayList[N + 1];
        for (int i = 0; i < N + 1; i++) {
            grpah[i] = new ArrayList<>();
        }
        // 建图
        for (int[] dislike : dislikes) {
            grpah[dislike[0]].add(dislike[1]);
            grpah[dislike[1]].add(dislike[0]);
        }
        colors = new HashMap<>();
        for (int v = 0; v < N; v++) {
            if (!colors.containsKey(v)) {
                if (!dfs(v, 0)) return false;
            }
        }
        return true;
    }

    public boolean dfs(int source, int color) {
        for (int v : grpah[source]) {
            if(colors.containsKey(v)){
                if(colors.get(v) == color) return false;
            }
            else{
                colors.put(v, 1 - color);
                if(!dfs(v, 1 - color)) return false;
            }
        }
        return true;
    }
}