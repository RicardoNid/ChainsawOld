import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Queue;

// insight 当你真正掌握一门技术之后,许多事情将变得非常简单
// 对于所有的知识和任务,都要克服情绪上的畏惧,用可知论指导自己
// 如果问题太过复杂,就增加封装
class Solution773 {

    private int[] dis;
    private int[][] connectivity = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    private boolean valid(int r, int c) {
        return (r >= 0 && r < 2 && c >= 0 && c < 3);
    }

    private int[][] decode(int encoded) {
        int[][] res = new int[2][3];
        int current = encoded;
        res[0][0] = current / 100000;
        current %= 100000;
        res[0][1] = current / 10000;
        current %= 10000;
        res[0][2] = current / 1000;
        current %= 1000;
        res[1][0] = current / 100;
        current %= 100;
        res[1][1] = current / 10;
        res[1][2] = current % 10;
        return res;
    }

    private int encode(int[][] decoded) {
        int res = 0;
        res += decoded[0][0] * 100000;
        res += decoded[0][1] * 10000;
        res += decoded[0][2] * 1000;
        res += decoded[1][0] * 100;
        res += decoded[1][1] * 10;
        res += decoded[1][2];
        return res;
    }


    public int slidingPuzzle(int[][] board) {

        if (encode(board) == 123450) return 0;

        Queue<Integer> queue = new LinkedList<>();
        dis = new int[1000000];
        for (int i = 0; i < dis.length; i++) {
            dis[i] = -1;
        }

        queue.add(encode(board));
        dis[encode(board)] = 0;
        while (!queue.isEmpty()) {
            int current = queue.remove();
            int rCurrent = -1;
            int cCurrent = -1;

            ArrayList<Integer> nexts = new ArrayList<>();
            int[][] decoded = decode(current);
            for (int r = 0; r < 2; r++) {
                for (int c = 0; c < 3; c++) {
                    if (decoded[r][c] == 0) {
                        rCurrent = r;
                        cCurrent = c;
                    }
                }
            }
            for (int[] dir : connectivity) {
                int rNext = rCurrent + dir[0];
                int cNext = cCurrent + dir[1];
                if (valid(rNext, cNext)) {
                    int temp = decoded[rCurrent][cCurrent];
                    decoded[rCurrent][cCurrent] = decoded[rNext][cNext];
                    decoded[rNext][cNext] = temp;
                    nexts.add(encode(decoded));
                    temp = decoded[rCurrent][cCurrent];
                    decoded[rCurrent][cCurrent] = decoded[rNext][cNext];
                    decoded[rNext][cNext] = temp;
                }
            }


            for (int next : nexts) {
                if (dis[next] == -1) {
//                    System.out.println(next);
                    queue.add(next);
                    dis[next] = dis[current] + 1;
                    if (next == 123450) {
                        return dis[next];
                    }
                }
            }
        }
        return -1;
    }
}