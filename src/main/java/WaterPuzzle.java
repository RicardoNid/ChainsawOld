import java.util.*;

public class WaterPuzzle {

    private int[] pre;
    private int end = -1;

    public WaterPuzzle() {
        Queue<Integer> queue = new LinkedList<>();
        boolean[] visited = new boolean[100];
        pre = new int[100];

        queue.add(0);
        visited[0] = true;
        while (!queue.isEmpty()) {
            int current = queue.remove();
            int a = current / 10, b = current % 10;

            ArrayList<Integer> nexts = new ArrayList<>();
            // 倒空
            nexts.add(a * 10);
            nexts.add(b);
            // 灌满
            nexts.add(5 * 10 + b);
            nexts.add(a * 10 + 3);
            // a -> b / b -> a
            int x = Math.min(a, 3 - b);
            nexts.add((a - x) * 10 + b + x);
            int y = Math.min(b, 5 - a);
            nexts.add((a + y) * 10 + b - y);

            for (int next : nexts) {
                if (!visited[next]) {
//                    System.out.println(next);
                    queue.add(next);
                    visited[next] = true;
                    pre[next] = current;
                    if (next / 10 == 4 || next % 10 == 4) {
                        end = next;
                        return;
                    }
                }
            }
        }
    }

    public Iterable<Integer> result() {
        ArrayList<Integer> res = new ArrayList<>();
        if (end == -1) return res;
        else {
            System.out.println(end);
            int current = end;
            while (current != 0) {
                res.add(current);
                current = pre[current];
            }
            res.add(0);
            Collections.reverse(res);
            return res;
        }
    }

    public static void main(String[] args) {
        System.out.println(new WaterPuzzle().result());
    }
}
