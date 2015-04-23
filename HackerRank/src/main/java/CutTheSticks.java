import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.*;

public class CutTheSticks {
    public Integer[] solve(int[] sticks) {
        List<Integer> cuts = new ArrayList<>();
        while (true) {
            int shortest = Integer.MAX_VALUE;
            for (int i = 0; i < sticks.length; i++) {
                if (sticks[i] > 0) {
                    shortest = Math.min(sticks[i], shortest);
                }
            }
            int cutted = 0;
            for (int i = 0; i < sticks.length; i++) {
                if (sticks[i] > 0) {
                    sticks[i] -= shortest;
                    cutted++;
                }
            }
            if (cutted == 0) {
                break;
            }
            cuts.add(cutted);
        }
        return cuts.toArray(new Integer[0]);
    }

    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int n = scanner.nextInt();
        int[] sticks = new int[n];
        for (int i = 0; i < sticks.length; i++) {
            sticks[i] = scanner.nextInt();
        }
        CutTheSticks algorithm = new CutTheSticks();
        for (int cut: algorithm.solve(sticks)) {
            stdout.println(cut);
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
