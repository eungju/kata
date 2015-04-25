import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class ChocolateFeast {
    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int tests = scanner.nextInt();
        for (int t = 0; t < tests; t++) {
            int n = scanner.nextInt();
            int c = scanner.nextInt();
            int m = scanner.nextInt();
            int chocolates = (n / c);
            int wrappers = chocolates;
            while (true) {
                int free = wrappers / m;
                if (free == 0) {
                    break;
                }
                chocolates += free;
                wrappers = wrappers % m + free;
            }
            stdout.println(chocolates);
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
