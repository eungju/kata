import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class HalloweenParty {
    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int tests = scanner.nextInt();
        for (int t = 0; t < tests; t++) {
            long k = scanner.nextInt();
            long r = k / 2;
            long c = k - r;
            stdout.println(r * c);
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
