import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Scanner;

public class FindDigits {
    public static void main(Reader stdin, PrintWriter stdout) {
        Scanner scanner = new Scanner(stdin);
        int tests = scanner.nextInt();
        for (int t = 0; t < tests; t++) {
            long n = scanner.nextInt();
            long digits = 0;
            long dividend = n;
            while (dividend != 0) {
                long divisor = dividend % 10;
                if (divisor != 0 && n % divisor == 0) {
                    digits++;
                }
                dividend = dividend / 10;
            }
            stdout.println(digits);
        }
        stdout.flush();
    }

    public static void main(String[] args) {
        main(new InputStreamReader(System.in), new PrintWriter(System.out));
    }
}
