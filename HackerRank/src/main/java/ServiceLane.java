import java.util.Scanner;

public class ServiceLane {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        int length = scanner.nextInt();
        int tests = scanner.nextInt();
        int[] widths = new int[length];
        for (int k = 0; k < widths.length; k++) {
            widths[k] = scanner.nextInt();
        }
        for (int t = 0; t < tests; t++) {
            int i = scanner.nextInt();
            int j = scanner.nextInt();
            int minWidth = Integer.MAX_VALUE;
            for (int k = i; k <= j; k++) {
                minWidth = Math.min(minWidth, widths[k]);
            }
            int vehicle = Math.min(minWidth, 3);
            System.out.println(vehicle);
        }
    }
}
