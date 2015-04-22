import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.*;

public class ServiceLaneTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("8 5\n" +
                "2 3 1 2 3 2 3 3\n" +
                "0 3\n" +
                "4 6\n" +
                "6 7\n" +
                "3 5\n" +
                "0 7\n");
        ServiceLane.main(stdin, new PrintWriter(stdout));
        assertEquals("1\n" +
                "2\n" +
                "3\n" +
                "2\n" +
                "1\n", stdout.toString());
    }
}
