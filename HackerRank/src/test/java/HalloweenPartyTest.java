import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class HalloweenPartyTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("4\n" +
                "5\n" +
                "6\n" +
                "7\n" +
                "8\n");
        HalloweenParty.main(stdin, new PrintWriter(stdout));
        assertEquals("6\n" +
                "9\n" +
                "12\n" +
                "16\n", stdout.toString());
    }
}
