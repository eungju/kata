import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class CutTheSticksTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("6\n" +
                "5 4 4 2 2 8\n");
        CutTheSticks.main(stdin, new PrintWriter(stdout));
        assertEquals("6\n" +
                "4\n" +
                "2\n" +
                "1\n", stdout.toString());
    }
}
