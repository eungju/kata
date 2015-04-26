import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import static org.junit.Assert.assertEquals;

public class CavityMapTest {
    @Test
    public void
    main() {
        StringWriter stdout = new StringWriter();
        StringReader stdin = new StringReader("4\n" +
                "1112\n" +
                "1912\n" +
                "1892\n" +
                "1234\n");
        CavityMap.main(stdin, new PrintWriter(stdout));
        assertEquals("1112\n" +
                "1X12\n" +
                "18X2\n" +
                "1234\n", stdout.toString());
    }
}
