package sbpp.money;

import java.util.Arrays;

import junit.framework.TestCase;

public class MoneyTest extends TestCase {
	public void testDebugPrintMethod() {
		assertEquals("5 USD", new Money(5, "USD").toString());
	}
	
	public void testAdd() {
		Money m1 = new Money(5, "USD");
		Money m2 = new Money(7, "USD");
		assertEquals("12 USD", m1.add(m2).toString());
	}
	
	public void testMoneySumDebugPrintMethod() {
		MoneySum sum = new MoneySum(Arrays.asList(new Money(1, "USD"), new Money(2, "GBP")));
		assertEquals("1 USD + 2 GBP", sum.toString());
	}
}
