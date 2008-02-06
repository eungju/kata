package sbpp.money;

import java.util.Collection;

public class MoneySum {
	private Collection<Money> monies;
	
	public MoneySum(Collection<Money> monies) {
		this.monies = monies;
	}
	
	public String toString() {
		StringBuffer buf = new StringBuffer();
		for (Money each : monies) {
			buf.append(each.toString());
			buf.append(" + ");
		}
		buf.delete(buf.length() - 3, buf.length());
		return buf.toString();
	}
}
