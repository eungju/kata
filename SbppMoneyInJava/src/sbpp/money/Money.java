package sbpp.money;

public class Money {
	private int amount;
	private String currency;
	
	public Money(int amount, String currency) {
		this.amount = amount;
		this.currency = currency;
	}
	
	public String toString() {
		return amount + " " + currency;
	}
	
	public Money add(Money aMoney) {
		return new Money(amount + aMoney.amount, currency);
	}
}
