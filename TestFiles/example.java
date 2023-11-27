package TestFiles;
public class example {
	public static void main(String[] args) {
		boolean x = true;
		if (x) {
			int A = 22;
			A += 1;
		}
		System.out.println(x);
		System.out.println(add10(x));
	}
	public static Poly add10(Poly x) {
		return x + 10;
	}
}