package TestFiles;
public class example {
	public static void main(String[] args) {
		boolean x = true;
		int A = 0;
		if (x) {
			A = 22;
			A += 1;
		}
		int[] B = null;
		int y = 0;
		if (true) {
			y = 30;
			B = new int[] {30000};
		}
		int[] C = {0};
		int D = 3;
		String E = "Squirrel";
		System.out.println(E);
		D = 14;
		System.out.println(add10(D));
	}
	public static Poly add10(Poly x) {
		return x + 10;
	}
}