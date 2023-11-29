package TestFiles;
public class elab {
	int y = 3;
	boolean A = false;
	System.out.println(A);
	public static void main(String[] args) {
		int y = 3;
		boolean B = false;
		System.out.println(B); // if the above were global declarations, wouldn't it be correct for these variables to not need declarations?
	}
	public static int test() {
		int x = 3;
		if (x == 3) {
			return x + 1;
		}
		return x;
	}
}