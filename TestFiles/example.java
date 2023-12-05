package TestFiles;
public class example {
	public static void main(String[] args) {
		boolean x = true;
		int[] y = null;
		if (x) {
			y = new int[] {30000};
		}
		int[] A = {0};
		int B = 3;
		String C = "Squirrel";
		System.out.println(C);
		B = 14;
		System.out.println(add10(B));
	}

	public static int add10(int x) {
		return x + 10;
	}

}