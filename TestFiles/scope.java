package TestFiles;
public class scope {
	public static void main(String[] args) {
		boolean x = true;
		int[] A;
		if (true) {
			A = new int[] {3};
		}
		int[][] A;
		if (true) {
			A = new int[][] {{3}};
		}
		int[][][] B;
		if (true) {
			B = new int[][][] {{{3}}};
		}
		int[][][][] C;
		if (true) {
			C = new int[][][][] {{{{3}}}};
		}
		boolean[] D;
		if (true) {
			D = new boolean[] {false};
		}
		String E = "squirrel";
	}
}