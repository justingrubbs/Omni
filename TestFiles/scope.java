package TestFiles;
public class scope {
	public static void main(String[] args) {
		boolean x = true;
		String[][] A;
		if (x) {
			if (x) {
				x = false;
				A = new String[][] {{"SQUIRREL?"}};
			}
		}
		x = true;
		boolean[] A;
		if (x) {
			x = false;
			x = true;
			A = new boolean[] {false};
		}
		int C;
		boolean[][] B;
		if (true) {
			B = new boolean[][] {{false,true}};
			C = 12;
		}
	}
}