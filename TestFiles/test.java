package TestFiles;
public class test {
	public static void main(String[] args) {
		int x = 3;
		x = 10;
		int y,z = y = 10;
		boolean[] A = {true};
		A = new boolean[] {true,false};
		boolean B,C,D = C = B = false;
		B = true;
		String E = "y";
		x = 3 / 10 - 7 + 5;
		B = true;
		B = 3 <= 4;
		B = 3 > 4;
		int[] F = {1,2,3};
		String G = "Test!";
		G = "Q";
		if (2 == 2) {
			if (2 == 2) {
				if (4 == 4) {
					x = 1000;
				}
			}
			B = false;
			G = "Wrong";
		}
		if (2 == 2) {
			B = false;
			B = true;
		} else {
			if (3 == 2) {
				B = true;
				B = false;
			} else {
				if (2 == 100) {
					B = true;
					B = false;
				} else {
					B = true;
					B = false;
				}
			}
		}
	}
}