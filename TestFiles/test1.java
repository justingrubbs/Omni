package TestFiles;
public class test1 {
	public static void main(String[] args) {
		int x = 3;
		x = 10;
		int y,z = y = 10;
		boolean[] H = {true};
		H = new boolean[] {false,true};
		boolean I,J,K = J = I = false;
		I = true;
		String A = "y";
		x = 3 / 10 - 7 + 5;
		I = true;
		I = 3 <= 4;
		I = 3 > 4;
		int[] L = {3,2,1};
		String M = "Test!";
		M = "Q";
		if (2 == 2) {
			if (2 == 2) {
				if (4 == 4) {
					x = 1000;
				}
			}
			I = false;
			M = "Wrong";
		}
		if (2 == 2) {
			I = false;
			I = true;
		} else {
			if (3 == 2) {
				I = true;
				I = false;
			} else {
				if (2 == 100) {
					I = true;
					I = false;
				} else {
					I = true;
					I = false;
				}
			}
		}
	}

}