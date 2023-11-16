public class test {
	public static void main(String[] args) {
		int x = 3;
		x = 10;
		int y,z = y = 10;
		boolean[] A = {true};
		boolean B,C,D = C = B = false;
		B = true;
		String o = 'y';
		x = 3 / 10 - 7 + 5;
		B = true;
		B = 3 <= 4;
		B = 3 > 4;
		int[] E = {1,2,3};
		String F = "Test!";
		F = 'Q';
		if (2 == 2) {
			F = "Wrong";
			B = false;
			if (2 == 2) {
				if (4 == 4) {
					x = 1000;
				}
			}
		}
		x = 1;
		if (2 == 2) {
			B = true;
		} else if (3 == 2) {
			B = false;
		} else {
			B = false;
		}
	}
}