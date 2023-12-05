package TestFiles;
public class test1 {
	public static void main(String[] args) {
		int x = 3;
		x = 10;
		int y,z = y = 10;
		float A = 3.1f;
		boolean[] B = {true};
		B = new boolean[] {false,true};
		boolean C,D,E = D = C = false;
		C = true;
		String F = "y";
		System.out.println(F);
		x = 3 / 10 - 7 + 5;
		C = true;
		C = 3 <= 4;
		C = 3 > 4;
		int[] G = {3,2,1};
		String H = "Test!";
		System.out.println(H);
		H = "Q";
		if (2 == 2) {
			if (2 == 2) {
				if (4 == 4) {
					x = 1000;
				}
			}
			C = false;
			H = "Wrong";
		}
		System.out.println(H);
		if (2 == 2) {
			C = false;
			System.out.println(H);  // Wrong
			C = true;
		}
		if (3 == 2) {
			C = true;
			C = false;
		} else {
			if (2 == 100) {
				C = true;
				C = false;
			} else {
				C = true;
				C = false;
			}
		}
		System.out.println(C);
	}

}