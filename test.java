public class test {
	public static void main(String[] args) {
		int x = 3;
		x = y = z = 10;
		x = {true};
		x = y = z = false;
		x = true;
		o = 'y';
		x = 3 / 10 - 7 + 5;
		x = true;
		x = 3 <= 4;
		x = 3 > 4;
		x = {1,2,3};
		x = "Test!";
		x = 'Q';
		if (2 == 2) {
			x = "Wrong";
			x = false;
			if (2 == 2) {
				if (4 == 4) {
					x = 1000;
				}
			}
		}
		x = 1;
		if (2 == 2) {
			x = true;
		} else if (3 == 2) {
			x = false;
		} else {
			x = false;
		}
	}
}