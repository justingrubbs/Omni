package TestFiles;
public class scope {
	public static void main(String[] args) {
		boolean x = false;
		if (x) {
			if (x) {
				x = false;
			}
		}
		System.out.println(x);
		x = true;
		boolean[] A = null;
		if (x) {
			x = false;
			x = true;
			A = new boolean[] {false};
		}
		boolean[][] B = null;
		if (true) {
			B = new boolean[][] {{true,false}};
		}
	}

}