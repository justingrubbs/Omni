package TestFiles;
public class scope {
	public static void main(String[] args) {
		boolean x = false;
		String A = "";
		if (x) {
			A = "";
			if (x) {
				x = false;
				A = "squirrel";
			}
		}
		System.out.println(x);
		x = true;
		boolean[] B = null;
		if (x) {
			x = false;
			x = true;
			B = new boolean[] {false};
		}
		int D = 0;
		boolean[][] C = null;
		if (true) {
			C = new boolean[][] {{false,true}};
			D = 12;
		}
	}
}