package TestFiles;
public class quirks {
	public static void main(String[] args) {
		int x = 2 * 3 - 2;
		x = 10;
		int y,z = y = 10;
		boolean[] A = {true};
		A = new boolean[] {false};
		String B = "String";
		B = "c";
		boolean a = true;
		boolean[][] C = null;
		while (a) {
			C = new boolean[][] {{true}};
		}
		System.out.println(a);
		a = true;
		if (a == true) {
			C = new boolean[][] {{true,false}};
		}
		System.out.println(a);
		int c = 0;
		if (true) {
			c = 30;
		}
		System.out.println(c);
		if (1) {
			System.out.println("Python has values - `if 1:` == `if True:`");
		}
		Poly[] D = {};
	}
}