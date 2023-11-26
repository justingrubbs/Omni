public class quirks {
	public static void main(String[] args) {
		int x = 2 * 3 - 2;
		x = 10;
		int y,z = y = 10;
		boolean[] A = {true};
		A = {false};
		String B = "String";
		B = 'c';
		boolean a = true;
		while (a == true) {
			boolean[][] C = {{true}};
		}
		System.out.println(C);
		a = true;
		if (a == true) {
			C = {{true,false}};
		}
		System.out.println(C);
		if (true) {
			int c = 30;
		}
		System.out.println(c);
	}
}