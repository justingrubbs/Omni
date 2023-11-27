package TestFiles;
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
		System.out.println(a); // Honestly should be `C`
		a = true;
		if (a == true) {
			boolean[][] D = {{true,false}};
		}
		System.out.println(a);
		if (true) {
			int c = 30;
		}
		System.out.println(c);
		if (1) {
         // Ghost `x`??? -- `func.java` helps explain it but this is still weird
			System.out.println("Python has values - `if 1:` == `if True:`"); 
		}
	}
}