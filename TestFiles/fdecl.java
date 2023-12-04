package TestFiles;
public class figure_1 {
	public static void main(String[] args) {
		int x = add(3,2);
		int y = mul(x,10);
		boolean z = true;
		while (z) {
			x += 1;
			z = false;
		}
		String B = "Squirrel";
	}
	public static int add(int x, int y) {
		return x + y;
	}
	public static int mul(int x, int y) {
		return x * y;
	}
}