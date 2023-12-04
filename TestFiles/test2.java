package TestFiles;
public class test2 {
	public static void main(String[] args) {
		int x = add(6,3);
		int y = add(2,4);
		int z = mul(x,y);
		System.out.println(y);
		System.out.println(z);
		if (x == y) {
			System.out.println(x);
		}
		if (y == z) {
			System.out.println(y);
		} else {
			System.out.println(z);
		}
	}

	public static int add(int x, int y) {
		z = true;
		return x + y;
	}

	public static int mul(int x, int y) {
		x = 3;
		return x * y;
	}

}