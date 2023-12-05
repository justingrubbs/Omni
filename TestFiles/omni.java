package TestFiles;
public class omni {
	public static void main(String[] args) {
		System.out.println(coolMath());
	}

	public static int add(int x, int y) {
		return x + y;
	}

	public static int mul(int x, int y) {
		return x * y;
	}

	public static int coolMath() {
		int x = 0;
		int y = 200;
		int z = 10;
		boolean A = true;
		z = 0;
		if (true) {
			x = x + 1;
			y = y + 1;
		}
		z = add(x,y);
		int a = mul(z,z);
		return a;
	}

}