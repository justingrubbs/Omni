package TestFiles;
public class func {
	public static void main(String[] args) {
		int a = add(3,1);
		System.out.println(add(3,2));
		System.out.println(a);
	}

	public static int add(int x, int y) {
		int z = x * y;
		boolean A = true;
		z = x * y;
		if (true) {
			return z + x + y;
		} else {
			return x + y;
		}
	}

}