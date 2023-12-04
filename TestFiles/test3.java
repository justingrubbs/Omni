package TestFiles;
public class test3 {
	public static void main(String[] args) {
		
		System.out.println(coolMath());;
	}
	public static int add(int x, int y) {
		return x + y;
	}
	public static int mul(int x, int y) {
		return x * y;
	}
	public static int coolMath() {
		boolean x = true;
		int A = 0;
		int y = 200;
		int z = 10;
		boolean B = true;
		z = 0;
		if (true) {
			A = A + 1;
			y = y + 1;
		}
		z = add(A,y);
		A = mul(z,z);
		return A;
	}
}