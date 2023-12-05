package TestFiles;
public class test4 {
	public static void main(String[] args) {
		boolean x = true;
		int B = fac(10);
		System.out.println(B);
	}
	public static int fac(int x) {
		return facHelp(x,x);
	}
	public static int facHelp(int x, int y) {
		if (x == 1) {
			return y;
		} else {
			return facHelp(x - 1,y * (x - 1));  // Recursion causes elaboration to never end, solution in mind
		}
	}
}