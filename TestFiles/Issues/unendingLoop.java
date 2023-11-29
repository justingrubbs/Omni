package TestFiles.Issues;
public class unendingLoop {
	public static void main(String[] args) {
		boolean x = true;
		int A = 0;
		while (x) {
			A = 0;
		}
		System.out.println(x);
	}
}