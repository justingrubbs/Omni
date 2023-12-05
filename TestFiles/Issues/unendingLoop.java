package TestFiles.Issues;
public class unendingLoop {
	public static void main(String[] args) {
		boolean x = true;
		int[] A = null;
		while (x) {
			A = new int[] {0};
		}
		System.out.println(A);
	}

}