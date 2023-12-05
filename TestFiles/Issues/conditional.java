package TestFiles.Issues;
public class conditional {
	public static void main(String[] args) {
		boolean x = false;
		String[] A = null;
		if (x) {
			A = new String[] {"Squirrel"};
		}
		System.out.println(A);
		int[] y = null;
		if (true) {
			y = new int[] {3};
		}
		System.out.println(y);
		float[] B = null;
		boolean[] z = null;
		if (true) {
			z = new boolean[] {true};
		} else {
			B = new float[] {4.2f,1.3f};
		}
		System.out.println(B);
	}

}