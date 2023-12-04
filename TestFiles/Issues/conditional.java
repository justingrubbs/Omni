package TestFiles.Issues;
public class conditional {
	public static void main(String[] args) {
		boolean x = false;
		String[] C = null;
		if (x) {
			C = new String[] {"Squirrel"};
		}
		System.out.println(C);
		int[] y = null;
		if (true) {
			y = new int[] {3};
		}
		System.out.println(y);
		float[] D = null;
		boolean[] z = null;
		if (true) {
			z = new boolean[] {true};
		} else {
			D = new float[] {4.2f,1.3f};
		}
		System.out.println(D);
	}

}