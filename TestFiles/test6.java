package TestFiles;
public class test6 {
	public static void main(String[] args) {
		run_game(82,2);
		System.out.println("Thank you for playing! ");
	}

	public static void run_game(int x, int y) {
		boolean num = false;
		String C = "String";
		int[] D = {3};
		int E = x;
		boolean done = false;
		int num_guess = 1;
		int guess = y;
		while (!done) {
			guess += 1;
			if (E == guess) {
				done = true;
				System.out.println("Correct! Number of attempts:");
				System.out.println(num_guess);
			} else {
				num_guess = num_guess + 1;
				if (guess < E) {
					System.out.println("Too low");
				} else {
					System.out.println("Too high");
				}
			}
		}
		test();
		return;
	}

	public static void test() {
		int a = 3;
		boolean A = true;
		String B = "c";
		return;
	}

}