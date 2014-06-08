package razie.actionables;

/**
 * result is false 69978 vs 100000
 * 
 * @author razvanc
 * 
 */
public class TestVol {
	volatile Long i = 0L;

	static final int CYCLES = 10;
	static final int THREADS = 200;
	static final int TESTS = 20;

	public void doit() {
		for (int j = 0; j < CYCLES; j++)
			i++;
	}

	public static void main(String[] argv) throws InterruptedException {
		Thread[] threads2 = new Thread[THREADS];
		for (int tests = 0; tests < TESTS; tests++) {
			final TestVol ii = new TestVol();

			for (int t = 0; t < THREADS; t++)
				threads2[t] = new Thread() {
					public void run() {
						ii.doit();
					}
				};

			for (int t = 0; t < THREADS; t++)
				threads2[t].start();

			for (int t = 0; t < THREADS; t++)
				threads2[t].join();

			System.out.println(""
					+ (ii.i == CYCLES * THREADS * 1L ? "true"
							: "              false") + " " + ii.i + " vs "
					+ CYCLES * THREADS * 1L);
		}
	}
}
