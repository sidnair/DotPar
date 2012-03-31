import java.util.ArrayList;

public abstract class ListTest {

	private final static int MILLION = 1000 * 1000;
	private final static int SIZE = 10 * MILLION;
	private final static int BOUND = 5;
	private final static int MAX_NUM_THREADS = 16;
	
	public void run() throws InterruptedException {
		ArrayList<Integer> ints = generateList(SIZE, BOUND);
		for (int i = 0; i <= MAX_NUM_THREADS; i++) {
			timedRun(i, ints);
		}
	}
	
	public void timedRun(int threads, ArrayList<Integer> ints) throws InterruptedException {
		long start = System.currentTimeMillis();
		int ct = -1;
		if (threads > 0) {
			ct = parallel(ints, threads);
		} else {
			ct = sequential(ints);
		}
		long end = System.currentTimeMillis();
		System.out.println("Threads: " + threads + "\t" +
				"Count: " + ct + "\t" +
				"Time: " + (end - start));
	}
	
	public ArrayList<Integer> generateList(int size, int bound) {
		ArrayList<Integer> ints = new ArrayList<Integer>(size);
		for (int i = 0; i < size; i++) {
			// [0, bound]
			ints.add((int) (Math.random() * (bound + 1)));
		}
		return ints;
	}
	
	protected int parallel(ArrayList<Integer> arr, int numThreads) throws InterruptedException {
		ArrayList<Thread> threads = new ArrayList<Thread>();
		int interval = arr.size() / numThreads;
		int start = 0;
		int end = interval;
		for (int i = 0; i < numThreads; i++) {
			if (i == numThreads - 1) {
				end = arr.size();
			}
			threads.add(makeWorker(start, end, arr));
			start = end;
			end += interval;
		}
		for (Thread t : threads) {
			t.run();
		}
		for (Thread t : threads) {
			t.join();
		}
		return getRet();
	}
	
	protected abstract Thread makeWorker(int start, int end, ArrayList<Integer> arr);

	protected abstract int getRet();

	protected abstract int sequential(ArrayList<Integer> ints);

}
