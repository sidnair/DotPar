import java.util.ArrayList;

public class CountThreeWorker implements Runnable {
	
	private final int start;
	private final int end;
	private ArrayList<Integer> arr;
	public static int sharedCount;
    private static final Object countLock = new Object();
	
	public CountThreeWorker(int start, int end, ArrayList<Integer> arr) {
		this.start = start;
		this.end = end;
		this.arr = arr;
	}

	@Override
	public void run() {
		int count = 0;
		for (int i = start; i < end; i++) {
			if (arr.get(i) == 3) {
				count++;
			}
		}
		synchronized (countLock) {
			sharedCount += count;
		}
	}

}
