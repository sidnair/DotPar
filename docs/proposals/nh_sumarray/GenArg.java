package sumarg;

import java.util.ArrayList;
import java.util.Random;

public class GenArg implements Runnable {
	ArrayList<Integer> arr;
	int lrange;
	int urange;
	
	public GenArg(ArrayList<Integer> arr, int lrange, int urange) {
		this.arr = arr;
		this.lrange = lrange;
		this.urange = urange;
	}
	
	@Override
	public void run() {
		// use ThreadLocalRandom?
		Random r = new Random();
		for(int i=lrange; i<urange; i++) {
			this.arr.set(i, r.nextInt(1000));
		}
	}
}
