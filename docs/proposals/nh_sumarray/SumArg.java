package sumarg;

import java.util.ArrayList;

public class SumArg implements Runnable {
	ArrayList<Integer> arr;
	ArrayList<Integer> res;
	int lrange;
	int urange;
	int ind;
	
	public SumArg(ArrayList<Integer> arr, ArrayList<Integer> res, int ind, int lrange, int urange) {
		this.arr = arr;
		this.res = res;
		this.ind = ind;
		this.lrange = lrange;
		this.urange = urange;
	}
	
	public void run() {
		int acc = 0;
		for(int i=lrange; i<urange; i++) {
			acc = acc + arr.get(i);
		}
		res.set(ind, acc);
	}
}
