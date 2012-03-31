package sumarg;

import java.util.ArrayList;
import java.util.Random;

public class SumArg_main {

	/**
	 * @param args
	 * @throws InterruptedException 
	 */
	public static void main(String[] args) throws InterruptedException {
		// fill the array with random things
		ArrayList<Integer> nums = new ArrayList<Integer>(1000);
		ArrayList<Thread> gens = new ArrayList<Thread>();
		for(int i=0; i<1000; i++) {
			nums.add(0);
		}
		for(int i=0; i<5; i++) {
			Thread t = new Thread(new GenArg(nums, i*200, (i+1)*200));
			gens.add(t);
			t.start();
		}
		for(int i=0; i<5; i++) {
			gens.get(i).join();
		}
		
		// sum the array
		// it might be a better idea to use a executor service here
		// especially if we can get batches
		ArrayList<Thread> sums = new ArrayList<Thread>();
		ArrayList<Integer> results = new ArrayList<Integer>(5);
		for(int i=0; i<5; i++) {
			results.add(0);
		}
		for(int i=0; i<5; i++) {
			Thread t = new Thread(new SumArg(nums, results, i, i*200, (i+1)*200)); 
			sums.add(t);
			t.start();
		}
		for(Thread t : sums)
			t.join();
		int total = 0;
		for(int i=0; i<5; i++) {
			total = total + results.get(i);
		}
		System.out.println(total);
	}
}
