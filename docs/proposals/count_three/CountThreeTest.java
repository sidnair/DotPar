import java.util.ArrayList;

public class CountThreeTest extends ListTest {
	
	protected int sequential(ArrayList<Integer> arr) {
		int ct = 0;
		for (int i : arr) {
			if (i == 3) {
				ct++;
			}
		}
		return ct;
	}
	
	protected Thread makeWorker(int start, int end, ArrayList<Integer> arr) {
		return new Thread(new CountThreeWorker(start, end, arr));
	}
	
	protected int getRet() {
		int tmp = CountThreeWorker.sharedCount;
		CountThreeWorker.sharedCount = 0;
		return tmp;
	}

}
