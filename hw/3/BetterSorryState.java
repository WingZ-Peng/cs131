/*
import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.locks.ReentrantLock;
*/

class BetterSorryState implements State {
    private byte[] value;
    private byte maxval;
    // private List<ReentrantLock> locks;

    BetterSorryState(byte[] v) { 
      // this.createLocks(v.length);
      value = v; 
      maxval = 127; 
    }

    BetterSorryState(byte[] v, byte m) {
      // this.createLocks(v.length);
      value = v; 
      maxval = m; 
    }

/*
    public void createLocks(int numLocks) {
      locks = new ArrayList<ReentrantLock>();
      for (int i = 0; i < numLocks; i++) {
        locks.add(new ReentrantLock());
      }
    }
*/

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
      if (value[i] <= 0 || value[j] >= maxval) {
          return false;
      }
      /*
      synchronized (this) {
        locks.get(i).lock(); locks.get(j).lock();
      }
      */
      synchronized (this) { value[i]--; }
      synchronized (this) { value[j]++; }
      /*
      synchronized (this) {
        locks.get(i).unlock(); locks.get(j).unlock();
      }
      */
      return true;
    }
}

/*
    public static AtomicInteger[] populateIntArray(byte[] v) {
      AtomicInteger[] tmp = new AtomicInteger[v.length];
      for (int i = 0; i < v.length; i++) {
        tmp[i] = new AtomicInteger(v[i]);
      }
      return tmp;
    }

    public static int[] byteArrayToIntArray(byte[] v) {
        int[] tmp = new int[v.length];
        for (int i = 0; i < v.length; i++) {
            tmp[i] = (int) v[i];
        }
        return tmp;
    }

    public static byte[] intArrayToByteArray(AtomicInteger[] v) {
        byte[] tmp = new byte[v.length];
        for (int i = 0; i < v.length; i++) {
            // convert AtomicInteger -> Int -> byte
            // cf. http://bit.ly/2haG3In
            tmp[i] = (byte) v[i].intValue();
        }
        return tmp;
    }


    public int size() { 
        return value.length;
    }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
    if (value[i] <= 0 || value[j] >= maxval) {
	    return false;
	  }
    // use getAndIncrement/Decrement to atomically perform a read and write
    // cf. http://bit.ly/2hFRF55
    value[i] = (byte) new AtomicInteger(value[i]).getAndDecrement();
    value[j] = (byte) new AtomicInteger(value[j]).getAndIncrement();
	return true;
    }
}

*/