import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) { 
        value = new AtomicIntegerArray(this.byteArrayToIntArray(v));
        maxval = 127; 
    }

    GetNSetState(byte[] v, byte m) { 
        value = new AtomicIntegerArray(this.byteArrayToIntArray(v)); 
        maxval = m; 
    }

    public static int[] byteArrayToIntArray(byte[] v) {
        int[] tmp = new int[v.length];
        for (int i = 0; i < v.length; i++) {
            tmp[i] = (int) v[i];
        }
        return tmp;
    }

    public static byte[] intArrayToByteArray(AtomicIntegerArray v) {
        byte[] tmp = new byte[v.length()];
        for (int i = 0; i < v.length(); i++) {
            // .get() method instead of bracket notation to access elements
            // http://bit.ly/2hFUAuB
            tmp[i] = (byte) v.get(i);
        }
        return tmp;
    }

    public int size() { 
        // length is a method (instead of a property) on AtomicIntegerArray
        // http://bit.ly/2h0l6eh
        return value.length();
    }

    public byte[] current() { return this.intArrayToByteArray(value); }

    public boolean swap(int i, int j) {
    if (value.get(i) <= 0 || value.get(j) >= maxval) {
	    return false;
	}
    // use nested get/set instead of `incrementAndGet`...?
    // cf. https://piazza.com/class/ij4pi2k5m0d5gn?cid=112
    // and https://piazza.com/class/ij4pi2k5m0d5gn?cid=137
    value.set(i, value.get(i) - 1);
    value.set(j, value.get(j) + 1);
	return true;
    }
}
