class BetterSorryState implements State {
    private byte[] value;
    private byte maxval;

    BetterSorryState(byte[] v) { 
      value = v; 
      maxval = 127; 
    }

    BetterSorryState(byte[] v, byte m) {
      value = v; 
      maxval = m; 
    }

    public int size() { return value.length; }

    public byte[] current() { return value; }

    public boolean swap(int i, int j) {
      if (value[i] <= 0 || value[j] >= maxval) {
          return false;
      }
      synchronized (this) { value[i]--; }
      synchronized (this) { value[j]++; }
      return true;
    }
}