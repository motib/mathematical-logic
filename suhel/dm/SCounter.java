package dm;
import java.util.*;

public class SCounter {
  TreeMap values, nextValues;
  private int hitCounter=0;
  private String name="";

  public SCounter() {
    values = new TreeMap();
  }
  public SCounter(String s){
    this.name=s;
    values = new TreeMap();
  }

  public SCounter(TreeMap nextValues) {
    values = new TreeMap();
    this.nextValues = nextValues;
  }

  int addValue(long lv) {
    int result = 0;
    hitCounter++;
    Integer freq = (Integer) values.get(new Long(lv));
    if (freq == null) {
      values.put(new Long(lv), new Integer(1));
      result = 1;
    }
    else {
      values.put(new Long(lv), new Integer(freq.intValue() + 1));
      result = freq.intValue() + 1;
    }
    return result;
  }

  int addValue(double v, int digits) {
    int result = 0;
    long lv = toLong(v,digits);
    return addValue(lv);
  }

  int getValueFreq(double d, int digits) {
    int result;
    long ld = toLong(d,digits);
    Integer freq = (Integer) values.get(new Long(ld));
    if (freq == null) result = 0;
    else result = freq.intValue();
    return result;
  }

  public static void main(String[] args) {
    SCounter SCounter1 = new SCounter();
  }

  private long toLong(double d,int digits) {
    return Math.round(Math.pow(10,digits) * d);
  }

  public String toString() {
    String result = "";
    Iterator iter = values.entrySet().iterator();
    while (iter.hasNext()) {
      Map.Entry me = (Map.Entry) iter.next();
      String key = ( (Long) me.getKey()).toString();
      String val = ( (Integer) me.getValue()).toString();
      result += key + "\t\t" + val + "\n";
    }

    return result;
  }

  int getSize() {
    return values.size();
  }

  public int getHitCounter() {
    return hitCounter;
  }
  public int getEqualsCounter(){
    return this.getHitCounter()-this.getSize();
  }

  void setHitCounter(int val) {
    hitCounter = val;
  }
  void reset(){
    setHitCounter(0);
    values.clear();
  }

}
