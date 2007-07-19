package dm;

public class RuleOrder implements Comparable<RuleOrder> {

  double[] com=null;
  static int[] order={1,1,1,1,1,1,1,1,1,1};
  
  
  public static void setOrder(int[] pOrder){
    order=pOrder;
  }
  public RuleOrder(double[] com) {
    // TODO Auto-generated constructor stub
    this.com=com;
  }

  public int compareTo(RuleOrder o) {
    // TODO Auto-generated method stub
    for (int i=0; i< this.com.length; i++){
      if(this.com[i] != o.com[i]) return com[i]<o.com[i]?order[i]:-order[i];
    }
    return 0;
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    

  }

}
