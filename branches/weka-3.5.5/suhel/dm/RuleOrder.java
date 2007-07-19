package dm;

import java.util.Iterator;
import java.util.TreeSet;
import org.apache.log4j.*;
import dm.RuleOrder;

public class RuleOrder implements Comparable<RuleOrder> {
  static Logger log=Logger.getLogger(RuleOrder.class);

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
    // TODO Auto-generated method stub
    RuleOrder.setOrder(new int[]{1,1,-1});
    RuleOrder r1=new RuleOrder(new double [] {2,1,1});
    RuleOrder r2=new RuleOrder(new double [] {3,1,1});
    RuleOrder r3=new RuleOrder(new double [] {3,2,1});
    RuleOrder r4=new RuleOrder(new double [] {1,2,2});
    RuleOrder r5=new RuleOrder(new double [] {1,3,1});
    
    TreeSet set=new TreeSet<RuleOrder>();
    set.add(r5);
    set.add(r3);
    set.add(r4);
    set.add(r1);
    set.add(r2);
    
    for (Iterator iter = set.iterator(); iter.hasNext();) {
      RuleOrder rl = (RuleOrder) iter.next();
      double[] com=rl.com;
      log.info(com[0]+"\t"+com[1]+"\t"+com[2]);
      
      
    }

  }

}
