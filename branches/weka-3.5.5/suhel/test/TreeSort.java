package test;
import dm.*;
import org.apache.log4j.*;
import java.util.*;

public class TreeSort {
  static Logger log=Logger.getLogger(TreeSort.class);
  /**
   * @param args  ccvccvcv
   */

  public static void main(String[] args) {
    // TODO Auto-generated method stub
    HashSet<Integer> t1=new HashSet<Integer>( );
    t1.add(3);
    t1.add(1);
    t1.add(5);
    t1.add(4);
    t1.add(2);
    
    HashSet<Integer> t2=new HashSet<Integer>( );
    t2.add(3);
    t2.add(1);
    t2.add(5);

    HashSet<Integer> t3=new HashSet<Integer>(t1);
    HashSet<Integer> t4=new HashSet<Integer>(t2);
    
    t3.retainAll(t2);
    t4.retainAll(t1);
    
    String s="";
    for (Iterator<Integer> iter = t3.iterator(); iter.hasNext();) {
     int name = ((Integer) iter.next()).intValue();
     s+=name+"\t";
      
    }
    log.info("t3 : " +s);
    
    
  }

}
