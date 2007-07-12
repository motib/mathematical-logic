package dm;
import java.util.*;
/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class classColumn {

  private DataMine dm;
          Map items;

  public classColumn(DataMine datMin) {
    dm=datMin;
    items=new HashMap();
    // scan the class column and get the distinct values and the lines which accompany each distinct value
    Iterator iter2=dm.allLines.iterator();
    while(iter2.hasNext()){
      Integer ttt=(Integer)iter2.next();

      String s=(String)((String[])dm.entity.get(ttt))[0];
      if(items.keySet().contains(s)){
        ((Set)items.get(s)).add(ttt);
      }else{
        Set ss= new TreeSet();
        ss.add(ttt);
        items.put(s,ss);
      }
    }
//      System.out.println("\n" + items.entrySet());
  }
}