package weka.dm;

import weka.core.*;

import org.apache.log4j.*;

import dm.ColumnName;
import dm.Sccl;

 

import java.util.*;

public class Column {
  
  static Logger log=Logger.getLogger(Column.class);
  private Instances instances=null;
  private Map<Long,Column> existingColumns=null;
  final long columnId;

  private boolean isAtomic=false;
  private boolean tag=false;// indicates that the column is either an atomic or the subcolumns exist;
  /*
   * columnId  = 1001101
   * fColumnId = 100110-
   * sColumnId = -001101 
   */
  private Column fColumn,sColumn;
  
  private Map<Double, Set<Double>> items=new HashMap<Double, Set<Double>>();
  
  
  public Column(Instances instances,long columnId,Map existingColumns) {
    super();
    this.instances = instances;
    this.columnId=columnId;
    
  }
  
  private void generateOccurances(){
    items=new TreeMap<Integer,Sccl>();
    Map<String,Integer> tempSet= new HashMap<String,Integer>();
    // scan the entity and get the distinct values and the lines which accompany each distinct value
    Iterator<Integer> iter2=dm.allLines.iterator();
    while(iter2.hasNext()){
      Integer line=iter2.next();
      String s=dm.entity.get(line)[ColumnName.atomicOrgColName(columnId)];
      if(tempSet.keySet().contains(s)){
        Integer fi= tempSet.get(s);
          items.get(fi).lines.add(line);
      }else{
	tempSet.put(s,line);
        Sccl ss= new Sccl();
        ss.lines.add(line);
        items.put(line,ss);
      }
    }
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

}
