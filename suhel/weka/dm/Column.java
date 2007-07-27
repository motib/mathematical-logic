package weka.dm;

import weka.core.*;

import org.apache.log4j.*;

 

import java.util.*;

public class Column {
  
  static Logger log=Logger.getLogger(Column.class);
  Instances instances=null;
  
  final long columnId;

  private boolean isAtomic=false;
  private boolean tag=false;// indicates that the column is either an atomic or the subcolumns exist;
  /*
   * columnId  = 1001101
   * fColumnId = 100110-
   * sColumnId = -001101 
   */
  private Column fColumn,sColumn;
  
  public Column(Instances instances) {
    super();
    this.instances = instances;
    columnId=0;
  }
  

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

}
