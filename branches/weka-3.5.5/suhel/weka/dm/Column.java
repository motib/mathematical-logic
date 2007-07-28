package weka.dm;


import weka.core.*;

import org.apache.log4j.*;

import dm.ColumnName;



import java.util.*;

public class Column {

  static Logger log=Logger.getLogger(Column.class);
  
  private Instances instances=null;
  private Map<Long,Column> existingColumns=null;
  final long columnId;
  private Map<Integer, Set<Integer>> items=new HashMap<Integer, Set<Integer>>();

  private boolean isAtomic=false;
  private boolean containsSubColoumns=false;// indicates that the column is either an atomic or the subcolumns exist;
  private boolean isChanged=false;
  /*
   * columnId  = 1001101
   * fColumnId = 100110-
   * sColumnId = -001101 
   */
  private Column fColumn,sColumn;

  private int oSupport;
  private double confidence;

  public Column(Instances instances,long columnId,Map<Long,Column> existingColumns) {
    super();
    this.instances = instances;
    this.columnId=columnId;
    this.existingColumns=existingColumns;

    if (ColumnName.length(columnId)==1){
      this.isAtomic=true;
      //TODO check containsSubColoumns=true 
      return;
    }
    long sub1=ColumnName.firstSubColumn(columnId);
    long sub2=ColumnName.secondSubColumn(columnId);
    if(   ! existingColumns.keySet().contains(sub1)
	|| ! existingColumns.keySet().contains(sub2))
      return;//w
    fColumn= existingColumns.get(sub1);
    sColumn= existingColumns.get(sub2);
    this.containsSubColoumns = true;

  }

  //public void setSupport
  protected boolean generateAtomicOccurances(){
    if(! isAtomic){
      log.equals("Coulmn is not Atomic");
      return false;
    }
    int col=ColumnName.atomicOrgColName(columnId);
    items=new HashMap<Integer,Set<Integer>>();
    Map<Double, Integer> tmap=new HashMap<Double, Integer>();
    for (int line=0; line< instances.numInstances(); line++){
      double value=instances.instance(line).value(col);
      if( tmap.containsKey(value)){
	int frstLine=tmap.get(value);
	boolean isAdded= items.get(frstLine).add(line);
	if(! isAdded) log.warn("Line "+line+" not added to the set");
      }else{
	Set<Integer> lines=new HashSet<Integer>();
	lines.add(line);
	items.put(line, lines);
      }
    }
    return true;
  }

  protected boolean generateOccurances(){
    items=new HashMap<Integer, Set<Integer>>();
    Set<int[]> possibleEntity = Tool.decart(fColumn.items.keySet(),sColumn.items.keySet());
    if(possibleEntity.size()==0)return false;
    for (int[] is : possibleEntity) {
      TreeSet<Integer> set=generateItemOccurances( is[0] , is[1] );
      if (set.size() > 0){
	int frstLine=set.first();
	items.put(frstLine, set);
      }
    }
    return (this.items.size()>0)?true:false;
  }

  protected TreeSet<Integer> generateItemOccurances(int fItem, int sItem){
    TreeSet<Integer> s1= new TreeSet<Integer>(fColumn.items.get(fItem));
    Set<Integer> s2= sColumn.items.get(sItem);
    s1 .retainAll(s2);
    return s1;
  }

  protected boolean generateAtomic(){
    return false;
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

  

 

}
