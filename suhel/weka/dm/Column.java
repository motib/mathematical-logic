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
  private Set<Integer> ruleSet=null;
  private Column fColumn,sColumn;

  private int oSupport=-1;


  public Column(Instances instances,long columnId,Map<Long,Column> existingColumns) {
    super();
    this.instances = instances;
    this.columnId=columnId;
    this.existingColumns=existingColumns;

  }

  private boolean setSubColumns(){
    if (isAtomic()) return false;
    long sub1=ColumnName.firstSubColumn(columnId);
    long sub2=ColumnName.secondSubColumn(columnId);
    if(   ! existingColumns.keySet().contains(sub1)
	|| ! existingColumns.keySet().contains(sub2))
      return false;//w
    fColumn= existingColumns.get(sub1);
    sColumn= existingColumns.get(sub2);
    return true;
  }


  //public void setSupport
  protected Map<Integer, Set<Integer>> generateAtomicOccurances(){
    Map<Integer, Set<Integer>> result=null;
    if(! isAtomic()){
      log.equals("Coulmn is not Atomic");
      return null;
    }
    int col=ColumnName.atomicOrgColName(columnId);
    result=new HashMap<Integer,Set<Integer>>();
    Map<Double, Integer> tmap=new HashMap<Double, Integer>();
    for (int line=0; line< instances.numInstances(); line++){
      double value=instances.instance(line).value(col);
      if( tmap.containsKey(value)){
	int frstLine=tmap.get(value);
	boolean isAdded= result.get(frstLine).add(line);
	if(!isAdded ) log.warn("Line "+line+" not added to the set");
      }else{
	Set<Integer> lines=new HashSet<Integer>();
	lines.add(line);
	result.put(line, lines);
      }
    }
    return result;
  }

  protected Map<Integer, Set<Integer>> generateOccurances(){
    if(!setSubColumns()){
      log.equals("coloumn does not exist");
      return null;
    }
    Map<Integer, Set<Integer>> result=new HashMap<Integer, Set<Integer>>();
    Set<int[]> possibleEntity = Tool.decart(fColumn.items.keySet(),sColumn.items.keySet());
    if(possibleEntity.size()==0)return null;
    for (int[] is : possibleEntity) {
      TreeSet<Integer> set=generateItemOccurances( is[0] , is[1] );
      if (set.size() > 0){
	int frstLine=set.first();
	result.put(frstLine, set);
      }
    }
    return result;
  }

  protected TreeSet<Integer> generateItemOccurances(int fItem, int sItem){
    TreeSet<Integer> s1= new TreeSet<Integer>(fColumn.items.get(fItem));
    Set<Integer> s2= sColumn.items.get(sItem);
    s1 .retainAll(s2);
    return s1;
  }

  /**
   * 
   * @param line
   * @param set
   * @return true if passed the support condition;
   */
  

  protected boolean isAtomic(){
    return ColumnName.length(columnId)==1 ?true:false;
  }
  protected boolean containsSubColoumns(){
    long sub1=ColumnName.firstSubColumn(columnId);
    long sub2=ColumnName.secondSubColumn(columnId);
    if(   ! existingColumns.keySet().contains(sub1)
	|| ! existingColumns.keySet().contains(sub2))
      return false;
    else
      return true;//w

  }
  protected boolean generate (){
    if(oSupport< 0  ){
      log.fatal("oSupport and confidence are not set yet");
      return false;
    }
    if( !isAtomic() && !setSubColumns()){
      return false;
    }

    items=new HashMap<Integer, Set<Integer>>();
    Map<Integer, Set<Integer>> map=null;
    if(isAtomic()) 
      map= generateAtomicOccurances();
    else 
      map=generateOccurances();
    
    for (Map.Entry<Integer, Set<Integer>> iter : map.entrySet()) {
      Set<Integer> s=iter.getValue();
      if(s.size() >= oSupport)	items.put(iter.getKey(), s);
    }
    
    if(items.size()>0)return true;
    else return false;
  }
  
  /**
   * affect items, ruleSet call checkPossibleRule
   * @return
   */
  

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

  public int getOSupport() {
    return oSupport;
  }

  public void setOSupport(int support) {
    oSupport = support;
  }





}
