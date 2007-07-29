package weka.dm;


import weka.core.*;

import org.apache.log4j.*;

import dm.ColumnName;



import java.io.FileReader;
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
    if(existingColumns==null)return false;
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
    int col=ColumnName.atomicOrgColName(columnId)-1;
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
	tmap.put(value, line);
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
      Set<Integer> set=generateItemOccurances( is[0] , is[1] );
      if (set.size() > 0){
	int frstLine= Column.first(set);
	result.put(frstLine, set);
      }
    }
    return result;
  }

  protected  Set<Integer> generateItemOccurances(int fItem, int sItem){
     Set<Integer> s1= new  HashSet<Integer>(fColumn.items.get(fItem));
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

  protected Map<Integer, Set<Integer>> getItems() {
    return items;
  }

  protected void setItems(Map<Integer, Set<Integer>> items) {
    this.items = items;
  }

  public  Instances getItemsAsInstances( ){
    return getItemsAsInstances(this.items, instances.relationName()+","+this.columnId);
  }
  public static int first(Set<Integer> set){
    int result=Integer.MAX_VALUE;
    for (Integer iter : set) {
      if(iter<result)result=iter;
    }
    return result;
  }
  public static Instances getItemsAsInstances(Map<Integer,Set<Integer>> map, String relationName){
    Instances result=null;
    FastVector atts=new FastVector(2);
    FastVector nullVectr=null;
    Attribute line=new Attribute("line",nullVectr);
    Attribute count=new Attribute("counts");
    Attribute occs=new Attribute("occs",nullVectr);

    atts.addElement(line);
    atts.addElement(count);
    atts.addElement(occs);
    result=new Instances(relationName,atts,0);
    for (Map.Entry<Integer,Set<Integer>> iter : map.entrySet()) {
      Instance ins=new Instance(3);
      ins.setDataset(result);
      ins.setValue(0, String.valueOf(iter.getKey()) );
      


      String occStr="";
      //TODO don't use TreeSet for performance issues
      TreeSet<Integer> lns=new TreeSet<Integer>(iter.getValue());
      for (Integer ln : lns) {
	occStr+=ln.toString()+"-";
      }
      // occStr+="'";
      ins.setValue(1, lns.size());
      ins.setValue(2, occStr );
      result.add(ins);
    }


    return result;
  }
  /**
   * affect items, ruleSet call checkPossibleRule
   * @return
   */


  /**
   * @param args
   */


  public int getOSupport() {
    return oSupport;
  }

  public void setOSupport(int support) {
    oSupport = support;
  }
  public   String printItemMap(){
    return printItemMap(items);
  }
  public static String printItemMap(Map<Integer,Set<Integer>> map){
    StringBuffer result=new StringBuffer();
    for (Map.Entry<Integer, Set<Integer>> iter : map.entrySet()) {
      result.append("\n"+iter.getKey().toString()+",");
      TreeSet<Integer> lns=new TreeSet<Integer>(iter.getValue());
      for (Integer ln : lns) {
	result.append(ln.toString()+"-");
      }
    }
    return result.toString();
  }
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    Instances ins=null;
    try{
      ins=new Instances(new FileReader("data/arff_116.arff"));
      ins.setClassIndex(ins.numAttributes()-1);
      log.info("Org \n"+ ins .toString());
      Column clmn=new Column(ins,1,null);
      clmn.setItems(clmn.generateAtomicOccurances());
      log.info("print item map"+clmn.printItemMap());
      log.info("get occs\n"+ clmn.getItemsAsInstances().toString());


    }catch (Exception e){
      e.printStackTrace();
    }

  }



}
