package dm;


import java.util.*;
//import javax.*;
import javax.swing.*;
import java.text.DecimalFormat;

/**
  <p>Title: data mining</p>
 * <p>Description:  fadi project</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 */

public class Column {
  private final DataMine dm;
  final long columnId;
  private boolean isAtomic=false;
  private boolean tag=false;// indicate that the column either an atomic or the subcolumns exist;
  private Column fColumn,sColumn;
  private int numOfCols;
  private final int oSupport;
  private double confidence;
  TreeMap items;//to be changed later to HashMap
  Map itemsAsString;
  private DecimalFormat tt=new DecimalFormat("00.0000");

  // check if the column is atomic, if not check the subcolumns if existed
  public Column(long intClmn, int oSup,double con ,DataMine datMin) {
    dm=datMin;
    oSupport=oSup;
//    JOptionPane.showMessageDialog(null,"new column minimum occ"+oSupport);
    confidence=con;
    columnId=intClmn;
    numOfCols=dm.CLASS;
    if (columnName.length(intClmn)==1){
      isAtomic=true;
      tag= true;
      return;
    }
    long sub1=columnName.firstSubColumn(intClmn);
    long sub2=columnName.secondSubColumn(intClmn);
    if(   !datMin.existingColumns.keySet().contains(new Long(sub1))
       || !datMin.existingColumns.keySet().contains(new Long(sub2)))
      return;
    fColumn=(Column)datMin.existingColumns.get(new Long(sub1));
    sColumn=(Column)datMin.existingColumns.get(new Long(sub2));
    tag = true;
  }

  public boolean isAppropriate(){
    if(tag==false) return false;
    if (isAtomic) return generateAtomicValues();
    return generateValues();
  }

  private void generateOccurances(){
    items=new TreeMap();
    Map ts= new HashMap();
    // scan the entity and get the distinct values and the lines which accompany each distinct value
    Iterator iter2=dm.allLines.iterator();
    while(iter2.hasNext()){
      Integer ttt=(Integer)iter2.next();
      String s=((String[])dm.entity.get(ttt))[columnName.atomicOrgColName(columnId)];
      if(ts.keySet().contains(s)){
        Integer fi=(Integer)ts.get(s);
        ((Sccl)items.get(fi)).lines.add(ttt);
      }else{
        ts.put(s,ttt);
        Sccl ss= new Sccl();
        ss.lines.add(ttt);
        items.put(ttt,ss);
      }
    }
  }
///generate Test occurances,
 public boolean generateTestOccurances(){
    itemsAsString=new HashMap();
    items=new TreeMap();
    // scan the entity and get the distinct values and the lines which accompany each distinct value
    Iterator iter2=dm.allLines.iterator();
    while(iter2.hasNext()){
      Integer ttt=(Integer)iter2.next();
      String s=((String[])dm.entity.get(ttt))[columnName.atomicOrgColName(columnId)];
      if(itemsAsString.keySet().contains(s)){
        Integer fi=(Integer)itemsAsString.get(s);
        ((Sccl)items.get(fi)).lines.add(ttt);
      }else{
        itemsAsString.put(s,ttt);
        Sccl ss= new Sccl();
        ss.lines.add(ttt);
        items.put(ttt,ss);
      }
    }
    return true;
  }
/////
  // generate the items of the Atomic column
  private boolean generateAtomicValues(){
    generateOccurances();
 // remove the items which has not the appropriate support
    TreeMap myMap=new TreeMap(items);
    Iterator itr=myMap.keySet().iterator();
    while(itr.hasNext()){
      Integer tgr=(Integer)itr.next();
      Sccl itemContent=(Sccl)items.get(tgr);
      if (!isSurvived(itemContent))items.remove(tgr);
      }
    // check the items , if there is any value return true
    if(items.size()==0) return false;
    return true;
  }
    private boolean generateValues(){
    items=new TreeMap();
    boolean result=false;
    Set possibleEntity =new HashSet(Tools.decart(fColumn.items.keySet(),sColumn.items.keySet()));
    Iterator itr= possibleEntity.iterator();
    while(itr.hasNext()){
      int[] ia=(int[])itr.next();
      if(genarateItemValue(new Integer(ia[0]),new Integer(ia[1])))
        result= true;
    }
    return result;
  }

  private boolean genarateItemValue(Integer fItem, Integer sItem){
    Sccl tsccl=new Sccl((Sccl)fColumn.items.get(fItem));
    TreeSet ts=(TreeSet)((Sccl)sColumn.items.get(sItem)).lines;
    tsccl.lines.retainAll(ts);
    if ( tsccl.lines.size() == 0) return false;
    if (!isSurvived(tsccl))return false;
    items.put(tsccl.lines.first(),tsccl);
    return true;
    }

  private boolean isSurvived(Sccl itm){
    if (itm.lines.size()<oSupport) return false;
    //calculate the confidence
    Map tm= new HashMap();
    TreeSet ts=(TreeSet)itm.lines;
    Iterator itr=ts.iterator();
    while( itr.hasNext()){
      String stcls=dm.classArray[((Integer)itr.next()).intValue()];
      Integer freq=(Integer)tm.get(stcls);
      tm.put(stcls,( freq==null? new Integer(1):new Integer(freq.intValue()+1)));
    }
    // calculate the max confidence
    String maxClass=" ";
    int maxInt=0;
    for (Iterator i=tm.entrySet().iterator(); i.hasNext(); ) {
      Map.Entry e = (Map.Entry) i.next();
      int j=((Integer)e.getValue()).intValue() ;
      if( j > maxInt){
        maxInt=j;
        maxClass=(String)e.getKey();
      }
    }
    if (maxInt<oSupport)return false;//check support
    double cnf=(double)maxInt/(double)ts.size();
    int rowId=((Integer)itm.lines.first()).intValue();
    itm.classId=maxClass;
    itm.confidence=cnf;
    itm.oSupport=maxInt;
    if( cnf >= confidence ){ //check confidence
      dm.rules.rankARule(ts.size(),maxInt, columnId, rowId, maxClass);
      dm.rules.idis.add(new Long(Tools.setItemId(columnId,rowId)));
    }
    return true;
  }


  public String calculateColumnConfidences(double confi){
    confidence=confi;
    String otpt1="\n\n Confidences for the column "+columnId+" are:"+
                "\n-----------------------------------";
    String otpt2=new String();
    Iterator itr=items.keySet().iterator();
    while( itr.hasNext()){
      Integer tr= (Integer)itr.next();
      String s=calculateItemConfidence(tr);
      if(!s.startsWith("-1")) otpt2+=s;
    }
    if(otpt2.length()!= 0) return otpt1+otpt2;
    return null;
  }

    public String calculateItemConfidence(Integer itm)throws NumberFormatException{
    Sccl scclItm=(Sccl)items.get(itm);
    double cnf=scclItm.confidence;
    double supp=(double)scclItm.oSupport/dm.TOTAL_ENTITIES;
    String maxClass=scclItm.classId;
    return "\n"+getLine(itm)+"\tclass="+maxClass+"\tconf.="+tt.format(cnf)+"\tsupp="+tt.format(supp);
  }




  public String getSupports(){
    if(!isAppropriate())return "-1";
    String output1="\n\nThe supports >= "+tt.format(oSupport)+" in column "+columnId+" are:"+
                "\n--- --- --- --- --- --- --- --- --- --- --- --- --- ";
    String output2="";
    Iterator itr=items.keySet().iterator();
    while( itr.hasNext()){
      Integer tr= (Integer)itr.next();
      output2+=getItemSupport(tr);
    }
    if(output2 != null) return new String(output1+output2);
    return null;
  }

  private String getItemSupport(Integer itm){
    double dbl=(double)((Sccl)items.get(itm)).oSupport/dm.TOTAL_ENTITIES;
    return "\n"+getLine(itm)+"\tsupp.="+tt.format(dbl);
  }
  //be carefull there is no code here to check the existance of the item :itm


  private String getLine(Integer itm){
    int[] index=columnName.orgColNames(columnId,60);//numOfCols
    int cols=index[0];
    String sss1="";
    for (int j=1; j<=cols; j++){
       sss1 +="C"+index[j]+"="+((String[])dm.entity.get(itm))[index[j]];
       if(index[j] <columnName.length(columnId))
        sss1+="\tAND\t";
    }
    return sss1;
  }
  public String prntRuleOcc(Integer teger){
    TreeSet ts=(TreeSet)((Sccl)items.get(teger)).lines;
    return ts.toString();
  }

  public TreeSet getValueOccurances(String value){
    Integer matchLine=(Integer)itemsAsString.get(value);
    if(matchLine==null)return new TreeSet();
//    JOptionPane.showMessageDialog(null,"getValueOccurances\n value: "+value+
//              "\n first Occurance: "+matchLine.intValue());
    Sccl resultSccl=(Sccl)items.get(matchLine);
    TreeSet resultSet=(TreeSet)resultSccl.lines;
    return new TreeSet(resultSet);
  }
  private boolean isSurvivedNew(Sccl itm){
    //calculate the confidence
    Map tm= new HashMap();
    TreeSet ts=(TreeSet)itm.lines;
    Iterator itr=ts.iterator();
    while( itr.hasNext()){
      String stcls=dm.classArray[((Integer)itr.next()).intValue()];
      Integer freq=(Integer)tm.get(stcls);
      tm.put(stcls,( freq==null? new Integer(1):new Integer(freq.intValue()+1)));
    }
    // calculate the max confidence
    String maxClass=" ";
    int maxInt=0;
    for (Iterator i=tm.entrySet().iterator(); i.hasNext(); ) {
      Map.Entry e = (Map.Entry) i.next();
      int j=((Integer)e.getValue()).intValue() ;
      if( j > maxInt){
        maxInt=j;
        maxClass=(String)e.getKey();
      }
    }
    double cnf=(double)maxInt/(double)ts.size();
    int rowId=((Integer)itm.lines.first()).intValue();
    itm.classId=maxClass;
    itm.confidence=cnf;
    itm.oSupport=maxInt;
    if( dm.rules.idis.contains(new Long(Tools.setItemId(columnId,rowId))))//check confidence
      dm.rules.rankARule(ts.size(),maxInt, columnId, rowId, maxClass);
    return true;
  }
  private boolean generateAtomicValuesNew(){
    generateOccurances();
 // remove the items which has not the appropriate support
    TreeMap myMap=new TreeMap(items);
    Iterator itr=myMap.keySet().iterator();
    while(itr.hasNext()){
      Integer tgr=(Integer)itr.next();
      Sccl itemContent=(Sccl)items.get(tgr);
      if (!isSurvived(itemContent))items.remove(tgr);
      }
    // check the items , if there is any value return true
    if(items.size()==0) return false;
    return true;
  }
    private boolean generateValuesNew(){
    items=new TreeMap();
    boolean result=false;
    Set possibleEntity =new HashSet(Tools.decart(fColumn.items.keySet(),sColumn.items.keySet()));
    Iterator itr= possibleEntity.iterator();
    while(itr.hasNext()){
      int[] ia=(int[])itr.next();
      if(genarateItemValueNew(new Integer(ia[0]),new Integer(ia[1])))
        result= true;
    }
    return result;
  }
  private boolean genarateItemValueNew(Integer fItem, Integer sItem){
    Sccl tsccl=new Sccl((Sccl)fColumn.items.get(fItem));
    TreeSet ts=(TreeSet)((Sccl)sColumn.items.get(sItem)).lines;
    tsccl.lines.retainAll(ts);
    if ( tsccl.lines.size() == 0) return false;
    if (!isSurvivedNew(tsccl))return false;
    items.put(tsccl.lines.first(),tsccl);
    return true;
    }
  public boolean isAppropriateNew(){
    if(tag==false) return false;
    if (isAtomic) return generateAtomicValuesNew();
    return generateValuesNew();
  }

}