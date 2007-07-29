package dm;


import java.io.*;
import java.util.*;

import javax.swing.*;
import java.text.DecimalFormat;
import org.apache.log4j.*;
import weka.core.*;

public class DataMine {
  static Logger log = Logger.getLogger(DataMine.class);
  /**
   * line number , String Array of the row items, row[0] is the class
   */
  public Map<Integer,String[]> entity;
  classColumn classCol;
  //holds the position of CLASS attribute
  public final int CLASS;
  final String[] classArray;
  public String[] pedictedClass;


  int orgSize;
  //size of the data (total number)
  int TOTAL_ENTITIES;
  public TreeMap <Long, Column>existingColumns;
  long allColumns=0L;
  public Rules rules;
  //holds the line numbers
  public Set<Integer> allLines;
  Instances instances;



  public DataMine(String filename,String instance){
    try{
      instances=new Instances(new FileReader(filename));
    }catch (Exception fnf){
      System.err.print(fnf.getStackTrace());
    }
    this. instances=new Instances(instances);
    int numOfCols=instances.numAttributes();
    TOTAL_ENTITIES=instances.numInstances();
    log.trace("TOTAL_ENTITES  "+ TOTAL_ENTITIES);
    log.trace("numOfCols "+numOfCols);

    orgSize=TOTAL_ENTITIES;
    classArray= new String[TOTAL_ENTITIES];
   
    entity=new TreeMap<Integer,String[]>();

    
    log.info("Instances: "+instances.toString());

    int line=1;
    Enumeration<Instance> ins=instances.enumerateInstances();
    while (ins.hasMoreElements()) {
      String[] row=new String[numOfCols];
      Instance inselem = (Instance) ins.nextElement();
      for (int i = 0; i < numOfCols-1; i++) {
	row[i+1]=inselem.stringValue(i);
	System.out.print(inselem.stringValue(i)+"\t");
      }
      System.out.println("\t");
      row[0]=inselem.stringValue(numOfCols-1);
      log.trace("row[0] :"+row[0]);
      classArray[line]=inselem.stringValue(numOfCols-1);
      entity.put(line, row);
    }

    allLines= new TreeSet<Integer>(entity.keySet());
    CLASS=((String[])entity.get(new Integer(1))).length;


    existingColumns =new TreeMap<Long, Column>();
    //Get the number of possible compound columns 
    allColumns=Math.round(Math.pow(2,CLASS-1))-1;
    rules=new Rules(this);
    classCol=new classColumn(this);


  }
  /**
   * 
   * @param entity
   * @param aClass
   */
  public DataMine( Map<Integer,String[]> entity, String[] aClass) {
    this.entity = new TreeMap<Integer,String[]>(entity);//////
    allLines= new TreeSet<Integer>(this.entity.keySet());
    CLASS=((String[])this.entity.get(new Integer(1))).length;
    TOTAL_ENTITIES=this.entity.size();
    orgSize=this.entity.size();
    classArray= new String[aClass.length];
    System.arraycopy(aClass,0,classArray,0,aClass.length);
    existingColumns =new TreeMap<Long, Column>();
    //Get the number of possible compound columns 
    allColumns=Math.round(Math.pow(2,CLASS-1))-1;
    rules=new Rules(this);
    classCol=new classColumn(this);
  }

  /**
   * 
   * @param pinstances
   */
  public DataMine( Instances instances){


    this. instances=new Instances(instances);
    int numOfCols=instances.numAttributes();
    TOTAL_ENTITIES=instances.numInstances();
    //log.trace("TOTAL_ENTITES  "+ TOTAL_ENTITIES);
    //log.trace("numOfCols "+numOfCols);

    orgSize=TOTAL_ENTITIES;
    classArray= new String[TOTAL_ENTITIES];
   
    entity=new TreeMap<Integer,String[]>();

    
    //log.info("Instances: "+instances.toString());

    int line=1;
    Enumeration<Instance> ins=instances.enumerateInstances();
    while (ins.hasMoreElements()) {
      String[] row=new String[numOfCols];
      Instance inselem = (Instance) ins.nextElement();
      for (int i = 0; i < numOfCols-1; i++) {
	row[i+1]=inselem.stringValue(i);
	System.out.print(inselem.stringValue(i)+"\t");
      }
      System.out.println("\t");
      row[0]=inselem.stringValue(numOfCols-1);
      //log.trace("row[0] :"+row[0]);
      classArray[line]=inselem.stringValue(numOfCols-1);
      entity.put(line, row);
    }

    allLines= new TreeSet<Integer>(entity.keySet());
    CLASS=((String[])entity.get(new Integer(1))).length;


    existingColumns =new TreeMap<Long, Column>();
    //Get the number of possible compound columns 
    allColumns=Math.round(Math.pow(2,CLASS-1))-1;
    rules=new Rules(this);
    classCol=new classColumn(this);



  }

  public DataMine(String fileName){
    Object[] o=new Object[2];
    dataReader(fileName,o);
    entity = (TreeMap)o[0];//////
    classArray=(String[])o[1];
    //    System.out.println("out side:" +entity.size());
    allLines= new TreeSet(entity.keySet());
    CLASS=((String[])entity.get(new Integer(1))).length;
    TOTAL_ENTITIES=entity.size();
    orgSize=entity.size();
    //    classArray= new String[aClass.length];classArray=
    //   System.arraycopy(aClass,0,classArray,0,aClass.length);
    existingColumns =new TreeMap<Long, Column>();
    allColumns=Math.round(Math.pow(2,CLASS-1))-1;
    rules=new Rules(this);
    classCol=new classColumn(this);

  }
  public DataMine(DataMine PDm){
    entity = new TreeMap(PDm.entity);//////
    allLines= new TreeSet(entity.keySet());
    CLASS=((String[])entity.get(new Integer(1))).length;
    TOTAL_ENTITIES=entity.size();
    orgSize=entity.size();
    classArray= new String[PDm.classArray.length];
    System.arraycopy(PDm.classArray,0,classArray,0,PDm.classArray.length);
    existingColumns =new TreeMap<Long, Column>();
    allColumns=Math.round(Math.pow(2,CLASS-1))-1;
    rules=new Rules(this);
    classCol=new classColumn(this);

  }

  public boolean generateColumnsNew(){
    existingColumns.clear();
    for(long i=1; i<=allColumns; i++){
      Column clmn=new Column(i,0,0,this);
      if(clmn.isAppropriateNew()){
	existingColumns.put(i,clmn);
      }
    }
    return true;
  }
  public boolean generateColumns(double support,double confidence){

    int oSupport=(int)Math.round(0.5+support*orgSize);
    existingColumns.clear();
    for(long i=1; i<=allColumns; i++){
      Column clmn=new Column(i,oSupport,confidence,this);
      if(clmn.isAppropriate()){
	existingColumns.put(i,clmn);
      }
    }
    return true;
  }
  public String getClassValue(int index){
    return classArray[index];
  }
  //generate the occurances atomic columns, no need to check the support and the confidence
  public boolean generateOccuranceColumns(){
    existingColumns.clear();
    for(long i=1; i<=allColumns; i*=2){
      Column clmn=new Column(i,0,0.0,this);
      existingColumns.put(new Long(i),clmn);
      clmn.generateTestOccurances();
    }
    return true;
  }
  
  public StringBuffer printConfidences(double support, double confidence){
    StringBuffer otpt=new StringBuffer();
    generateColumns(support,confidence);
    Iterator itr =existingColumns.keySet().iterator();
    while(itr.hasNext()){
      Long myInt = (Long)itr.next();
      Column clmn=(Column)existingColumns.get(myInt);
      otpt.append(clmn.calculateColumnConfidences(confidence));
    }
    return otpt;
  }
  public StringBuffer printSupports(double support,double confidence){
    StringBuffer otpt=new StringBuffer();
    generateColumns(support,confidence);
    Iterator<Long> itr =existingColumns.keySet().iterator();
    while(itr.hasNext()){
      Long myInt = (Long)itr.next();
      Column clmn=(Column)existingColumns.get(myInt);
      otpt.append(clmn.getSupports());
    }
    return otpt;
  }
  public void adjustTotalEn(){
    TOTAL_ENTITIES=entity.size();
  }
  
  public boolean setLines(Set lines){
    existingColumns.clear();
    allLines.clear();
    boolean b=allLines.addAll(lines);
    TOTAL_ENTITIES=allLines.size();
    classCol=new classColumn(this);
    return b;

  }
  public boolean delLines(Set<Integer> lines){
    existingColumns.clear();
    boolean b=allLines.removeAll(lines);
    TOTAL_ENTITIES=allLines.size();
    classCol=new classColumn(this);
    return b;
  }
  
  public boolean resetLines(){
    boolean b=setLines(entity.keySet());
    return b;
  }
  public Set<Integer> getRuleColValOcc(String cond,int clmn){
    long clmnNm=1;
    for(int i=0;i<clmn-1;i++)clmnNm*=2;
    Column c=existingColumns.get(clmnNm);
    Integer fstOcc=c.getItemInt(cond);
    if(fstOcc==null)return new HashSet<Integer>();
    Sccl sccl=c.getSccl(fstOcc);
    return sccl.lines;
  }
/**
 *
 * @param PFileName
 * @param o: o[0]=entity;o[1]=classes;
 */
  void dataReader(String PFileName,Object[] o){
    String[] classes;
    TreeMap entts;
    int errorLine=1;
    try{
      BufferedReader in= new BufferedReader(new FileReader(PFileName));
      //String s=new String();
      in.readLine();
      in.read();
      String s=in.readLine();//read the number of rows
      int size=Integer.parseInt(s)	;
      entts=new TreeMap();
      classes=new String[size+1];
      int numOfCols=0;
      s=in.readLine().toUpperCase().trim();//
      while(!s.startsWith("@DATA") && s.startsWith("@ATTRIBUTE")){
	numOfCols++;
	s=in.readLine();
      }

      s=in.readLine();//read the first row
      for(int lineNumber=1; lineNumber<=size; lineNumber++){
	errorLine++;
	StringTokenizer st=new StringTokenizer(s,",");
	String[] row=new String[numOfCols] ;
	int i=0;
	while(st.hasMoreTokens()){
	  i=(i+1)%numOfCols;
	  row[i]=st.nextToken();
	}
	//        System.out.println(""+lineNumber+" , "+row[1]);
	entts.put(new Integer(lineNumber), row );
	classes[lineNumber]=row[0];
	s=in.readLine();
      }

      in.close();
      //    System.out.println("size:"+entts.size());
      o[0]=entts;
      o[1]=classes;
    }catch (Exception e){
      JOptionPane.showMessageDialog(null,"Error At Line "+ errorLine);
    }
  }
  
  /**
   *
   * @return string represent this datamine
   */
  public String printDataMine(){
    //    Iterator iterS=
    String[] line=new String[CLASS];
    String result="\n";
    Iterator iter=entity.entrySet().iterator();
    while(iter.hasNext()){
      Map.Entry<Integer,String[]> e= (Map.Entry<Integer,String[]>)iter.next();
      result+="\n"+(e.getKey()).intValue();
      line=e.getValue();
      for(int i=1; i<line.length; i++){
	result+="\t"+line[i];
      }
    }
    return result;
  }
  public void iterate(double minRemainInst,double conf, double supp,int numOfItr)
  throws IOException{
    allLines= new TreeSet<Integer>(entity.keySet());
    rules=new Rules(this);
    rules.iterate(minRemainInst,conf,supp,numOfItr);
  }
  public void iterate2(double minRemainInst,double conf, double supp,int numOfItr)
  throws IOException{
    allLines= new TreeSet<Integer>(entity.keySet());
    rules=new Rules(this);
    rules.iterate2(minRemainInst,conf,supp,numOfItr);
  }

  public Set<Integer>  getValueOccurancesInColumn(String value,long colId){
    Column clmn=existingColumns.get(colId);
    return clmn.getValueOccurances(value);
  }

  public static void main(String[] args){
    Instances instances=null;
    try{
      FileReader reader = new FileReader("data/test.txt"); 
      instances = new Instances(reader);
    }catch( Exception e){
      log.fatal("File Not Found");
    }
    DataMine dm=new DataMine(instances);

  }
}











