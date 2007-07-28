package weka.dm;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.io.*;

import javax.xml.transform.Result;

import weka.core.*;

import org.apache.log4j.*;
import weka.classifiers.mcar.*;
import weka.filters.mcar.LineFilter;

public class Tool {
  static Logger log=Logger.getLogger(Tool.class);

  public static Instances getLineMappedIntstances(Instances data){

    MapLine[] mapline=new MapLine[data.numAttributes()];
    for (int i = 0; i < mapline.length; i++) {
      MapLine ml=new MapLine();
      mapline[i]=ml;
    }

    ///TODO 
    Instances result = null;
    FastVector attributes =new FastVector();
    //result=new Instances()

    for (int line = 0; line < data.numInstances(); line++) {
      for (int col = 0;col < data.numAttributes(); col++){
	mapline[col].addValue(data.instance(line).value(col),line);
      }
    }

    for (int col=0; col < data.numAttributes();col++){
      TreeSet<Double> ts=new TreeSet<Double>(mapline[col].getMap().values());
      FastVector attvalues=new FastVector(ts.size());

      for (Iterator iter = ts.iterator(); iter.hasNext();) {
	Double item = (Double) iter.next();
	attvalues.addElement(String.valueOf(item));
      }
      Attribute attribute=new Attribute(String.valueOf(col),attvalues);

      attributes.addElement(attribute);
      ///TODO
      //result.deleteAttributeAt(result.numAttributes()-1);

    }
    result=new Instances( data.relationName()+ ",Mapped",attributes,0);
    result.setClassIndex(result.numAttributes()-1);
    for (int line = 0; line < data.numInstances(); line++) {
      double[] newValue=new double[data.numAttributes()];

      for (int col = 0;col < data.numAttributes(); col++){
	//values[line][col]=mapline[col].addValue(data.instance(line).value(col),line);
	double tod=data.instance(line).value(col);
	double todIndex=mapline[col].get(tod);
	String str=String.valueOf(todIndex);
	newValue[col]=result.attribute(col).indexOfValue(str);

      }
      result.add(new Instance(1,newValue));
    }
    int index=data.classIndex();
    if (index< 0){
      log.error("Class is not defiened");
      result.setClassIndex(result.numAttributes()-1);
    }else{
      result.setClassIndex(data.classIndex());
    }
    
    return result;
    //return new Instances(data,0);
  }

  public static void saveInstances(Instances  data, String fileName){
    
    BufferedWriter out=null;
    try{
       out= new BufferedWriter(new FileWriter(fileName));
      out.write(data.toString());
      out.flush();
      out.close();
    }catch(IOException ioe){
      ioe.printStackTrace();
    }
  }
  public Tool() {
    // TODO Auto-generated constructor stub
  }
  
  
  public static Instances[] getClassInstances(Instances data){
    Instances[] result=null;
    Map<Double,Instances> instancesMap=new HashMap<Double,Instances>();
    /*for (int i = 0; i < mapline.length; i++) {
      MapLine ml=new MapLine();
      mapline[i]=ml;
    }*/
    /** define the index of the class attribute , if not found set it to the last attributes*/
    int classIndex=data.classIndex();
    if (classIndex <0 ){
      classIndex=data.numAttributes()-1;
      log.error("ClassIndex not set :"+ classIndex);
    }
    /** Iterate through all data*/
    for(int i=0; i< data.numInstances();i++){
      double[] values=new double[data.numAttributes()];

      for(int j=0; j< data.numAttributes(); j++){
	values[j] =data.instance(i).value(j);
      }
      double classValue=values[classIndex];

      Instances ins=null; //to h
      /** case :    new value*/
      if(instancesMap.containsKey(classValue)){

	ins=instancesMap.get(classValue);
	if(ins== null) log.fatal("coudn't retreive ins");

      }else  {
	//log.info("add new class , map size "+ instancesMap.size());
	ins=new Instances(data,0);
	int cls=new Double(classValue).intValue();
	ins.setRelationName(data.attribute(classIndex).value(cls));
	instancesMap.put(classValue	, ins);

      }

      ins.add(new Instance(1,values));

    }
    result=new Instances[instancesMap.size()];
    instancesMap.size();
    int counter=0;
    for (Iterator iter = instancesMap.values().iterator(); iter.hasNext();) {
      Instances tins = (Instances) iter.next();

      result[counter++]=tins;
    }
    return result;
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    Instances ins=null;
    try{
      ins=new Instances(new FileReader("data/arff_116.arff"));
      ins.setClassIndex(ins.numAttributes()-1);
      log.info("Org \n"+ ins .toString());
      Instances ins2=getLineMappedIntstances(ins);
      Tool.saveInstances(ins2, "data//a.arff");
      log.info("Instances \n"+ ins2.toString());
    }catch (Exception e){
       e.printStackTrace();
    }
    //log.info(ins.toString())
  }
  
  public static Set<int[]> decart(Set<Integer> s1, Set<Integer> s2){
    Set<int[]> result=new HashSet<int[]>();
    //TODO delete the if condition later for performance
    if(s1.size()==0)log.warn("S1 is empty");
    if(s2.size()==0)log.warn("S2 is empty");
    for (Integer is1 : s1) {
      for (Integer is2 : s2) {
	int[] item=new int[2];
	item[0]=is1;
	item[1]=is2;
	result.add(item);
      }
    }
    return result;
  }

  
}
