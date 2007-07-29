package weka.filters.mcar;

import weka.core.*;
import weka.core.Capabilities.*;
import weka.filters.*;
import weka.core.Instances;
import weka.dm.MapLine;
import weka.filters.SimpleBatchFilter;
import weka.gui.beans.ClassValuePicker;

import org.apache.log4j.*;

import java.io.FileReader;
import java.util.*;

public class LineFilter extends SimpleBatchFilter {
  static Logger log=Logger.getLogger(MccaFilter.class);

  public LineFilter() {
    // TODO Auto-generated constructor stub
  }

  @Override
  protected Instances determineOutputFormat(Instances data)
  throws Exception {
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
    result=new Instances( data.relationName()+ ",Formatted",attributes,0);
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
    
    return new Instances(result,0);
    //return new Instances(data,0);
  }

  @Override
  public String globalInfo() {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Capabilities getCapabilities() {
    // TODO Auto-generated method stub
    Capabilities result = super.getCapabilities();
    result.enableAllAttributes();
    result.enableAllClasses();
    //result.enable(Capability.NO_CLASS);  // filter doesn't need class to be set
    return result;
  }

  @Override
  protected Instances process(Instances data) throws Exception {
    Instances result = null;//determineOutputFormat(data);
    MapLine[] mapline=new MapLine[data.numAttributes()];
    for (int i = 0; i < mapline.length; i++) {
      MapLine ml=new MapLine();
      mapline[i]=ml;
    }


    
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
      
    }
    //TODO 
    result=new Instances( data.relationName()+ ",Mapped",attributes,0);
    
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
    
    log.info("Procceced \n "+ result.toString());
    return result;

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
    //runFilter(new MccaFilter(), options)

    Instances ins=null;
    try{
      ins=new Instances(new FileReader("D://eclipse/eclipse.3.3/workspace/weka-3.5.5/data/arff_116.arff"));
      ins.setClassIndex(ins.numAttributes()-1);
      log.info("Org \n"+ ins .toString());
      Instances ins2=new LineFilter().process(ins);
      log.info("Instances \n"+ ins2.toString());
    }catch (Exception e){
       e.printStackTrace();
    }
    //log.info(ins.toString());
    
    
   
    
  }

  private void testMapLine(){
    MapLine mpln=new MapLine();
    log.info(mpln.addValue(10, 1));
    log.info(mpln.addValue(20, 2));
    log.info(mpln.addValue(10, 3));
    log.info(mpln.addValue(30, 4));
  }
  
  private void testInsances(Instances ins){
    Instances[] insArr=MccaFilter.getClassInstances(ins);
    for (Instances in : insArr) {
      log.info(" sub ins\n "+ in.toString());
    }
  }
}





