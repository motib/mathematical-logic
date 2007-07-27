package weka.filters.mcar;

import weka.core.*;
import weka.core.Capabilities.*;
import weka.filters.*;
import weka.core.Instances;
import weka.filters.SimpleBatchFilter;
import weka.gui.beans.ClassValuePicker;

import org.apache.log4j.*;

import java.io.FileReader;
import java.util.*;

public class MccaFilter extends SimpleBatchFilter {
  static Logger log=Logger.getLogger(MccaFilter.class);

  public MccaFilter() {
    // TODO Auto-generated constructor stub
  }

  @Override
  protected Instances determineOutputFormat(Instances inputFormat)
  throws Exception {
    // TODO Auto-generated method stub
    Instances result = new Instances(inputFormat, 0);
    Attribute line=new Attribute("line");
    line.setWeight(1);
    result.insertAttributeAt(line, 0);
    return result;
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
    result.enable(Capability.NO_CLASS);  // filter doesn't need class to be set
    return result;
  }

  @Override
  protected Instances process(Instances data) throws Exception {

    int numOfAtts=data.numAttributes();

    /*MapLine[] mapline=new MapLine[numOfAtts];
    for (int i = 0; i < mapline.length; i++) {
      MapLine ml=new MapLine();
      mapline[i]=ml;
    }*/


    Instances result = new Instances(determineOutputFormat(data), 0);
    for (int i = 0; i < data.numInstances(); i++) {
      Instance ins=data.instance(i);

      double[] mValues = new double[result.numAttributes()];
      mValues[0]=i;//set the line number
      for (int n = 0; n < numOfAtts; n++){
	mValues[n+1] = data.instance(i).value(n);
      }
      result.add(new Instance(1,mValues));
    }
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
    }catch (Exception e){}
    log.info(ins.toString());

    Instances[] insArr=MccaFilter.getClassInstances(ins);
    for (Instances in : insArr) {
      log.info(" sub ins\n "+ in.toString());
    }
  }

  private void testMapLine(){
    MapLine mpln=new MapLine();
    log.info(mpln.addValue(10, 1));
    log.info(mpln.addValue(20, 2));
    log.info(mpln.addValue(10, 3));
    log.info(mpln.addValue(30, 4));
  }
}




