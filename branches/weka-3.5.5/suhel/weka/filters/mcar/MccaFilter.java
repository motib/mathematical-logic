package weka.filters.mcar;

import weka.core.*;
import weka.core.Capabilities.*;
import weka.filters.*;
import weka.core.Instances;
import weka.filters.SimpleBatchFilter;
import org.apache.log4j.*;
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
    result.insertAttributeAt(new Attribute("line"), 0);
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
    // TODO Auto-generated method stub
    int numOfAtts=data.numAttributes();
    log.info("Number of Atts " + numOfAtts);
    MapLine[] mapline=new MapLine[numOfAtts];
    
    
    Instances result = new Instances(determineOutputFormat(data), 0);
    
    for (int i = 0; i < data.numInstances(); i++) {
      double[] values = new double[result.numAttributes()];
      double[] mValues = new double[result.numAttributes()];
      for (int n = 0; n < numOfAtts; n++){
	log.info(data.instance(i).value(n));
	values[n+1] = data.instance(i).value(n);
	log.info(data.instance(i).value(n));
	double td=mapline[n].addValue(values[n+1], i);
	mValues[n+1]=td;
      }
      values[0] = i;
      
      
      for (int j = 0; j < values.length; j++) {
	System.out.print(""+ values[j]+"\t");
      }
      System.out.println();
      
      for (int j = 0; j < values.length; j++) {
	System.out.print(""+ mValues[j]+"\t");
      }
      System.out.println();
      
      result.add(new Instance(1, mValues));
    }
    return result;

  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    //runFilter(new MccaFilter(), options)
    MapLine mpln=new MapLine();
    log.info(mpln.addValue(10, 1));
    log.info(mpln.addValue(20, 2));
    log.info(mpln.addValue(10, 3));
    log.info(mpln.addValue(30, 4));
    
  }

}

class MapLine{
  static Logger log=Logger.getLogger(MapLine.class);
  
  private Map<Double,Double> map=new HashMap<Double, Double>();
  
  public double addValue(double value, double line){
    double result=0.0;
    Double ln=map.get(value);
    if (ln!=null){
      if (ln.doubleValue()<line){
	return ln.intValue();
      }else
	log.error("line " + line +" must be greater than " + ln.doubleValue());
      return -1;
     
      
    }else{
      map.put(value, line);
      return line;
    }
    
  }
  
}
