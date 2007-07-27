package weka.filters.mcar;

import org.apache.log4j.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.log4j.Logger;

public class MapLine {

  static Logger log=Logger.getLogger(MapLine.class);

  private Map<Double,Double> map=new HashMap<Double, Double>();
  private Map<String,Double> smap=new HashMap<String, Double>();
  private Map<Double,Double> m	=new HashMap<Double, Double>();


  public double addValue(double value, double line){
    double result=0.0;
    Double ln=map.get(value);
    if (ln!=null){
      if (ln.doubleValue()<  line){
	return ln.doubleValue();
      }else
	log.error("line " + line +" must be greater than " + ln.doubleValue());
      return -1;


    }else{
      map.put(value, line);
      return line;
    }

  }
  public double addValue(String value, double line){
    double result=0.0;
    Double ln=smap.get(value);
    if (ln!=null){
      if (ln.doubleValue()<  line){
	return ln.doubleValue();
      }else
	log.error("line " + line +" must be greater than " + ln.doubleValue());
      return -1;


    }else{
      smap.put(value, line);
      return line;
    }

  }
  /**
   * 
   * @return double[][] orgValue ----> firstLineOcc
   */
  public double[][] getMapArray(){
    double[][] result=new double [map.size()][2];
    int counter=0;
    for (Iterator iter = map.entrySet().iterator(); iter.hasNext();) {
      Map.Entry<Double,Double> e = (Map.Entry<Double,Double>) iter.next();
      result[counter][0]=e.getKey();
      result[counter][1]=e.getValue();
    }
    
    return result;
  }
  
  public Map getMap(){
    
    return  map;
  }
  
  public double get(double key){
    return map.get(key);
  }


}