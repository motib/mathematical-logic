package weka.filters.mcar;

import org.apache.log4j.*;
import java.util.HashMap;
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

 
}