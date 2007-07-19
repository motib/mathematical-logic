package dm;

import java.io.FileReader;

import org.apache.log4j.*;
import weka.core.*;
public class Test5 {

  static Logger log = Logger.getLogger(Test5.class); 
  /**
   * @param args
   */
  
  
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    Instances instances=null;;
    try{
    instances=new Instances(new FileReader("data/116.arff"));
    }catch (Exception e){
      
    }
    System.out.println(instances.toString());
  }

}
