package test;

import org.apache.log4j.*;
public class TestLog {

  /**
   * @param args
   */
  static Logger log= Logger.getLogger(TestLog.class);
  public static void main(String[] args) {
    // TODO Auto-generated method stub
    TestLog tl=new TestLog();
    tl.test();
    
    	

  }
  
  public void test(){
    log.debug("debug");
    log.info("info)" );
    log.warn("warn");
    log.fatal("fatal");
  }

}
