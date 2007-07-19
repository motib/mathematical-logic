package dm;


import java.util.*;
//import javax.*;
import javax.swing.*;

import org.apache.log4j.*;



public class Sccl implements Comparable<Sccl>{
  public int compareTo(Sccl o) {
    int result=0;
    
    if(this.confidence == o.confidence){
      if(this.oSupport == o.oSupport){
	if(true);
      }
    }
    // TODO Auto-generated method stub
    return result;
  }

  static Logger log = Logger.getLogger(Sccl.class);
  public TreeSet<Integer> lines;
  public int oSupport;
  public double confidence;
  public String classId;

  public Sccl(){
    lines=new TreeSet<Integer>();
  }
  public Sccl(Sccl tsccl){
    oSupport=tsccl.oSupport;
    confidence=tsccl.confidence;
    classId=tsccl.classId;
    lines=new TreeSet<Integer>(tsccl.lines);
  }

  public Sccl(int s,double c, String cid, TreeSet ts) {
    oSupport=s;
    confidence=c;
    classId= cid;
    lines=new TreeSet<Integer>(ts);
  }
}