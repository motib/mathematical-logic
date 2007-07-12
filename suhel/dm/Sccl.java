package dm;


import java.util.*;
//import javax.*;
import javax.swing.*;
/**
 * <p>Title: data mining</p>
 * <p>Description: fadi project </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 */

public class Sccl {
  public TreeSet lines;
  public int oSupport;
  public double confidence;
  public String classId;

  public Sccl(){
    lines=new TreeSet();
  }
  public Sccl(Sccl tsccl){
    oSupport=tsccl.oSupport;
    confidence=tsccl.confidence;
    classId=tsccl.classId;
    lines=new TreeSet(tsccl.lines);
  }

  public Sccl(int s,double c, String cid, TreeSet ts) {
    oSupport=s;
    confidence=c;
    classId= cid;
    lines=new TreeSet(ts);
  }
}