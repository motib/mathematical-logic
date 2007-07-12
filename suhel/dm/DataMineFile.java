package dm;


import java.util.*;
import java.io.*;
//import javax.*;
import javax.swing.*;
import java.text.DecimalFormat;

/**
 * <p>Title: data mining</p>
 * <p>Description:  fadi project</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 */

public class DataMineFile {

  public DataMineFile(){
  }

  public static DataMine setFilePath(File filePath)
    throws java.io.IOException {
     ReadPara rp=new ReadPara(filePath);
    rp.fafo();
    String[] aClass=new String[rp.arrData.length+1];//
//    ArrayList a1=new ArrayList();a1.add("dummy");
    TreeMap aa1=new TreeMap();/////
    for(int i=0; i<rp.arrData.length; i++){
//    ArrayList a2=new ArrayList();
      String[] aa2=new String[rp.arrData[1].length];/////
//      a2.add(rp.arrData[i][rp.arrData[2].length-1]);
      aa2[0]=rp.arrData[i][rp.arrData[2].length-1];///
        for(int j=0 ;j<rp.arrData[1].length-1; j++){
 //         a2.add(rp.arrData[i][j]);
          aa2[j+1]=rp.arrData[i][j];///////
        }
//        a1.add(a2);
        aa1.put(new Integer(i+1),aa2);

        aClass[i+1]=rp.arrData[i][rp.arrData[2].length-1];
    }
    return new DataMine(aa1,aClass);
 }
//suhel version of set file path
  public static DataMine setFilePath(String filePath)
      throws java.io.IOException {
     ReadPara rp=new ReadPara(filePath);
    rp.fafo();
    String[] aClass=new String[rp.arrData.length+1];//
    Map aa1=new HashMap();/////
    for(int i=0; i<rp.arrData.length; i++){
      String[] aa2=new String[rp.arrData[1].length];/////
      aa2[0]=rp.arrData[i][rp.arrData[2].length-1];///
        for(int j=0 ;j<rp.arrData[1].length-1; j++){
          aa2[j+1]=rp.arrData[i][j];///////
        }
        aa1.put(new Integer(i+1),aa2);
        aClass[i+1]=rp.arrData[i][rp.arrData[2].length-1];
    }
    return new DataMine(aa1,aClass);
    }
  /*
    throws java.io.IOException {
     ReadPara rp=new ReadPara(filePath);
    rp.fafo();
    String[] aClass=new String[rp.arrData.length+1];//
    ArrayList a1=new ArrayList();a1.add("dummy");

    for(int i=0; i<rp.arrData.length; i++){
      ArrayList a2=new ArrayList();a2.add(rp.arrData[i][rp.arrData[2].length-1]);
        for(int j=0 ;j<rp.arrData[1].length-1; j++)
          a2.add(rp.arrData[i][j]);
        a1.add(a2);
        aClass[i+1]=rp.arrData[i][rp.arrData[2].length-1];
    }
    return new DataMine(a1,aClass);
 }

*/
}