package dm;
import java.text.*;
import java.io.*;
import java.util.*;

/**
 * <p>Title: data mining</p>
 * <p>Description: fadi project </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 */

class Tools {
   static DecimalFormat tt=new DecimalFormat("0000000000000000000000.00000");

  public Tools() {
  }

  public static int[] getRandomSamp(int requiredLines,int size){
    int[] result=new int[requiredLines];
    Set ts=new HashSet();
    int count=0;
    while(count<requiredLines){
      int num=(int)Math.round(Math.random()*size-0.5);
      if(!ts.contains(new Integer(num))){
        ts.add(new Integer(num));
        result[count]=num;
        count++;
      }
    }
    return result;
  }
/*
b[0]=conf
b[1]=supp
b[2]=1- len/(cols num) //column Length
b[3]=occurances/total
b[3]=1- colId/total
b[4]=1- rowId/total

a[0]=conf shift
a[1]=supp sift
a[2]=len shift
a[3]=occo shift
a[4]=colId shift
*/
  public static long setItemId(long columnName,int itemLine){
    long result=columnName*1000000+itemLine;
    return result;
  }
  static double ruleOrder(int[] a,double[] b){
//    for(int k=0;k<b.length; k++){
//    System.out.print(b[k]+"\t");
 //   }
    double sum=0.0;
    for(int i=0; i<5; i++){
      sum=Math.round((sum+Math.abs(b[i]))* Math.pow(10,a[i]));
//      System.out.println(tt.format(sum));
    }
      sum+=b[5];
//    System.out.println(tt.format(sum));
//    DecimalFormat tt=new DecimalFormat("0000000000000000000000000000.000000000");

    return sum;
  }

  static Set decart(Set c1, Set c2){
    Set ss=new HashSet();
    Set cc1=new HashSet(c1);if(cc1.size()==0)System.out.println("S1 is empty");
    Set cc2=new HashSet(c2);if(cc2.size()==0)System.out.println("S2 is empty");
    int i=1;
    Iterator i1=cc1.iterator();
    while (i1.hasNext()){
      Iterator i2=cc2.iterator();
      int tntn=((Integer)i1.next()).intValue();
      while (i2.hasNext()){
        int[] a= new int[2];
        a[0]=tntn;
        a[1]=((Integer)i2.next()).intValue();
        ss.add(a);
      }
    }
    return new HashSet(ss);
  }
  public static void saveToFile(Collection testSet,Map testMap,String fs)throws IOException{
    BufferedWriter out2= new BufferedWriter(new FileWriter(fs+".xls"));
    Iterator iter=testSet.iterator();
    while(iter.hasNext()){
      Object o=(Object)testMap.get(iter.next());
      out2.write(o+"\n");
    }
    out2.close();
  }

//  public SetDataMin
}

/*class timeMeasure{

  Date d1,d2,tempD;
  long t1,t2,tempT=0,sumT=0;

  public timeMeasure(){
    d1=new Date();
  }
  public timeMeasure(Date d){
    d1=d;
  }
  public timeMeasure(long d){
    d1= new Date(d);
  }

  public void setStartDate(Date d){
    d1=new Date(d.getTime());
    tempT=0;
  }

  public void setStartDate(){
    d1=new Date();
    tempT=0;
  }

  public start(){
    d1=new Date();
    tempT=0;
    sumT=0;
  }

  public void stop(){
    d2=new Date();
    sumT=d2.getTime()-tempT.getTime();
  }

  public void pause(){
    d2=new date();
    sumT+=d2.getTime()-tempT.getTime();
  }

  public void play(){
    tempT=new date();
  }

  public void setTime(long t){
    d1=new Date(t);
  }

  public long getCurrentTime(){
    return new Date().getTime()-d1.getTime;
  }

  public date getStartTime(){
    return d1.getTime();
  }
  public Date getStartDate(){
    return d1;
  }
}
*/
