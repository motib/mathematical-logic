package dm;

import javax.swing.*;
import java.util.*;
import java.io.*;
import java.text.DecimalFormat;

/**
 * <p>Title: data mining</p>
 * <p>Description:  fadi project</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 * */

public class Test2 {

public static void main(String[] args){
  int rows=5;
  int cols=1000000;
  double[] arr2;

  ArrayList al=new ArrayList();
  for (int j=0 ;j<rows; j++){
    double[] arr= new double[cols];
    for (int i=0; i<cols; i++){
     arr[i]=Math.random();
    };
     al.add(arr);
  }

  long t1Start,t1End,t2Start, t2End;
  double temp;
  int times=1;

  Date d1Start= new Date();
  t1Start=d1Start.getTime();

  for(int k=0; k<times; k++){
    for (int j=0 ;j<rows; j++){
      arr2=(double[])al.get(j);
      for (int i=0; i<cols; i++){
        temp=arr2[i];
      }
    }
  }
  Date d1End= new Date();
  t1End=d1End.getTime();
  System.out.println("\nFadi time1 is: "+(t1End-t1Start));
/////////
  Date d2Start= new Date();
  t2Start=d2Start.getTime();

  for(int k=0; k<times; k++){
    for (int j=0 ;j<cols; j++){
      for (int i=0; i<rows; i++){
        arr2=(double[])al.get(i);
        temp=arr2[j];
      }
    }
  }
  Date d2End= new Date();
  t2End=d2End.getTime();
  System.out.println("\nsuhel  time1 is: "+(t2End-t2Start));
  long lo=10;
  System.out.println("\nbinary "+columnName.binary(lo));
  int[] aa=columnName.orgColNames(lo,63);
  int loop=aa[0];
  System.out.println();
  for (int i=1; i<=loop; i++){
    System.out.print("\t"+aa[i]);
  }
  System.out.println("\nLONG LENGTH  "+columnName.length(lo));
  System.out.println("\natomic  "+columnName.atomicOrgColName(lo));
  System.out.println("\nfirst sub column  "+columnName.firstSubColumn(lo));
  System.out.println("\nsecond sub column  "+columnName.secondSubColumn(lo));
  double b[]={0.50,0.015,0.8,0.100,0.90};
  int a[]={2,3,2,3};
 DecimalFormat tt=new DecimalFormat("0000000000000000000000000000000000.0000");

  System.out.println("\nthe hash code is: "+tt.format(Tools.ruleOrder(a,b)));
}
}

/////
/*
Date d2Start= new Date();
t2Start=d2Start.getTime();

for(int k=0; k<times; k++){
for (int j=0 ;j<cols; j++){
  for (int i=0; i<rows; i++){
    temp=arr[i][j];
  }
}
}
Date d2End= new Date();
t2End=d2End.getTime();
*/
