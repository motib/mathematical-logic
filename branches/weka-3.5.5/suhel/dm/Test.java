package dm;



import javax.swing.*;
import java.util.*;
import java.io.*;

/**
 * <p>Title: data mining</p>
 * <p>Description:  fadi project</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 * */

public class Test {
/*
public static void main(String[] args){
int rows=1000000;
int cols=5;


double[][] arr= new double[rows][cols];
for (int j=0 ;j<rows; j++){
  for (int i=0; i<cols; i++){
    arr[j][i]=Math.random();
  }
}

long t1Start,t1End,t2Start, t2End;
double temp;
int times=100;

Date d1Start= new Date();
t1Start=d1Start.getTime();

for(int k=0; k<times; k++){
for (int j=0 ;j<rows; j++){
  for (int i=0; i<cols; i++){
    temp=arr[j][i];
  }
}
}
Date d1End= new Date();
t1End=d1End.getTime();


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

System.out.println("\nfadi time1 is: "+(t1End-t1Start));
System.out.println("\nsuhel  time1 is: "+(t2End-t2Start));
}

*/
  static double sup=0.0500;
  static double con=0.400;
  public static void main(String args[] )throws java.io.IOException{
//    DataMine dm =DataMineFile.setFilePath("c://a//zoo.txt");//training file
    DataMine dm=new DataMine("c://a//contact.txt");
//    System.out.println(dm.printSupports(sup,con));
    System.out.println(dm.printConfidences(sup,con));
    System.out.print(dm.printSupports(sup,con));

    System.out.println(dm.rules.printRankedRuls());
    dm.rules.iterate(0.15,con,sup,1);
    dm.rules.SaveToFile("c://a//contact_Rules"); //save the classifier

    //DataMine dm4 =DataMineFile.setFilePath("c://a//contact2.txt");
    ///dm.rules.applyToDatamineAndSaveTo(dm,"c://a//ssss");

    dm.rules.applyToDataMineFileAndSaveTo("c://a//contact.txt","c://a//contact_Analysis");//Test file and save the results

}



}