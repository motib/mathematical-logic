package dm;
import java.util.*;
import java.io.*;
import javax.swing.*;
/**
 * <p>Title: MCAR</p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author Fadi & Suhel
 * @version 1.2
 */

public class FileResort {
      ArrayList result=new ArrayList();


  public FileResort() {
  }

  List dataFileRead(String PInputFileName){
      result.clear();
      String row=new String();
      int rowIndex=0;
      try{
        BufferedReader in= new BufferedReader(new FileReader(PInputFileName));

        row=in.readLine();
    	while(row!=null){
          result.add(row);
          row=in.readLine();
        }
        in.close();
      }catch (IOException ioe){}
      return result;
  }

  void dataFileWrite(String outFileName,int startRow,int endRow,String addition){
    try{
      BufferedWriter out=new BufferedWriter(new FileWriter(outFileName));
      if(endRow==-1) endRow=result.size();
      for(int i=startRow; i<=endRow;i++){
        out.write((String)result.get(i-1)+addition+"\n");
      }
    }catch (IOException ioe){}
  }

  void shuffle(int startRow,int endRow){
    ArrayList newResult=new ArrayList();
    for(int j=0; j<startRow; j++){
      newResult.add((String)result.get(j));
    }
    if(endRow==-1) endRow=result.size();
    int[] randIndex=Tools.getRandomSamp(endRow-startRow,endRow-startRow);
    for(int i=0; i<randIndex.length; i++){
      newResult.add((String)result.get(startRow-1+randIndex[i]));
    }
    for(int k=endRow; k<result.size(); k++){
      newResult.add((String)result.get(k));
    }
    result=newResult;
  }

  public static  void main(String[] args){
    FileResort fr=new FileResort();
    String inputFile=new String("c://a//116.txt");
    String outFile=new String("c://a//new_116.txt");
    int startLine=7;
    int endLine=-1;
    fr.dataFileRead(inputFile);
    fr.dataFileWrite(outFile,startLine,endLine,".");
  }
}