
//comment 1
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

public class Cv {
  String fileName="";
  DataMine dm;
  DataMine dmTrn;
  DataMine dmTst;
  double support=0.0;
  double confidence=0.0;
  int iteration=0;
  final Set orgLines;
  public Cv(DataMine PDm){
    dm=PDm;
    orgLines=dm.entity.keySet();
  }
  public Cv(String PFileName) {
    fileName=PFileName;
    dm=new DataMine(PFileName);
    orgLines=dm.entity.keySet();
  }
  void setSuppConIter(double PSupport,double PConfidence,int PIteration){
    support=PSupport;
    confidence=PConfidence;
    iteration=PIteration;
  }
  Set getPortionTrn(double trnRate){
    Set resultSet=new HashSet();
    Iterator  iter=dm.classCol.items.entrySet().iterator();
    while(iter.hasNext()){
      Map.Entry e=(Map.Entry)iter.next();
      Set ts=(Set)e.getValue();
      int minLines=(int)Math.round(trnRate*ts.size()-0.5);
      if(minLines==0)minLines=1;
      Integer[] lns=new Integer[ts.size()];
      int dex=0;
      Iterator iter2=ts.iterator();
      while(iter2.hasNext()){
        lns[dex]=(Integer)iter2.next();
        dex++;
      }
      int[] linesIndexes=Tools.getRandomSamp(minLines,ts.size());
      for(int i=0; i<linesIndexes.length; i++){
        resultSet.add(lns[linesIndexes[i]]);
      }
    }

    return resultSet;
  }

  Set getRemainTst(Set trnSet){
    dm.allLines.removeAll(trnSet);
    return new HashSet(dm.allLines);
  }

  void setLines( Set PLines){
    dm.allLines=PLines;
  }

  void setDataMineLines(Set lines){

  }

  double[] getCvAccuracy(double PTrnRate,int repeat) throws IOException{
    double[] totalAccuracy=new double[5];
    for(int j=0; j<repeat; j++){
      dm.resetLines();
      dmTrn=new DataMine(fileName);
      dmTst=new DataMine(fileName);
      Set trainLines=getPortionTrn(PTrnRate);
      Set testLines=getRemainTst(trainLines);
//      System.out.print("\ntrainLine"+trainLines);
//      System.out.print("\ntestLine"+testLines);
      dmTrn.delLines(testLines);
      dmTst.delLines(trainLines);
//      JOptionPane.showMessageDialog(null,"\nbefore iterate , supp, conf ,iter,"+support+"\t"+confidence+"\t"+iteration+"\t");
      dmTrn.rules.iterate2(0.0,confidence,support*PTrnRate,iteration);
      dmTrn.rules.applyToDatamineAndSaveToOld(dmTst,"");
      double[] acc=dmTrn.rules.getAccuracy(dmTst);
      System.out.print("\n inside iteration Accuracy :"+ j+" :\t");
      for(int i=0; i<5; i++){
        System.out.print(""+acc[i]+"\t");
        totalAccuracy[i]+=acc[i];
      }
    }
    for(int k=0; k<5; k++){
        totalAccuracy[k]=(double)totalAccuracy[k]/(double)repeat;
    }
    return totalAccuracy;
  }
  public StringBuffer getLastClassifier()throws IOException{
    return dmTrn.rules.getClassifier();
  }
//////////new code for cv


  Set getPortionTrnNew(double trnRate,Set orgS){
    double correctRate=(double)orgLines.size()/(double)orgS.size();
    Set resultSet=new HashSet();
    Iterator  iter=dm.classCol.items.entrySet().iterator();
    while(iter.hasNext()){
      Map.Entry e=(Map.Entry)iter.next();
      Set ts=(Set)e.getValue();
//      orgS.removeAll(ts);//
      int minLines=(int)Math.round(trnRate*ts.size());//- 0.5*Math.random());
      ts.retainAll(orgS);
      if(minLines < 0)minLines=0;
      Integer[] lns=new Integer[ts.size()];
      int dex=0;
      Iterator iter2=ts.iterator();
      while(iter2.hasNext()){
        lns[dex]=(Integer)iter2.next();
        dex++;
      }
      if(minLines<ts.size()){
        int[] linesIndexes=Tools.getRandomSamp(minLines,ts.size());
        for(int i=0; i<linesIndexes.length; i++){
         resultSet.add(lns[linesIndexes[i]]);
        }
      }else{
        resultSet.addAll(ts);
      }
    }
    return resultSet;
  }

////////

  double[] getCvAccuracyNew(int numOfFolds,int repeat) throws IOException{
      Set remainLines;
      dm.resetLines();
      dmTrn=new DataMine(fileName);
      dmTst=new DataMine(fileName);
    ArrayList folds=new ArrayList(numOfFolds+10);
    final double PTstRate=(double)1.0/(double)numOfFolds;
    System.out.println("\n RATE inside getCvAccuracyNew IS "+PTstRate);

    double[] totalAccuracy=new double[5];
    for(int j=0; j<repeat; j++){
      remainLines=new HashSet(orgLines);
      folds.clear();
      System.out.println("\n remainLines iterate "+(j+1)+"= "+remainLines);
      for(int i=0; i<numOfFolds-1; i++){
        Set ts2=getPortionTrnNew(PTstRate,remainLines);
        remainLines.removeAll(ts2);
        folds.add(ts2);
        System.out.println("\n Test part "+(i+1)+"= "+ts2);
        System.out.println("\n remain part "+(i+1)+"= "+remainLines);
      }
      folds.add(remainLines);

      for(int k=0; k<numOfFolds; k++){
        dm.resetLines();
        Set testLines=(Set)folds.get(k);
        Set trainLines=new HashSet(orgLines);
        trainLines.removeAll(testLines);
//        System.out.print("\ntrainLine"+trainLines);
//        System.out.print("\ntestLine"+testLines);
//        dmTrn.setLines(testLines);////to be returned
//      dmTst.setLines(trainLines);////to be returned
/////sss
      dmTrn=new DataMine(fileName);
      dmTst=new DataMine(fileName);
      dmTrn.delLines(testLines);
      dmTst.delLines(trainLines);
/////sss
  //      JOptionPane.showMessageDialog(null,"\nbefore iterate , supp, conf ,iter,"+support+"\t"+confidence+"\t"+iteration+"\t");
        dmTrn.rules.iterate2(0.0,confidence,support*(1.0-PTstRate),iteration);
        dmTrn.rules.applyToDatamineAndSaveToOld(dmTst,"");
        double[] acc=dmTrn.rules.getAccuracy(dmTst);
        System.out.print("\n inside iteration Accuracy :"+ j+" :\t");
        for(int i=0; i<5; i++){
          System.out.print(""+acc[i]+"\t");
          totalAccuracy[i]+=acc[i];
        }

      }
    }
    for(int k=0; k<5; k++){
        totalAccuracy[k]=(double)totalAccuracy[k]/((double)repeat*(double)numOfFolds);
    }
    return totalAccuracy;
  }

}