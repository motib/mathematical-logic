package dm;
import javax.swing.*;
/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class Rule {
  private String ruleId;
  String[] cols;
  int numOfClasses=1;
  String[] clss;
  int[] clssOcc;
  int allOcc;

  public Rule(int numOfCols2,
              String[] cols2,
              int allOcc2,
              int numOfClasses2,
              String[] clss2,
              int[] clssOcc2){
    cols=new String[numOfCols2];
    clss=new String[100];
    clssOcc=new int[100];
    String s="";
    for(int i=0; i<numOfCols2; i++){
      cols[i]=cols2[i];
      s+=cols2[i]+"|";
    }
    allOcc=allOcc2;
    numOfClasses=numOfClasses2;
    for(int i=0; i<numOfClasses2; i++){
      clss[i]=clss2[i];
      clssOcc[i]=clssOcc2[i];
    }
//    System.out.println(s+"numOfClasses "+numOfClasses+"clss "+clss[0]+" occ"+clssOcc[0] );
    ruleId=s;
  }

  public String getId(){
    return ruleId;
  }
  //just if the Id are identical
  public boolean updateRule(Rule rl2){
    if (!this.getId().equals(rl2.getId())){
      JOptionPane.showMessageDialog(null, "error in the ruleId");
      return false;}
      clss[numOfClasses]=rl2.clss[0];
      clssOcc[numOfClasses]=rl2.clssOcc[0];
    for(int i=0; i<rl2.numOfClasses;i++){
      clss[i+numOfClasses]=rl2.clss[i];
      clssOcc[i+numOfClasses]=rl2.clssOcc[i];
    }
    allOcc+=rl2.allOcc;
    numOfClasses+=rl2.numOfClasses;

    return true;
  }
}