package weka.classifiers.mcar;


import java.util.*;

import weka.core.*;
import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Option;
import weka.core.OptionHandler;
import weka.core.TechnicalInformation;
import weka.core.TechnicalInformationHandler;
import weka.core.Utils;
import weka.core.TechnicalInformation.Field;
import weka.core.TechnicalInformation.Type;

import org.apache.log4j.*;

import com.sun.corba.se.spi.orbutil.fsm.Guard.Result;

public class MCCA extends Classifier implements OptionHandler,
    TechnicalInformationHandler {
  
  static Logger log=Logger.getLogger(MCCA.class);
  static final long serialVersionUID = -862006694996767545L;
  
  protected double m_support=0.05;
  protected double m_confidence=0.4;
  protected int m_iter=1;

  /** The class index from the training data */
  protected int m_classIndex = -1;
  
  public double getM_support() {
    return m_support;
  }

  public void setM_support(double m_support) {
    this.m_support = m_support;
  }

  public double getM_confidence() {
    return m_confidence;
  }

  public void setM_confidence(double m_confidence) {
    this.m_confidence = m_confidence;
  }

  public int getM_iter() {
    return m_iter;
  }

  public void setM_iter(int m_iter) {
    this.m_iter = m_iter;
  }

  public int getM_classIndex() {
    return m_classIndex;
  }

  public void setM_classIndex(int index) {
    m_classIndex = index;
  }

  public MCCA() {
    // TODO Auto-generated constructor stub
  }

  @Override
  public void buildClassifier(Instances data) throws Exception {
    // TODO Auto-generated method stub
    
  }
  
  private Map<String,Instances> splitData(Instances data){
    HashMap<String,Set<Integer>> resutl=new HashMap<String, Set<Integer>>();
    //HashMap<String,Instances> resutl=new HashMap<String, Instances>();
    
    Attribute classAtt=data.classAttribute();
    int classIndex=data.classIndex();
    
    Enumeration<Instance>ins=data.enumerateInstances();
    while (ins.hasMoreElements()) {
      Instance inselem = (Instance) ins.nextElement();
      String classValue=inselem.stringValue(classIndex);
      Set ts=resutl.get(classValue);
//      if(ts==null){
//	ts=new HashSet<Integer>();
//	inselem.
//	ts.add(inselem.index(position)())
//	resutl.put(classValue, new HashSet<Integer>())
//      }
//      inselem.
//      
    }
    return null;
  }

  public TechnicalInformation getTechnicalInformation() {
    // TODO Auto-generated method stub
    TechnicalInformation 	result;

    result = new TechnicalInformation(Type.MISC);
    result.setValue(Field.AUTHOR, "S. HAMMOUD & F. THABATAH");
    result.setValue(Field.YEAR, "2007");
    result.setValue(Field.TITLE, "Introduction to MCCA classifier");
    result.setValue(Field.ADDRESS, "Brunel University, UK");
    result.setValue(Field.PS, "http://mlcs.googlecode.com/svn/branches/weka-3.5.5");

    return result;
  }

  @Override
  public Enumeration listOptions() {
    // TODO Auto-generated method stub
    Vector result = new Vector();

    Enumeration enm = super.listOptions();
    while (enm.hasMoreElements())
      result.addElement(enm.nextElement());


   result.addElement(new Option(
	"\tSupport value\n"
	+ "\t(default: 0.05 )",
	"SUPP", 1, "-SUPP"));

   result.addElement(new Option(
	"\tConfidence\n"
	+ "\t(default: 0.4)",
	"CONF", 1, "-CONF"));



   return result.elements();
     
  }

  /**
   * @param args
   */
  public static void main(String[] args) {
    // TODO Auto-generated method stub

  }

  @Override
  protected Object clone() throws CloneNotSupportedException {
    // TODO Auto-generated method stub
    return super.clone();
  }

  @Override
  public String[] getOptions() {
    // TODO Auto-generated method stub
    return super.getOptions();
  }

  @Override
  public void setOptions(String[] options) throws Exception {
    // TODO Auto-generated method stub
    String	tmpStr;
    String[]	tmpOptions;

    

    tmpStr = Utils.getOption('S', options);
    if (tmpStr.length() != 0)
      setM_support(Double.parseDouble(tmpStr));
    else
      setM_support(0.05);

    tmpStr = Utils.getOption('C', options);
    if (tmpStr.length() != 0)
      setM_confidence(Double.parseDouble(tmpStr));
    else
      setM_confidence(0.40);

    tmpStr = Utils.getOption('I', options);
    if (tmpStr.length() != 0)
      setM_iter(Integer.parseInt(tmpStr));
    else
      setM_iter(1);

   

    super.setOptions(options);
  }

}
