package weka.classifiers.mcar;

import test.TestLog;
import weka.classifiers.Classifier;
import weka.classifiers.IntervalEstimator;
import weka.classifiers.functions.supportVector.Kernel;
import weka.classifiers.functions.supportVector.PolyKernel;
import weka.classifiers.functions.supportVector.RBFKernel;
import weka.core.Capabilities;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.Option;
import weka.core.OptionHandler;
import weka.core.SelectedTag;
import weka.core.Statistics;
import weka.core.Tag;
import weka.core.TechnicalInformation;
import weka.core.TechnicalInformationHandler;
import weka.core.Utils;
import weka.core.Capabilities.Capability;
import weka.core.TechnicalInformation.Field;
import weka.core.TechnicalInformation.Type;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.NominalToBinary;
import weka.filters.unsupervised.attribute.Normalize;
import weka.filters.unsupervised.attribute.ReplaceMissingValues;
import weka.filters.unsupervised.attribute.Standardize;

import java.io.IOException;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;
import org.apache.log4j.*;

import dm.Cv;
import dm.DataMine;

/**
 <!-- globalinfo-start -->
 * Implements MCAR Classifier algorithm
 * Suhel Hammoud (2007). 
 <!-- globalinfo-end -->
 *
 <!-- technical-bibtex-start -->
 * <p/>
 <!-- technical-bibtex-end -->
 *
 <!-- options-start -->
 * Valid options are: <p/>
 * 
 * <pre> -D
 *  If set, classifier is run in debug mode and
 *  may output additional info to the console</pre>
 * 
 * 
 * <pre> -S
 *  Support
 *  (default: 0.05)</pre>
 * 
 * <pre> -C 
 * 	Confidence
 *  (default: 0.4)</pre>
 * 
 * <pre> -I
 * Iteration
 * (default: 1)
 * </pre>
 * 
 * @author Suhel Hammoud (suheil77@gmail.com)
 * @version $Revision: 1.0 $
 */

public class MCA 
extends Classifier 
implements Serializable,OptionHandler, IntervalEstimator, TechnicalInformationHandler {

  static Logger log= Logger.getLogger(MCA.class);

  DataMine trainDatamine;
  DataMine testDatamine;
  Cv cv;//??
  double support=0.05;
  double confidence=0.40;
  int iteration=1;

  /** for serialization */
  static final long serialVersionUID = -8620066949967678545L;

  /** The filter used to make attributes numeric. */
  protected NominalToBinary m_NominalToBinary;


  /** no filter */
  public static final int FILTER_NONE = 0;
  /** The filter to apply to the training data */
  public static final Tag [] TAGS_FILTER = {

    new Tag(FILTER_NONE, "No normalization/standardization")
  };

  /** The filter used to standardize/normalize all values. */
  protected Filter m_Filter = null;

  /** Whether to normalize/standardize/neither */
  protected int m_filterType = FILTER_NONE;

  /** The filter used to get rid of missing values. */
  protected ReplaceMissingValues m_Missing;

  /** Turn off all checks and conversions? Turning them off assumes
      that data is purely numeric, doesn't contain any missing values,
      and has a numeric class. */
  protected boolean m_checksTurnedOff = false;


  protected double m_support=0.05;
  protected double m_confidence=0.4;
  protected int m_iter=1;

  /** The class index from the training data */
  protected int m_classIndex = -1;



  /** The number of training instances */
  protected int m_NumTrain = 0;

  /** The training data. */
  protected double m_avg_target;


  /**
   * the default constructor
   */
  public MCA() {
    super();


  }

  /**
   * Returns a string describing classifier
   * @return a description suitable for
   * displaying in the explorer/experimenter gui
   */
  public String globalInfo() {

    return  "Implements MCAR classifier\n\n  "
    + getTechnicalInformation().toString();
  }

  /**
   * Returns an instance of a TechnicalInformation object, containing 
   * detailed information about the technical background of this class,
   * e.g., paper reference or book this class is based on.
   * 
   * @return the technical information about this class
   */
  public TechnicalInformation getTechnicalInformation() {
    TechnicalInformation 	result;

    result = new TechnicalInformation(Type.MISC);
    result.setValue(Field.AUTHOR, "S. HAMMOUD & F. THABATAH");
    result.setValue(Field.YEAR, "2007");
    result.setValue(Field.TITLE, "Introduction to MCAR classifier");
    result.setValue(Field.ADDRESS, "Brunel University, UK");
    result.setValue(Field.PS, "http://mlcs.googlecode.com/svn/branches/weka-3.5.5");

    return result;
  }

  /**
   * Returns default capabilities of the classifier.
   *
   * @return      the capabilities of this classifier
   */
  @Override
  public Capabilities getCapabilities() {
    Capabilities result =new Capabilities(this);
    result.setOwner(this);

    // attribute
    result.enableAllAttributeDependencies();
    // with NominalToBinary we can also handle nominal attributes, but only
    // if the kernel can handle numeric attributes
    if (result.handles(Capability.NUMERIC_ATTRIBUTES))
      result.enable(Capability.NOMINAL_ATTRIBUTES);
    result.enable(Capability.MISSING_VALUES);

    // class
    result.disableAllClasses();
    result.disableAllClassDependencies();
    result.enable(Capability.NUMERIC_CLASS);
    result.enable(Capability.DATE_CLASS);
    result.enable(Capability.MISSING_CLASS_VALUES);

    return result;
  }

  /**
   * Method for building the classifier. 
   *
   * @param insts the set of training instances
   * @throws Exception if the classifier can't be built successfully
   */
  @Override
  public void buildClassifier(Instances insts) throws Exception {

    try{
      trainDatamine= new DataMine(insts);
     // trainDatamine.iterate2(0.0,confidence,support,iteration);
      trainDatamine.iterate(0.0,confidence,support,2);
      log.info("\n--->Classifier has been buit successfully");
    }catch (IOException ioe){}


  }

  /**
   * Classifies a given instance.
   *
   * @param inst the instance to be classified
   * @return the classification
   * @throws Exception if instance could not be classified
   * successfully
   */
  @Override
  public double classifyInstance(Instance inst) throws Exception {

   
    double result =  trainDatamine.rules.classifyInstance(inst);
    
    return result;

  }

  /**
   * Predicts a confidence interval for the given instance and confidence level.
   *
   * @param inst the instance to make the prediction for
   * @param confidenceLevel the percentage of cases the interval should cover
   * @return a 1*2 array that contains the boundaries of the interval
   * @throws Exception if interval could not be estimated
   * successfully
   */
  public double[][] predictInterval(Instance inst, double confidenceLevel) throws Exception {

    return new double[1][1];

  }

  /**
   * Gives the variance of the prediction at the given instance
   *
   * @param inst the instance to get the variance for
   * @return tha variance
   * @throws Exception if computation fails
   */


  /**
   * Returns an enumeration describing the available options.
   *
   * @return an enumeration of all the available options.
   */
  @Override
  public Enumeration listOptions() {

    Vector result = new Vector();

    Enumeration enm = super.listOptions();
    while (enm.hasMoreElements())
      result.addElement(enm.nextElement());

    result.addElement(new Option(
	"\tLevel of Gaussian Noise.\n"
	+ "\t(default: 1.0)",
	"L", 1, "-L <double>"));

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



  @Override
  public void setOptions(String[] options) throws Exception {
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

  /**
   * Gets the current settings of the classifier.
   *
   * @return an array of strings suitable for passing to setOptions
   */
  @Override
  public String[] getOptions() {
    int       i;
    Vector    result;
    String[]  options;

    result = new Vector();
//  options = super.getOptions();
//  for (i = 0; i < options.length; i++)
//  result.add(options[i]);

    result.add("-S");
    result.add("" + getM_support());

    result.add("-C");
    result.add("" + m_confidence);

    result.add("-I");
    result.add("" + m_iter);

    return (String[]) result.toArray(new String[result.size()]);	  
  }

  /**
   * Returns the tip text for this property
   * 
   * @return 		tip text for this property suitable for
   * 			displaying in the explorer/experimenter gui
   */
  public String kernelTipText() {
    return "The kernel to use.";
  }



  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String filterTypeTipText() {
    return "Determines how/if the data will be transformed.";
  }

  /**
   * Gets how the training data will be transformed. Will be one of
   * FILTER_NORMALIZE, FILTER_STANDARDIZE, FILTER_NONE.2200Instances
   *
   * @return the filtering mode
   */
  public SelectedTag getFilterType() {

    return new SelectedTag(m_filterType, TAGS_FILTER);
  }

  /**
   * Sets how the training data will be transformed. Should be one of
   * FILTER_NORMALIZE, FILTER_STANDARDIZE, FILTER_NONE.
   *
   * @param newType the new filtering mode
   */
  public void setFilterType(SelectedTag newType) {

    if (newType.getTags() == TAGS_FILTER) {
      m_filterType = newType.getSelectedTag().getID();
    }
  }

  /**
   * Returns the tip text for this property
   * @return tip text for this property suitable for
   * displaying in the explorer/experimenter gui
   */
  public String noiseTipText() {
    return "The level of Gaussian Noise (added to the diagonal of the Covariance Matrix).";
  }





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



  /**
   * Prints out the classifier.
   *
   * @return a description of the classifier as a string
   */
  @Override
  public String toString() {

    
    return trainDatamine.rules.getClassifier().toString();
  }

  /**
   * Main method for testing this class.
   * 
   * @param argv the commandline parameters
   */
  public static void main(String[] argv) {
    MCA mcar=new MCA();
    mcar.
    runClassifier(new MCA(), argv);
  }

  public int getM_iter() {
    return m_iter;
  }

  public void setM_iter(int m_iter) {
    this.m_iter = m_iter;
  }
}
