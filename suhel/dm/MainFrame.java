//Counter added here
package dm;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.beans.*;
import java.io.*;
import javax.swing.event.*;
/**
 * <p>Title: MCAR</p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author Fadi & Suhel
 * @version 1.2
 */

public class MainFrame extends JFrame {

  DataMine trainDatamine;
  DataMine testDatamine;
  Cv cv;
  double support=0.05;
  double confidence=0.40;
  int iteration=1;
  String trainDatamineFile="notChoosedYet";
  String testDatamineFile="notChoosedYet";
  String classifierFile="notChoosedYet";
  String applyResultsFile="notChoosedYet";

  JPanel contentPane;
  BorderLayout borderLayout1 = new BorderLayout();
  JMenuBar MenuBar = new JMenuBar();
  JMenu jMenu1 = new JMenu();
  JMenuItem jMenuItem1 = new JMenuItem();
  JMenuItem jMenuItem2 = new JMenuItem();
  JMenuItem jMenuItem3 = new JMenuItem();
  JToolBar jToolBar1 = new JToolBar();
  JTextField txtIteration = new JTextField();
  JTextField txtconfidence = new JTextField();
  JTextField txtSupport = new JTextField();
  JButton btnGenerate = new JButton();
  JLabel jLabel1 = new JLabel();
  JLabel jLabel2 = new JLabel();
  JLabel jLabel3 = new JLabel();
  JScrollPane jScrollPane1 = new JScrollPane();
  JTextArea txtAreaMain = new JTextArea();
  JToolBar jToolBar2 = new JToolBar();
  JLabel jLabel4 = new JLabel();
  JTextField txtTrainFile = new JTextField();
  JLabel jLabel5 = new JLabel();
  JTextField txtTestFile = new JTextField();
  JTextField txtSaveTo = new JTextField();
  JButton btnSaveTo = new JButton();
  JMenu jMenu3 = new JMenu();
  JMenuItem jMenuItem7 = new JMenuItem();
  JMenuItem jMenuItem8 = new JMenuItem();
  JMenu jMenu4 = new JMenu();
  JMenuItem jMenuItem12 = new JMenuItem();
  JMenuItem jMenuItem10 = new JMenuItem();
  JMenuItem jMenuItem13 = new JMenuItem();
  JMenuItem jMenuItem9 = new JMenuItem();
  JMenu jMenu2 = new JMenu();
  JMenuItem jMenuItem6 = new JMenuItem();
  JMenuItem jMenuItem11 = new JMenuItem();
  JMenuItem jMenuItem14 = new JMenuItem();
  JMenuItem jMenuItem15 = new JMenuItem();
  JMenuItem sss = new JMenuItem();
  JMenu jMenu5 = new JMenu();
  JMenuItem jMenuItem16 = new JMenuItem();
  JMenuItem jMenuItem17 = new JMenuItem();
  JMenuItem jMenuItem18 = new JMenuItem();
  JMenuItem jMenuItem19 = new JMenuItem();
  JMenuItem jMenuItem5 = new JMenuItem();
  JMenuItem jMenuItem20 = new JMenuItem();
  JMenuItem menuItemPrintTrain = new JMenuItem();
  JMenuItem menuItemPrintTest = new JMenuItem();
  JMenuItem MenuItemPrintRuleDm = new JMenuItem();
  JMenuItem jMenuItem4 = new JMenuItem();
  JMenuItem jMenuItem21 = new JMenuItem();
  JMenu jMenuCv = new JMenu();
  JMenuItem jMenuItemNewCv = new JMenuItem();
  JMenuItem jMenuItem23 = new JMenuItem();
  JMenu jMenueCounter = new JMenu();
  JMenuItem jMenuResetCounters = new JMenuItem();
  JMenu jMenu6 = new JMenu();
  JMenuItem jMenuItem22 = new JMenuItem();
  JMenuItem jMenuItem24 = new JMenuItem();
  JMenuItem jMenuItem25 = new JMenuItem();
  //Construct the frame
  public MainFrame() {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try {
      jbInit();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
  //Component initialization
  private void jbInit() throws Exception  {
    //setIconImage(Toolkit.getDefaultToolkit().createImage(MainFrame.class.getResource("[Your Icon]")));
    contentPane = (JPanel) this.getContentPane();
    contentPane.setLayout(borderLayout1);
    this.setJMenuBar(MenuBar);
    this.setSize(new Dimension(400, 300));
    this.setTitle("MMAC Algorithm");
    this.addWindowListener(new java.awt.event.WindowAdapter() {
      public void windowOpened(WindowEvent e) {
        this_windowOpened(e);
      }
    });
    jMenu1.setText("File");
    jMenuItem1.setText("Open");
    jMenuItem1.addAncestorListener(new javax.swing.event.AncestorListener() {
      public void ancestorAdded(AncestorEvent e) {
        jMenuItem1_ancestorAdded(e);
      }
      public void ancestorRemoved(AncestorEvent e) {
      }
      public void ancestorMoved(AncestorEvent e) {
      }
    });
    jMenuItem2.setText("Save");
    jMenuItem3.setText("Exit");
    jMenuItem3.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem3_actionPerformed(e);
      }
    });
    btnGenerate.setPreferredSize(new Dimension(85, 27));
    btnGenerate.setText("Genarate");
    btnGenerate.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        btnGenerate_actionPerformed(e);
      }
    });
    jLabel1.setText("Support");
    jToolBar1.setToolTipText("");
    jToolBar1.setMargin(new Insets(5, 5, 5, 5));
    jLabel2.setText("Confidence");
    jLabel3.setText("Iteration");
    txtAreaMain.setText("hi there ");
    jLabel4.setText("Training");
    jLabel5.setText("Test");
    txtSaveTo.setText("c:/a/");
    btnSaveTo.setText("SaveTo");
    btnSaveTo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        btnSaveTo_actionPerformed(e);
      }
    });
    txtSupport.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusLost(FocusEvent e) {
        txtSupport_focusLost(e);
      }
    });
    txtconfidence.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        txtconfidence_actionPerformed(e);
      }
    });
    txtIteration.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusLost(FocusEvent e) {
        txtIteration_focusLost(e);
      }
    });
    txtIteration.setPreferredSize(new Dimension(1, 21));
    txtIteration.setToolTipText("");
    txtIteration.setText("1");
    jMenu3.setText("Datamine");
    jMenuItem7.setText("New Training Data");
    jMenuItem7.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem7_actionPerformed(e);
      }
    });
    jMenu4.setText("Rules");
    jMenuItem8.setText("Print Rules In Columns");
    jMenuItem8.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem8_actionPerformed(e);
      }
    });
    jMenuItem12.setText("Print Ranked Classifier");
    jMenuItem12.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem12_actionPerformed(e);
      }
    });
    jMenuItem10.setText("Save Classifier");
    jMenuItem10.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem10_actionPerformed(e);
      }
    });
    jMenuItem13.setText("New Test Data");
    jMenuItem13.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem13_actionPerformed(e);
      }
    });
    jMenuItem9.setText("Build Classifier");
    jMenuItem9.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem9_actionPerformed(e);
      }
    });
    jMenu2.setText("Screen");
    jMenuItem11.setText("Clear");
    jMenuItem11.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem11_actionPerformed(e);
      }
    });
    jMenuItem14.setText("Change Parameters");
    jMenuItem15.setText("Predicted Classes");
    jMenuItem15.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem15_actionPerformed(e);
      }
    });
    sss.setText("Save  Predicted Classes");
    sss.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        sss_actionPerformed(e);
      }
    });
    jMenu5.setText("Help");
    jMenuItem16.setText("About");
    jMenuItem17.setText("Help Topics");
    jMenuItem18.setText("Print Classifier");
    jMenuItem18.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem18_actionPerformed(e);
      }
    });
    jMenuItem19.setText("Save Predicted Class 2");
    jMenuItem19.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem19_actionPerformed(e);
      }
    });
    menuItemPrintTrain.setText("Print Training");
    menuItemPrintTrain.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        menuItemPrintTrain_actionPerformed(e);
      }
    });
    menuItemPrintTest.setText("Print Test");
    menuItemPrintTest.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        menuItemPrintTest_actionPerformed(e);
      }
    });
    MenuItemPrintRuleDm.setText("Print Rule DataMine");
    MenuItemPrintRuleDm.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        MenuItemPrintRuleDm_actionPerformed(e);
      }
    });
    txtconfidence.addFocusListener(new java.awt.event.FocusAdapter() {
      public void focusLost(FocusEvent e) {
        txtconfidence_focusLost(e);
      }
    });
    jMenuItem20.setText("Save predicted Class Old");
    jMenuItem20.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem20_actionPerformed(e);
      }
    });
    jMenuItem4.setText("Cross Validation");
    jMenuItem4.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem4_actionPerformed(e);
      }
    });
    jMenuItem21.setText("CV Classifier");
    jMenuCv.setText("CV");
    jMenuItemNewCv.setText("new Cv");
    jMenuItemNewCv.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItemNewCv_actionPerformed(e);
      }
    });
    jMenueCounter.setText("Counters");
    jMenuResetCounters.setText("Reset Counters");

    jMenu6.setText("Counters");
    jMenuItem24.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem24_actionPerformed(e);
      }
    });
    jMenuItem22.setText("Print Counters Sizes");
    jMenuItem22.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem22_actionPerformed(e);
      }
    });
    jMenuItem25.setText("Print Counters Details");
    jMenuItem25.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jMenuItem25_actionPerformed(e);
      }
    });
    jMenuItem24.setText("Reset Counters");
    MenuBar.add(jMenu1);
    MenuBar.add(jMenu3);
    MenuBar.add(jMenu4);
    MenuBar.add(jMenu2);
    MenuBar.add(jMenuCv);
    MenuBar.add(jMenu6);
    MenuBar.add(jMenu6);
    MenuBar.add(jMenu5);
    jMenu1.add(jMenuItem1);
    jMenu1.add(jMenuItem2);
    jMenu1.add(jMenuItem3);
    jToolBar1.add(btnGenerate, null);
    jToolBar1.add(jLabel1, null);
    jToolBar1.add(txtSupport, null);
    jToolBar1.add(jLabel2, null);
    jToolBar1.add(txtconfidence, null);
    jToolBar1.add(jLabel3, null);
    jToolBar1.add(txtIteration, null);
    contentPane.add(jScrollPane1, BorderLayout.CENTER);
    contentPane.add(jToolBar2, BorderLayout.SOUTH);
    jToolBar2.add(jLabel4, null);
    jToolBar2.add(txtTrainFile, null);
    jToolBar2.add(jLabel5, null);
    jToolBar2.add(txtTestFile, null);
    jToolBar2.add(btnSaveTo, null);
    jToolBar2.add(txtSaveTo, null);
    contentPane.add(jToolBar1, BorderLayout.NORTH);
    jScrollPane1.getViewport().add(txtAreaMain, null);
    jMenu3.add(jMenuItem7);
    jMenu3.add(jMenuItem13);
    jMenu3.addSeparator();
    jMenu3.add(jMenuItem14);
    jMenu3.add(menuItemPrintTrain);
    jMenu3.add(menuItemPrintTest);
    jMenu4.add(jMenuItem18);
    jMenu4.add(jMenuItem10);
    jMenu4.add(jMenuItem9);
    jMenu4.add(jMenuItem8);
    jMenu4.add(jMenuItem12);
    jMenu4.addSeparator();
    jMenu4.add(jMenuItem15);
    jMenu4.add(MenuItemPrintRuleDm);
    jMenu4.add(sss);
    jMenu4.add(jMenuItem19);
    jMenu4.add(jMenuItem20);
    jMenu2.add(jMenuItem11);
    jMenu2.add(jMenuItem6);
    jMenu5.add(jMenuItem17);
    jMenu5.add(jMenuItem16);
    jMenu3.add(jMenuItem4);
    jMenu3.add(jMenuItem21);
    jMenuCv.add(jMenuItemNewCv);
    jMenuCv.add(jMenuItem23);
    jMenueCounter.add(jMenuItem22);
    jMenu6.add(jMenuItem24);
    jMenu6.add(jMenuItem22);
    jMenu6.add(jMenuItem25);
  }
  //Overridden so we can exit when window is closed
  protected void processWindowEvent(WindowEvent e) {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING) {
      System.exit(0);
    }
  }

  void btnGenerate_actionPerformed(ActionEvent e) {
    try{
      trainDatamine= new DataMine(trainDatamineFile);
      JOptionPane.showMessageDialog( null,"iterate(0.0,"+
                "\nconfidence= "+confidence+","+
                "\nsupport= "+support+","+
                "\niteration= "+iteration+");");
      trainDatamine.iterate2(0.0,confidence,support,iteration);
    }catch (IOException ioe){

     // JOptionPane.showMessageDialog(null,ioe);
     // JOptionPane.showMessageDialog(null,"TrainFile "+txtTrainFile.getText()+".txt");
    }

  }

  void jMenuItem6_actionPerformed(ActionEvent e) {
    txtAreaMain.setText("");
  }

  void jMenuItem1_actionPerformed(ActionEvent e) {
    String fName="";
    JOptionPane.showMessageDialog(null,"you choosed the file"+fName);
  }


  void txtSupport_focusLost(FocusEvent e) {
    support=Double.parseDouble(txtSupport.getText());
  }

  void txtconfidence_actionPerformed(ActionEvent e) {
    confidence=Double.parseDouble(txtconfidence.getText());

  }

  void txtIteration_focusLost(FocusEvent e) {
    iteration=Integer.parseInt(txtIteration.getText());
  }

  void btnSaveTo_actionPerformed(ActionEvent e) {
    try{
      BufferedWriter out2= new BufferedWriter(new FileWriter(txtSaveTo.getText()));
      out2.write(txtAreaMain.getText());
      out2.close();
    }catch (IOException ioe){

    }

  }

  void menuItemPrintTest_actionPerformed(ActionEvent e) {
    txtAreaMain.append("\n--->Test Dataset:");
    txtAreaMain.append(testDatamine.printDataMine());
  }

  void jMenuItem1_ancestorAdded(AncestorEvent e) {

  }

  void jMenuItem7_actionPerformed(ActionEvent e) {
    Chooser chooser=new Chooser(this);
    if(!chooser.isFileSelected("open"))return;
    trainDatamineFile=chooser.getFileName();
    trainDatamine= new DataMine(trainDatamineFile);
    txtTrainFile.setText(trainDatamineFile);

  }

  void jMenuItem13_actionPerformed(ActionEvent e) {
    Chooser chooser=new Chooser(this);
    if(!chooser.isFileSelected("open"))return;
    testDatamineFile=chooser.getFileName();
    testDatamine= new DataMine(testDatamineFile);
    txtTestFile.setText(testDatamineFile);

  }

  void jMenuItem9_actionPerformed(ActionEvent e) {
    try{
      trainDatamine= new DataMine(trainDatamineFile);
      trainDatamine.iterate(0.0,confidence,support,iteration);

       txtAreaMain.append("\n--->Classifier has been buit successfully");
   }catch (IOException ioe){}
  }

  void jMenuItem12_actionPerformed(ActionEvent e) {
//    trainDatamine.printConfidences(support,confidence);
    txtAreaMain.append( trainDatamine.rules.printRankedRuls());
  }

  void jMenuItem10_actionPerformed(ActionEvent e) {
    try{
    Chooser chooser=new Chooser(this);
    if(!chooser.isFileSelected("Save Classifier "))return;
    trainDatamine.rules.SaveToFile(chooser.getFileName());
    }catch (IOException ioe){}

  }

  void sss_actionPerformed(ActionEvent e) {
    try{
//      trainDatamine.printConfidences(support,confidence);
      Chooser chooser=new Chooser(this);
      if(!chooser.isFileSelected("Predict"))return;
      trainDatamine.rules.applyToDatamineAndSaveTo(testDatamine,chooser.getFileName());
      trainDatamine.rules.saveWithPrediction(chooser.getFileName());
    }catch (IOException ioe){}

  }

  void jMenuItem15_actionPerformed(ActionEvent e) {
  try{
//    trainDatamine.printConfidences(support,confidence);
    StringBuffer sb=trainDatamine.rules.applyToDatamine(testDatamine);
    txtAreaMain.append(sb.toString());
  }catch (IOException ioe){}
  }

  void jMenuItem11_actionPerformed(ActionEvent e) {
    txtAreaMain.setText("");
  }

  void jMenuItem8_actionPerformed(ActionEvent e) {
    txtAreaMain.append(trainDatamine.printSupports(support,confidence).toString());
  }

  void jMenuItem18_actionPerformed(ActionEvent e) {
    txtAreaMain.append(trainDatamine.rules.getClassifier().toString());
  }

  void jMenuItem19_actionPerformed(ActionEvent e) {
    try{
//      trainDatamine.printConfidences(support,confidence);
      Chooser chooser=new Chooser(this);
      if(!chooser.isFileSelected("Predict2"))return;
      trainDatamine.rules.applyToDataMineFileAndSaveTo2(txtTestFile.getText(),chooser.getFileName());
      trainDatamine.rules.saveWithPrediction(chooser.getFileName());

    }catch (IOException ioe){}

  }

  void jMenuItem3_actionPerformed(ActionEvent e) {
    System.exit(0);
  }

  void this_windowOpened(WindowEvent e) {
    txtSupport.setText(String.valueOf(support));
    txtconfidence.setText(String.valueOf(confidence));
    txtIteration.setText(String.valueOf(iteration));
  }

  void menuItemPrintTrain_actionPerformed(ActionEvent e) {
    txtAreaMain.append("\n--->Trining Dataset:");
    txtAreaMain.append(trainDatamine.printDataMine());

  }

  void MenuItemPrintRuleDm_actionPerformed(ActionEvent e) {
    txtAreaMain.append("\n--->Rules DataMin Dataset:");
 //   txtAreaMain.append(trainDatamine.rules.rulesDataMine.printDataMine());

  }

  void txtconfidence_focusLost(FocusEvent e) {
    confidence=Double.parseDouble(txtconfidence.getText());
  }

  void jMenuItem20_actionPerformed(ActionEvent e) {
   try{
//      trainDatamine.printConfidences(support,confidence);
      Chooser chooser=new Chooser(this);
      if(!chooser.isFileSelected("PredictOld"))return;
      trainDatamine.rules.applyToDataMineFileAndSaveToOld(txtTestFile.getText(),chooser.getFileName());
      trainDatamine.rules.saveWithPrediction(chooser.getFileName());
    }catch (IOException ioe){}

  }

  void jMenuItem4_actionPerformed(ActionEvent e) {
  try{
    cv=new Cv(trainDatamineFile);
    cv.setSuppConIter(support,confidence,iteration);
    double rate=Double.parseDouble(JOptionPane.showInputDialog("Enter the Rate of Train Partition"));
    int repeat=Integer.parseInt(JOptionPane.showInputDialog("repeat?"));
    double[] acc=cv.getCvAccuracy(rate,repeat);
    txtAreaMain.append("\nCv results\nsingle\tsched\tfadi\tsuhel\tbest\n");
    for(int i=0; i<5; i++){
      txtAreaMain.append(""+acc[i]+"\t");
    }
  }catch (IOException ioe){}
  }

  void jMenuItemNewCv_actionPerformed(ActionEvent e) {
  try{
    cv=new Cv(trainDatamineFile);
    cv.setSuppConIter(support,confidence,iteration);
    int numOfFolds=Integer.parseInt(JOptionPane.showInputDialog("number of folds?"));
    int repeat=Integer.parseInt(JOptionPane.showInputDialog("repeat?"));
    double[] acc=cv.getCvAccuracyNew(numOfFolds,repeat);
    txtAreaMain.append("\nNew CV results\nsingle\tsched\tfadi\tsuhel\tbest\n");
    for(int i=0; i<5; i++){
      txtAreaMain.append(""+acc[i]+"\t");
    }
  }catch (IOException ioe){}

  }



  void jMenuItem24_actionPerformed(ActionEvent e) {
    trainDatamine.rules.resetCounters();
  }

  void jMenuItem22_actionPerformed(ActionEvent e) {
    txtAreaMain.append( trainDatamine.rules.prntCounterSizes());
  }

  void jMenuItem25_actionPerformed(ActionEvent e) {
    txtAreaMain.append( trainDatamine.rules.prntCounterMaps());
  }


}
