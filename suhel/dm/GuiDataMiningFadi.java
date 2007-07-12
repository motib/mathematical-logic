package dm;

import javax.swing.*;
import javax.swing.event.*;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

/**
 * <p>Title: data mining</p>
 * <p>Description: fadi project </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>

 * @version 1.0
 */

public class GuiDataMiningFadi extends JFrame implements ActionListener
{
    //static ReadPara read;
    private static JButton openBut;
    public static JTextArea mainarea;
    private static JFileChooser open;
    private static JTextField support;
    private static JTextField filePathName;
    private static JTextField confidance;
    private static JButton start,applySetting;
    private static JPanel tabpane;
    public static String supp=new String();
    public static String conf=new String();
    public boolean isRunning=false;
    private static int fontSize;
    public static File fileNamePath=new File("c:\\Moaath.txt");
    private static Dimension c;
     static DataMine dm;
    public GuiDataMiningFadi()
    {
       mainarea= new  JTextArea();
       //mainarea.setWrapStyleWord(true);
       JScrollPane jwe=new JScrollPane(mainarea);
       confidance= new JTextField("0.15");
       support= new JTextField("0.15");
       open=new JFileChooser();
       open.setCurrentDirectory(new File("c:\\"));
       open.setDialogType(open.OPEN_DIALOG);
       start= new JButton("Run");
       start.setEnabled(false);
       start.addActionListener(this);
       applySetting= new JButton("Set para");
       applySetting.setEnabled(false);
       applySetting.addActionListener(this);
       openBut=new JButton("open a File");
       openBut.addActionListener(this);
       tabpane= new JPanel();
       tabpane.setLayout(new BorderLayout(15, 15));
       JPanel p=new JPanel();
       p.setLayout(new GridLayout(2,6,5, 5));
       p.add(openBut);
       p.add(new JPanel());
       p.add(start);
       p.add(new JLabel("add the support"));
       p.add(support);
       p.add(new JPanel());
       p.add(new JPanel());
       p.add((new JPanel()));
       p.add(applySetting);
       p.add(new JLabel("add the confidence"));
       p.add(confidance);
       p.add(new JPanel());
       tabpane.add(p,"North");
       tabpane.add(jwe,"Center");
       tabpane.setBackground(Color.red);
    }
    public static void main(String[] dummy)
    throws java.io.IOException{

      // sup=Double.parseDouble(jj.supp);
     //  con=Double.parseDouble(jj.conf);
       //dm.generateColumns(0.1,0.15);

        //read=new ReadPara(String.valueOf(filePathName));
        c=Toolkit.getDefaultToolkit().getScreenSize();
        GuiDataMiningFadi f=new GuiDataMiningFadi();
        f.getContentPane().add(tabpane);
        f.setSize(600, 400);
        f.setLocation(c.width/2-300, c.height/2-200);
        f.show();
        //
        f.setDefaultCloseOperation(f.EXIT_ON_CLOSE);

    }
    public void actionPerformed(ActionEvent e)
    {
        if(e.getSource()==start)
        {
            //DataMining dd=new DataMining();
       /*     support.setEnabled(false);
            confidance.setEnabled(false);
            openBut.setEnabled(false);
            applySetting.setEnabled(false);*/
           try{
             dm =DataMineFile.setFilePath(fileNamePath);
           }catch (IOException et)
           {}
            mainarea.append(String.valueOf(dm.printConfidences(Double.parseDouble(supp),Double.parseDouble(conf))));
           // mainarea.append(String.valueOf(dm.printSupports(Double.parseDouble(supp))),0.1);



//            isRunning=true;
//            dd.supportIs();
       }
        if(e.getSource()==applySetting)
        {
            supp=support.getText();
            conf=confidance.getText();
            if(mainarea.getText()!=null)
               mainarea.append(" Support Value is > "+supp+"\n");
            else
               mainarea.setText(" Support Value is > "+supp+"\n");
            mainarea.append(" Confedence Value is > "+conf+"\n");
            mainarea.append("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n");
            mainarea.append("~~~~~~~~~~~the~result~is~~~~~~~~~~~~~~~~~~~~~\n");

        }
        if(e.getSource()==openBut)
        {
            if(open.showOpenDialog(this)==JFileChooser.APPROVE_OPTION)
            fileNamePath=open.getSelectedFile();
            mainarea.setText("File Name and Directory is > "+fileNamePath+"\n");
            start.setEnabled(true);
            applySetting.setEnabled(true);
        }

    }

}
