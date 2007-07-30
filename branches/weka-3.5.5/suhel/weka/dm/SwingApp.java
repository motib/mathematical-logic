package weka.dm;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileReader;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;

import org.apache.log4j.Logger;

import weka.core.Instances;
import dm.Chooser;


/**
* This code was edited or generated using CloudGarden's Jigloo
* SWT/Swing GUI Builder, which is free for non-commercial
* use. If Jigloo is being used commercially (ie, by a corporation,
* company or business for any purpose whatever) then you
* should purchase a license for each developer using Jigloo.
* Please visit www.cloudgarden.com for details.
* Use of Jigloo implies acceptance of these licensing terms.
* A COMMERCIAL LICENSE HAS NOT BEEN PURCHASED FOR
* THIS MACHINE, SO JIGLOO OR THIS CODE CANNOT BE USED
* LEGALLY FOR ANY CORPORATE OR COMMERCIAL PURPOSE.
*/
public class SwingApp extends javax.swing.JFrame {
static Logger log=Logger.getLogger(SwingApp.class);
private JMenuItem mnuPrintExistingColumns;
	WorkBench wb=new WorkBench();
	{
		//Set Look & Feel
		try {
			javax.swing.UIManager.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
		} catch(Exception e) {
			e.printStackTrace();
		}
	}


	private JMenuItem helpMenuItem;
	private JMenu jMenu5;
	private JToolBar statusBar;
	private JTextArea txtLeft;
	private JLabel lblStatus;
	private JTextArea txt;
	private JScrollPane scrl_2;
	private JScrollPane scrl_1;
	private JFileChooser fileChooser;
	private JMenuItem mnuOpenFile;
	private JMenu mnuFile;
	private JSplitPane splitPane;
	private JButton btnTest;
	private JToolBar toolBar;
	private JMenuItem mnuCleanLeft;
	private JMenuItem mnuPrintInstance;
	private JMenu mnuPrint;
	private JMenuItem mnuClean;
	private JMenu screen;
	private JMenuBar menus;

	/**
	* Auto-generated main method to display this JFrame
	*/
	public static void main(String[] args) {
		SwingApp inst = new SwingApp();
		inst.setVisible(true);
	}
	
	public SwingApp() {
		super();
		initGUI();
	}
	
	private void initGUI() {
		try {
			{
				BorderLayout thisLayout = new BorderLayout();
				getContentPane().setLayout(thisLayout);
				this.setLocation(new java.awt.Point(300, 200));
			}
			{
				splitPane = new JSplitPane();
				getContentPane().add(splitPane, BorderLayout.CENTER);
				splitPane.setBounds(0, 30, 392, 211);
				splitPane.setPreferredSize(new java.awt.Dimension(392, 22));
				{
					scrl_1 = new JScrollPane();
					splitPane.add(scrl_1, JSplitPane.RIGHT);
					{
						txt = new JTextArea();
						scrl_1.setViewportView(txt);
						txt.setText("txt");
						txt.setEditable(false);
					}
				}
				{
					scrl_2 = new JScrollPane();
					splitPane.add(scrl_2, JSplitPane.LEFT);
					{
						txtLeft = new JTextArea();
						scrl_2.setViewportView(txtLeft);
						txtLeft.setText("txtLeft");
					}
				}
			}
			{
				toolBar = new JToolBar();
				getContentPane().add(toolBar, BorderLayout.NORTH);
				toolBar.setBounds(0, 0, 392, 30);
				{
					btnTest = new JButton();
					GridLayout btnTestLayout = new GridLayout(1, 1);
					btnTestLayout.setColumns(1);
					btnTestLayout.setHgap(5);
					btnTestLayout.setVgap(5);
					toolBar.add(btnTest);
					btnTest.setText("Test");
					btnTest.setLayout(null);
					btnTest.setEnabled(false);
					btnTest.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent evt) {
							btnTestActionPerformed(evt);
						}
					});
				}
			}
			{
				statusBar = new JToolBar();
				getContentPane().add(statusBar, BorderLayout.SOUTH);
				{
					lblStatus = new JLabel();
					statusBar.add(lblStatus);
					lblStatus.setText("Status");
				}
			}
			this.setSize(400, 300);
			{
				menus = new JMenuBar();
				setJMenuBar(menus);
				{
					mnuFile = new JMenu();
					menus.add(mnuFile);
					mnuFile.setText("File");
					{
						mnuOpenFile = new JMenuItem();
						mnuFile.add(mnuOpenFile);
						mnuOpenFile.setText("Open");
						mnuOpenFile.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent evt) {
								mnuOpenFileActionPerformed(evt);
							}
						});
					}
				}
				{
					screen = new JMenu();
					menus.add(screen);
					screen.setText("Screen");
					{
						mnuClean = new JMenuItem();
						screen.add(mnuClean);
						mnuClean.setText("Clean");
						mnuClean.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent evt) {
								mnuCleanActionPerformed(evt);
							}
						});
					}
					{
						mnuCleanLeft = new JMenuItem();
						screen.add(mnuCleanLeft);
						mnuCleanLeft.setText("Clean Left");
						mnuCleanLeft.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent evt) {
								txtLeft.setText("");
							}
						});
					}
				}
				{
					jMenu5 = new JMenu();
					menus.add(jMenu5);
					jMenu5.setText("Help");
					{
						helpMenuItem = new JMenuItem();
						jMenu5.add(helpMenuItem);
						helpMenuItem.setText("Help");
					}
				}
				{
					mnuPrint = new JMenu();
					menus.add(mnuPrint);
					mnuPrint.setText("Print");
					{
						mnuPrintInstance = new JMenuItem();
						mnuPrint.add(mnuPrintInstance);
						mnuPrintInstance.setText("Print File");
						mnuPrintInstance.setEnabled(false);
						mnuPrintInstance
							.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent evt) {
								mnuPrintInstanceActionPerformed(evt);
							}
							});
					}
					{
						mnuPrintExistingColumns = new JMenuItem();
						mnuPrint.add(mnuPrintExistingColumns);
						mnuPrintExistingColumns
							.setText("Print Existing Columns");
						mnuPrintExistingColumns.setEnabled(false);
						mnuPrintExistingColumns
							.addActionListener(new ActionListener() {
							public void actionPerformed(ActionEvent evt) {
								mnuPrintExistingColumnsActionPerformed(evt);
							}
							});
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void mnuOpenFileActionPerformed(ActionEvent evt) {
		System.out.println("mnuOpenFile.actionPerformed, event=" + evt);
		//TODO add your code for mnuOpenFile.actionPerformed
		Chooser chooser=new Chooser(this);
		if(!chooser.isFileSelected("open"))return;
		    String dataFile=chooser.getFileName();
		    try{
			    Instances data= new Instances(new FileReader(dataFile));
			    wb.setData(data);
			    btnTest.setEnabled(true);
			    mnuPrintInstance.setEnabled(true);
			   
		      
		    }catch(Exception e){
		      
		    }
	}
	
	private void mnuPrintInstanceActionPerformed(ActionEvent evt) {
		System.out.println("mnuPrintInstance.actionPerformed, event=" + evt);
		//TODO add your code for mnuPrintInstance.actionPerformed
		if( wb.getData()==null)
		  lblStatus.setText("data not set yet");
		else
		  txt.setText(wb.getData().toString());
	}
	
	private void mnuCleanActionPerformed(ActionEvent evt) {
		System.out.println("mnuClean.actionPerformed, event=" + evt);
		//TODO add your code for mnuClean.actionPerformed
		txt.setText("");
		
	}
	
	private void mnuPrintExistingColumnsActionPerformed(ActionEvent evt) {
		if(wb.existingColumns == null){
			lblStatus.setText("existing coulmns are not set yet");
			return;
		}
		if(wb.existingColumns.size()==0){
			lblStatus.setText("no columns in the existing columns");
			return;
		}
		txt.setText(wb.printColumns());
	}
	
	private void btnTestActionPerformed(ActionEvent evt) {
		wb.generate(0, 2);
		mnuPrintExistingColumns.setEnabled(true);
	}

}
