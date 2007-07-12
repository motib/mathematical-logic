package dm;

import java.awt.*;
import javax.swing.*;

/**
 * <p>Title: MCAR</p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author Fadi & Suhel
 * @version 1.2
 */

public class DialogChangeParameters extends JDialog {
  JPanel panel1 = new JPanel();
  GridLayout gridLayout1 = new GridLayout();
  GridLayout gridLayout2 = new GridLayout();

  public DialogChangeParameters(Frame frame, String title, boolean modal) {
    super(frame, title, modal);
    try {
      jbInit();
      pack();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  public DialogChangeParameters() {
    this(null, "", false);
  }
  void jbInit() throws Exception {
    panel1.setLayout(gridLayout2);
    this.getContentPane().setLayout(gridLayout1);
    getContentPane().add(panel1, null);
  }
}