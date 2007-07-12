package dm;

import java.awt.*;
import javax.swing.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class Frame1 extends JFrame {
  JTextArea jTextArea1 = new JTextArea();

  public Frame1() {
    try {
      jbInit();
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
  public static void main(String[] args){}
  private void jbInit() throws Exception {
    jTextArea1.setText("jTextArea1");
    this.getContentPane().add(jTextArea1, BorderLayout.CENTER);
  }
}