package dm;
import java.io.*;
import javax.swing.*;
public class fileTest {

public static void main(String[] args)throws IOException {
    DataOutputStream out = new DataOutputStream(new FileOutputStream("c://invoice1.txt"));
    for(int i=0; i<2; i++){
    out.writeDouble(0.1);
    out.writeChar('\t');
    out.writeInt(5000);
    out.writeChar('\t');
    out.writeChars("chars");
    out.writeChar('\n');
    }
    out.close();
    /////
    DataInputStream in = new DataInputStream(new FileInputStream("c://invoice1.txt"));
    try {
      for(int i=0; i<2; i++){
        System.out.print(in.readDouble());
        System.out.print(in.readChar());       //throws out the tab
        System.out.print(in.readInt());
        System.out.print(in.readChar());

        //System.out.println(in.readChar());     //throws out the tab
        char chr;
        StringBuffer desc = new StringBuffer(20);
        while ((chr = in.readChar()) != "\n".charAt(0))
            desc.append(chr);
        System.out.print(desc.toString());
        System.out.print(chr);
      }

    } catch (EOFException e) { }
    in.close();
}

  public fileTest() {
  }
}