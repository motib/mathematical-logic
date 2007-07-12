package dm;


import java.io.*;
import java.util.*;
import javax.swing.*;
/**
 * <p>Title: data mining</p>
 * <p>Description: fadi project </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: Bradford University</p>
 * @version 1.0
 */

public class ReadPara
{
     StringTokenizer token;
    public  File filePath=new File("c:\\Moaath.txt");
    private  FileReader file;
    public  String data;
    public  String sizeOfFile;
    public  String nameOfFile;
    private  String[] store=new String[100];;
    public  String[][] col_Dis;
    public  String[][] arrData;
    public  int colNum=-2;
    private  int help;
    public ReadPara(File filePath1)
    {
        filePath=filePath1;
    }
        public ReadPara(String filePath1)
    {
        filePath=new File(filePath1);
    }

    public ReadPara()
    {
    }
///////////////////////////////////////////////////////////////////////////
    public  String deleteAt(String str)
    {
        char[] chr=new char[100];
        char[] ch=str.toCharArray();
        for (int i=1;i<ch.length;i++)
        chr[i-1]=ch[i];
        return  String.copyValueOf(chr);
    }
////////////////////////////////////////////////////////////////////////
  public void fafo ()
    throws java.io.IOException {
    StringBuffer strbuf=new StringBuffer();
       BufferedReader bFile=new BufferedReader(new FileReader((filePath)));
       String str=bFile.readLine();
       int i=0;
        while(!str.startsWith("@data"))
        {
                 colNum++;
                 store[i]=deleteAt(str);
                 str=bFile.readLine();
                 i++;
        }
          while(str!=null)
          {
                strbuf=strbuf.append(","+str);
                str=bFile.readLine();
          }
    nameOfFile=store[0];
    sizeOfFile=store[1];
    double p=Double.parseDouble(sizeOfFile);
    help=(int)Math.round(p);
    strbuf=strbuf.replace(0, 7,"");

    data=String.valueOf(strbuf);
     col_Dis=new String[colNum][help];
    col_Dis=val_dist(collectIt());
    arrData=getData(data);

  }

    public  StringTokenizer[] collectIt()
    {
        StringTokenizer[] strr=new StringTokenizer[colNum];
        for(int i=0;i<colNum;i++)
            strr[i]=distVal(store[i+2]);
        return strr;
    }
/////////////////////////////////////////////////////////////////////////
    public  StringTokenizer distVal(String str)
    throws StringIndexOutOfBoundsException
    {
        int i=0;
        StringBuffer strb=new StringBuffer();
        String[] strAr=new String[60];
        char c=str.charAt(i);
         while(c!='{'&& i<(str.length()-1))
            {
                c=str.charAt(i);
                i++;
            }
        while(c!='}'&& i<(str.length()-1))
        {
            strb=strb.append(c);
            c=str.charAt(i);
             i++;
       }
    strb=strb.delete(0,1);
    return new StringTokenizer(String.valueOf(strb),",");
    }
 ////////////////////////////////////////////////////////////////////////////
    public  String[][] val_dist(StringTokenizer[] strTo)
    throws ArrayIndexOutOfBoundsException
    {
        String c=" ";
        int j=0;
        String[][] str=new String[strTo.length][help];
        for(int i=0;i<strTo.length;i++)
        {
            while(strTo[i].hasMoreTokens())
            {
                 c=strTo[i].nextToken();
                 str[i][j]=c;
                 j++;
            }
            j=0;
        }
        return str;
    }
////////////////////////////////////////////////////////////////////////////
    public  String[][] getData(String str)
    throws ArrayIndexOutOfBoundsException
    {
        token=new StringTokenizer(str,",");
        String[][] arr1Data=new String[help][colNum];
         for(int j=0;j<arr1Data.length;j++)
            for(int i=0;i<arr1Data[j].length;i++)
                arr1Data[j][i]=token.nextToken();
        return arr1Data;
    }
}
