package dm;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author unascribed
 * @version 1.0
 */

public class columnName {

  public columnName() {
  }

  public static int length(long lon){
    int len=0;
    for (byte i=63; i>=0; i--){
      if (((1L<<i) & lon) != 0)
        len+=1;
    }
    return len ;
  }
// the binary representaion of the long number
  public static String binary(long lon){
    String s="";
    for (byte i=63; i>=0; i--){
      if (((1L<<i) & lon) != 0){
        s+="1";
      }else{
        s+="0";
      }
    }
    return s;
  }
//the first sub column
  public static long firstSubColumn(long lon){
    long lon2;
    for (byte i=63; i>=0; i--){
      lon2=(1L<<i);
      if ((lon2 & lon) != 0){
        return lon ^ (1L << i);
      }
    }
    return -1;
  }

  public static long secondSubColumn(long lon){
    long lon2;
    for (byte i=0; i<=63; i++){
      lon2=(1L<<i);
      if ((lon2 & lon) != 0){
        return lon ^ (1L << i);
      }
    }
    return -1;
  }
  // lon is the colomn name
  public static int[] orgColNames(long lon,int numOfCols){
    long lon2=0L;
    int[] index=new int[numOfCols+1];
    int cols=1;

    for (int i=0; i<=numOfCols; i++){
      lon2=(1L<<i);
      if ((lon2 & lon) != 0){
        index[cols]=i+1;
        cols++;
      }
    }
  index[0]=cols-1;
  return index;
  }
  public static int atomicOrgColName(long lon){
    for (int i=0; i<=63; i++){
      if (((1L<<i) & lon) != 0)
        return i+1;
  }
  return -1;
 }
}