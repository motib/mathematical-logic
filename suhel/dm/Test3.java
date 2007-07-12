package dm;

/**
 * <p>Title: MCAR</p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author Fadi & Suhel
 * @version 1.2
 */

public class Test3 {
  public Test3() {
  }
  public  static void wwww(int[] a){
    System.out.print("\n");
    for(int i=0; i<a.length; i++){
      System.out.print(""+a[i]+"\t");
    }
  }

  public static void main(String[] args){
  int[] arr={11,20,30,40,50,60,70,80,90,100};
  wwww(arr);
  System.out.print("\n");
  int[] rnd=Tools.getRandomSamp(6,10);
  wwww(rnd);
  System.out.print("\n");
  for(int j=0; j<6; j++){
    System.out.print(""+arr[rnd[j]]+"\t");
  }


  }
}