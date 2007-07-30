package dm;

import org.apache.log4j.*;

public class ColumnName {
	static Logger log = Logger.getLogger(ColumnName.class);

	public ColumnName() {
	}

	/**
	 * Count how many bits in the binary representation  in the column
	 */
	public static int length(long columnId){
		int len=0;
		for (byte i=63; i>=0; i--){
			if (((1L<<i) & columnId) != 0)
				len+=1;
		}
		return len ;
	}

	/**
	 * Return String value of  the binary representation of the number
	 */

	public static String binary(long columnId){
		String result="";
		for (byte i=63; i>=0; i--){
			if (((1L<<i) & columnId) != 0){
				result+="1";
			}else{
				result+="0";
			}
		}
		return result;
	}


	/**
	 * get long number which differs only in one bit in the binary representation (remove the first "1" bit from the right)
	 */
	public static long firstSubColumn(long columnId){
		long tempLon;
		for (byte i=63; i>=0; i--){
			tempLon=(1L<<i);
			if ((tempLon & columnId) != 0){
				return columnId ^ (1L << i);
			}
		}
		return -1;
	}


	/**
	 * get long number which differs only in one bit in the binary representation (remove the first "1" bit from the left)
	 */
	public static long secondSubColumn(long columnId){
		long tempLon;
		for (byte i=0; i<=63; i++){
			tempLon=(1L<<i);
			if ((tempLon & columnId) != 0){
				return columnId ^ (1L << i);
			}
		}
		return -1;
	}


	/**
	 * 
	 * @param lon: the 
	 * @param numOfCols
	 * @return array of the atomic columns in the column name "lon"
	 */
	public static int[] orgColNames(long columnId,int numOfCols){
		long tempLon=0L;
		int[] index=new int[numOfCols+1];
		int cols=1;

		for (int i=0; i<=numOfCols; i++){
			tempLon=(1L<<i);
			if ((tempLon & columnId) != 0){
				index[cols]=i+1;
				cols++;
			}
		}
		index[0]=cols-1;
		return index;
	}

	/**
	 * 
	 * @param lon  
	 * @return the first atomic column found in the name, (we suppose there is only one).
	 */
	public static int atomicOrgColName(long columnId){
		for (int i=0; i<=63; i++){
			if (((1L<<i) & columnId) != 0)
				return i+1;
		}
		return -1;
	}

	/**
	 * 
	 * @param columnId
	 * @param bit : must be > 1
	 * @return
	 */
	public static boolean containsBit(long columnId,int bit){
		if (bit<1){
			log.fatal("Bit "+ bit+ "\t is less than 1");
			return false;
		}
		if ( ((1L<<(bit-1)) & columnId) != 0) 
			return true;
		else
			return false;

	}

	public static void main(String[] args){
		for(int i=1; i< 10; i++){
			log.info(i+"\t"+containsBit(4,i));
		}
	}
}