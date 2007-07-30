package weka.dm;
import org.apache.log4j.*;

import dm.ColumnName;



import java.util.*;
import weka.core.*;

public class WorkBench {
	static Logger log=Logger.getLogger(WorkBench.class);

	protected Instances data=null;
	Map<Long, Column> existingColumns=new HashMap<Long, Column>();
	public Instances getData() {
		return data;
	}
	public void setData(Instances data) {
		this.data = data;
	}
	/**
	 * @param args
	 */

	public void generate(double support, int classCol){
		if(data==null){
			log.error("data not set yet");
			return;
		}
		data.setClassIndex(classCol);
		int oSupport=(int)Math.round(0.5+support*data.numInstances());
		long allColumns=Math.round(Math.pow(2,data.numAttributes()))-1;
		existingColumns.clear();
		for(long i=1; i<=allColumns; i++){
			if(ColumnName.containsBit(i, classCol+1))continue;
			Column clmn=new Column(data,i,existingColumns);
			clmn.setOSupport(oSupport);

			if(clmn.generate()){
				log.info("Add column"+ i);
				existingColumns.put(i,clmn);
			}
			String ssss=null;
		}

		return  ;
	}

	public String printColumns(){
		String result="All existing columns :";
		for (Column iter : existingColumns.values()) {
			result+=iter.getItemsAsInstances().toString()+"\n";
		}
		return result;
	}
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}
