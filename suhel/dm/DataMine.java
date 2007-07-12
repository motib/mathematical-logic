package dm;

import java.io.*;
import java.util.*;
import javax.swing.*;
import java.text.DecimalFormat;



public class DataMine {

	public TreeMap entity;
	classColumn classCol;
	//holds the position of CLASS attribute
	public final int CLASS;
	final String[] classArray;
	public String[] pedictedClass;
	public boolean[] instanceStatus;
	int orgSize;
	//size of the data (total number)
	int TOTAL_ENTITIES;
	public TreeMap existingColumns;
	long allColumns=0L;
	Rules rules;
	//holds the line numbers
	public Set allLines;

	private static double tempSupport=1234321.025545454;//random number

	/*
	 * entts: Map
	 * aClass: String[] holds the class values
	 */
	public DataMine(Map entts, String[] aClass) {
		entity = new TreeMap(entts);//////
		allLines= new TreeSet(entity.keySet());
		CLASS=((String[])entity.get(new Integer(1))).length;
		TOTAL_ENTITIES=entity.size();
		orgSize=entity.size();
		classArray= new String[aClass.length];
		System.arraycopy(aClass,0,classArray,0,aClass.length);
		existingColumns =new TreeMap();
		//Get the number of possible compound columns 
		allColumns=Math.round(Math.pow(2,CLASS-1))-1;
		rules=new Rules(this);
		classCol=new classColumn(this);
	}
	public DataMine(String fileName){
		Object[] o=new Object[2];
		dataReader(fileName,o);
		entity = (TreeMap)o[0];//////
		classArray=(String[])o[1];
		//    System.out.println("out side:" +entity.size());
		allLines= new TreeSet(entity.keySet());
		CLASS=((String[])entity.get(new Integer(1))).length;
		TOTAL_ENTITIES=entity.size();
		orgSize=entity.size();
		//    classArray= new String[aClass.length];classArray=
		//   System.arraycopy(aClass,0,classArray,0,aClass.length);
		existingColumns =new TreeMap();
		allColumns=Math.round(Math.pow(2,CLASS-1))-1;
		rules=new Rules(this);
		classCol=new classColumn(this);

	}
	public DataMine(DataMine PDm){
		entity = new TreeMap(PDm.entity);//////
		allLines= new TreeSet(entity.keySet());
		CLASS=((String[])entity.get(new Integer(1))).length;
		TOTAL_ENTITIES=entity.size();
		orgSize=entity.size();
		classArray= new String[PDm.classArray.length];
		System.arraycopy(PDm.classArray,0,classArray,0,PDm.classArray.length);
		existingColumns =new TreeMap();
		allColumns=Math.round(Math.pow(2,CLASS-1))-1;
		rules=new Rules(this);
		classCol=new classColumn(this);

	}

	public boolean generateColumnsNew(){
		existingColumns.clear();
		for(long i=1; i<=allColumns; i++){
			Column clmn=new Column(i,0,0,this);
			if(clmn.isAppropriateNew()){
				existingColumns.put(new Long(i),clmn);
			}
		}
		return true;
	}
	public boolean generateColumns(double support,double confidence){
		/*
	    if(Math.abs(tempSupport-support)<0.0000000000001)
	      return false;
	    tempSupport=support;
		 */
		int oSupport=(int)Math.round(0.5+support*orgSize);
		//    System.out.println("minimum occurance:" + oSupport);
		existingColumns.clear();
		for(long i=1; i<=allColumns; i++){
			Column clmn=new Column(i,oSupport,confidence,this);
			if(clmn.isAppropriate()){
				existingColumns.put(new Long(i),clmn);
			}
		}
		return true;
	}

	//generate the occurances atomic columns, no need to chech the support and the confidence
	public boolean generateOccuranceColumns(){
		existingColumns.clear();
		//    JOptionPane.showMessageDialog(null,"allColl"+allColumns);
		for(long i=1; i<=allColumns; i*=2){
			//      JOptionPane.showMessageDialog(null,"i"+i);
			Column clmn=new Column(i,0,0.0,this);
			existingColumns.put(new Long(i),clmn);
			//      JOptionPane.showMessageDialog(null,"existing size"+existingColumns.size());

			clmn.generateTestOccurances();
		}
		return true;
	}
	public StringBuffer printConfidences(double support, double confidence){
		StringBuffer otpt=new StringBuffer();
		generateColumns(support,confidence);
		Iterator itr =existingColumns.keySet().iterator();
		while(itr.hasNext()){
			Long myInt = (Long)itr.next();
			Column clmn=(Column)existingColumns.get(myInt);
			otpt.append(clmn.calculateColumnConfidences(confidence));
		}
		return otpt;
	}
	public StringBuffer printSupports(double support,double confidence){
		StringBuffer otpt=new StringBuffer();
		generateColumns(support,confidence);
		Iterator itr =existingColumns.keySet().iterator();
		while(itr.hasNext()){
			Long myInt = (Long)itr.next();
			Column clmn=(Column)existingColumns.get(myInt);
			otpt.append(clmn.getSupports());
		}
		return otpt;
	}
	public void adjustTotalEn(){
		TOTAL_ENTITIES=entity.size();
	}
	public boolean setLines(Set PLines){
		existingColumns.clear();
		allLines.clear();
		boolean b=allLines.addAll(PLines);
		TOTAL_ENTITIES=allLines.size();
		classCol=new classColumn(this);
		return b;

	}
	public boolean delLines(Set s2){
		existingColumns.clear();
		boolean b=allLines.removeAll(s2);
		TOTAL_ENTITIES=allLines.size();
		classCol=new classColumn(this);
		return b;
	}
	public boolean resetLines(){
		boolean b=setLines(entity.keySet());
		return b;
	}
	public Set getRuleColValOcc(String cond,int clmn){
		long clmnNm=1;
		for(int i=0;i<clmn-1;i++)clmnNm*=2;
		//    JOptionPane.showMessageDialog(null,"clmn"+clmnNm);
		Column c=(Column)existingColumns.get(new Long(clmnNm));
		//    JOptionPane.showMessageDialog(null,"existing "+existingColumns.size());

		Integer fstOcc=(Integer)c.itemsAsString.get(cond);
		if(fstOcc==null)return new HashSet();
		Sccl sccl=(Sccl)c.items.get(fstOcc);
		return sccl.lines;
	}

	void dataReader(String PFileName,Object[] o){
		String[] classes;
		TreeMap entts;
		int errorLine=1;
		try{
			BufferedReader in= new BufferedReader(new FileReader(PFileName));
			//String s=new String();
			in.readLine();
			in.read();
			String s=in.readLine();//read the number of rows
			int size=Integer.parseInt(s)	;
			entts=new TreeMap();
			classes=new String[size+1];
			int numOfCols=0;
			s=in.readLine();//
			while(!s.startsWith("@data")){
				numOfCols++;
				s=in.readLine();
			}

			s=in.readLine();//read the first row
			for(int lineNumber=1; lineNumber<=size; lineNumber++){
				errorLine++;
				StringTokenizer st=new StringTokenizer(s,",");
				String[] row=new String[numOfCols] ;
				int i=0;
				while(st.hasMoreTokens()){
					i=(i+1)%numOfCols;
					row[i]=st.nextToken();
				}
				//        System.out.println(""+lineNumber+" , "+row[1]);
				entts.put(new Integer(lineNumber), row );
				classes[lineNumber]=row[0];
				s=in.readLine();
			}

			in.close();
			//    System.out.println("size:"+entts.size());
			o[0]=entts;
			o[1]=classes;
		}catch (Exception e){
			JOptionPane.showMessageDialog(null,"Error At Line "+ errorLine);
		}
	}
	/**
	 *
	 * @return string represent this datamine
	 */
	public String printDataMine(){
		//    Iterator iterS=
		String[] line=new String[CLASS];
		String result="\n";
		Iterator iter=entity.entrySet().iterator();
		while(iter.hasNext()){
			Map.Entry e= (Map.Entry)iter.next();
			result+="\n"+((Integer)e.getKey()).intValue();
			line=(String[])e.getValue();
			for(int i=1; i<line.length; i++){
				result+="\t"+line[i];
			}
		}
		return result;
	}
	public void iterate(double minRemainInst,double conf, double supp,int numOfItr)
	throws IOException{
		allLines= new TreeSet(entity.keySet());
		rules=new Rules(this);
		rules.iterate(minRemainInst,conf,supp,numOfItr);
	}
	public void iterate2(double minRemainInst,double conf, double supp,int numOfItr)
	throws IOException{
		allLines= new TreeSet(entity.keySet());
		rules=new Rules(this);
		rules.iterate2(minRemainInst,conf,supp,numOfItr);
	}

	public Set getValueOccurancesInColumn(String value,long colId){
		Column clmn=(Column)existingColumns.get(new Long(colId));
		return clmn.getValueOccurances(value);
	}

}