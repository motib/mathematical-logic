	package dm;
	
	import java.util.*;
	import java.io.*;
	import javax.swing.*;
	import java.text.*;
	
	/**
	 * <p>Title: </p>
	 * <p>Description: </p>
	 * <p>Copyright: Copyright (c) 2004</p>
	 * <p>Company: </p>
	 * @author unascribed
	 * @version 1.0
	 */
	
	public class Rules {
	  final int orgSize;
	  int minOcc = 0;
	  int accOcc = 0;
	  int numOfCols; //number of fields without the class field
	  private DecimalFormat tt = new DecimalFormat("0000000000000000.0000");
	  /**
	   * Map mp (classifier)
	   * key :String ruleId e.g. 20|*|47|
	   * value: Rule type object
	   */
	  Map mp;
	  /**
	   * Arraylist al: contain the id of the survived rules of the ranked rule set.
	   * the actual rules (Rule objects) exist in Map mp
	   * items: ruleId added in a ranked order
	   */
	  ArrayList al;
	  DataMine dm;
	  //added lately to check the classifiers
	  SCounter con1, con2, con3, con4, con5, con6;
	  Map counterMap = new HashMap();
	  //
	  public DataMine rulesDataMine;
	  /**
	   * rankedRuleSet TreeMap
	   * key: hassh Code
	   * value: long[]
	   * long[0]: columnId (inside the datamine)
	   * long[1]: itemId (inside the column)
	   */
	  public TreeMap rankedRuleSet;
	  /**
	   * TreeMap pc (predicted class)
	   * Key: Integer line ids of tested datamine
	   * value:String the corrosponding rule ids in the classifier
	   */
	  TreeMap pc = new TreeMap(); ///added when writing saveWithPredition() method
	  Set idis = new HashSet(); //contains the original idis of the rules (new)
	  DataMine destDm;
	
	  public Rules(DataMine dm2) {
	    dm = dm2;
	    orgSize = dm2.TOTAL_ENTITIES;
	    numOfCols = dm.CLASS - 1;
	    mp = new HashMap();
	    al = new ArrayList();
	    rankedRuleSet = new TreeMap();
	    //added lately to check the classifiers
	    con1 = new SCounter();
	    con2 = new SCounter();
	    con3 = new SCounter();
	    con4 = new SCounter();
	    con5 = new SCounter();
	    con6 = new SCounter();
	    counterMap.put("confidence", con1);
	    counterMap.put("support", con2);
	    counterMap.put("numOfAttribs", con3);
	    counterMap.put("occ_orgSize", con4);
	    counterMap.put("rndCol", con5);
	    counterMap.put("rndRow", con6);
	    //
	
	  }
	
	  ///
	
	  public String printRankedRuls() {
	    String s = "\nthe rules";
	    String s2 = "";
	    Iterator itr = rankedRuleSet.entrySet().iterator();
	    while (itr.hasNext()) {
	      Map.Entry e = (Map.Entry) itr.next();
	      double dbl = ( (Double) e.getKey()).doubleValue();
	      long[] a = (long[]) e.getValue();
	      int tint = (int) a[1];
	      Column clmn = (Column) dm.existingColumns.get(new Long(a[0]));
	      s2 += "\n" + tt.format(dbl) + "\t" + a[0] + "\t" + a[1] + "\t" +
	          clmn.calculateItemConfidence(new Integer(tint)) + "\n"; //+clmn.prntRuleOcc(new Integer(tint));//tt.format
	    }
	    return s + s2;
	  }
	
	  ////
	
	  public void rankARule(int occ, int nomin, long columnId, int rowId,
	                        String pClass) {
	    System.out.println("rank \tcolId= " + columnId + "\trowId=" + rowId +
	                       "\tpClass " + pClass);
	    rankARule(occ, nomin, columnId, rowId);
	  }
	
	  public void rankARule(int occ, int nomin, long columnId, int rowId) {
	    double[] b = {
	         (double) nomin / occ,
	        (double) nomin / orgSize,
	        (double) 1.0 - (double) columnName.length(columnId) / dm.allColumns,
	        (double) occ / orgSize,
	        (double) ( (double) dm.allColumns - (double) columnId) /
	        (double) dm.allColumns,
	        (double) rowId / orgSize};
	    int a[] = {3, 2, 3, 3, 2};
	    double hCode = Tools.ruleOrder(a, b);
	    long[] rl = new long[2];
	    rl[0] = columnId;
	    rl[1] = rowId;
	
	//  System.out.println("rank rule Code="+tt.format(hCode)+"\tcolId= "+columnId+"\trowId="+rowId);
	    rankedRuleSet.put(new Double( -hCode), rl);
	    //added lately to check the classifier
	
	    int digits = 0;
	
	    //confidence
	    double tempValue = 0.0;
	    tempValue = b[0] * Math.pow(10, 3);
	
	    if (getSCounter("confidence").addValue(tempValue, digits) > 1) {
	
	      //support
	      tempValue = (tempValue + b[1]) * Math.pow(10, 3);
	      if (getSCounter("support").addValue(tempValue, digits) > 1) {
	
	        //number of attributes
	        tempValue = (tempValue + b[2]) * Math.pow(10, 3);
	        if (getSCounter("numOfAttribs").addValue(tempValue, digits) > 1) {
	
	          // occ/orgSize will be deleted lately
	          tempValue = (tempValue + b[3]) * Math.pow(10, 3);
	          if (getSCounter("occ_orgSize").addValue(tempValue, digits) > 1) {
	
	            //random for colomn number
	            tempValue = (tempValue + b[4]) * Math.pow(10,3);
	            if (getSCounter("rndCol").addValue(tempValue, digits) > 1) {
	
	              //random for row number
	              tempValue = (tempValue + b[5])*Math.pow(10, 3);
	              if (getSCounter("rndRow").addValue(tempValue, digits) > 1)
	
	                System.out.print("\nidentical ranked rule" + b[0] + "\t," + b[1] +
	                                 "\t," + b[2] + "\t," + b[3] + "\t," + b[4] +
	                                 "\t," +
	                                 b[5] + "\n");
	            }
	          }
	        }
	      }
	    }
	      //
	
	    }
	
	    public boolean addRule(long clmn2, int itm2, int occ2, String cls) {
	      String[] cols = new String[numOfCols];
	      String[] clss = new String[100];
	      clss[0] = cls; //this is not good ,but to be changed later
	      int[] clssOcc = new int[100];
	      clssOcc[0] = occ2;
	      int allOcc = occ2;
	      String ruleId = fillCols(cols, clmn2, itm2);
	      addRule(new Rule(numOfCols,
	                       cols,
	                       allOcc,
	                       1,
	                       clss,
	                       clssOcc));
	      return true;
	    }
	    private String fillCols(String[] cols, long clm, int itm) {
	      String s = "";
	      for (byte i = 0; i < numOfCols; i++) {
	        if ( ( (1L << i) & clm) != 0) {
	          cols[i] = ( (String[]) dm.entity.get(new Integer(itm)))[i + 1];
	        }
	        else {
	          cols[i] = "*";
	        }
	        s += cols[i] + "|";
	      }
	      return s;
	    }
	    public boolean addRule(Rule rl) {
	      String s = rl.getId();
	      Rule trl = (Rule) mp.get(s);
	      if (trl == null) {
	        mp.put(s, rl);
	        al.add(s);
	      }
	      else {
	        trl.updateRule(rl);
	        mp.put(s, trl);
	      }
	      return true;
	    }
	
	    public void SaveToFile(String fs) throws IOException {
	      int tot = mp.keySet().size();
	      BufferedWriter out2 = new BufferedWriter(new FileWriter(fs));
	      out2.write(getClassifier().toString());
	      out2.close();
	
	    }
	    public StringBuffer getClassifier() {
	      int tot = mp.keySet().size();
	      StringBuffer out2 = new StringBuffer();
	      out2.append("number of rules\t" + tot + "\n");
	      out2.append("number of columns\t" + numOfCols + "\n");
	      out2.append("original dataset size\t" + orgSize + "\n");
	      out2.append("minimum occurance\t" + minOcc + "\n");
	      for (int i = 0; i < numOfCols; i++) {
	        out2.append("Co1_" + i + "\t");
	      }
	      out2.append("allOcc\t#clss\n");
	      for (int j = 0; j < al.size(); j++) {
	        Rule rl = (Rule) mp.get( (String) al.get(j));
	        for (int i = 0; i < numOfCols; i++) {
	          out2.append(rl.cols[i] + "\t");
	        }
	        out2.append(rl.allOcc + "\t");
	        out2.append(rl.numOfClasses + "\t");
	        for (int i = 0; i < rl.numOfClasses; i++) {
	          out2.append(rl.clss[i] + "\t");
	        }
	        for (int i = 0; i < rl.numOfClasses; i++) {
	          out2.append(rl.clssOcc[i] + "\t");
	        }
	        out2.append("\n");
	      }
	      out2.append("Accuracy  is\n" +
	                  accOcc + "/" + orgSize + "= " + (double) accOcc / orgSize);
	      return out2;
	    }
	
	    public void loadFromFile(String fs) throws IOException {
	    }
	    public void applyToDatamineAndSaveTo(DataMine testDataMine,
	                                         String resultFile) throws IOException {
	    }
	    public void saveWithPrediction(String resultFile) throws IOException {
	      double countSingleClss = 0;
	      double countMultiSched = 0;
	      double countMultiFadi = 0;
	      double countMultiSuhel = 0;
	      double countSingleBest = 0;
	
	      BufferedWriter out2 = new BufferedWriter(new FileWriter(resultFile));
	      out2.write("add your comments here\n");
	      out2.write("line\t");
	      for (int i = 1; i <= numOfCols; i++) {
	        out2.write("C" + i + "\t");
	      }
	      out2.write("class\t");
	      out2.write("bClass\t");
	      out2.write("single\t");
	      out2.write("sched\t");
	      out2.write("fadi\t");
	      out2.write("suhel\t");
	      out2.write("bClass\t");
	      out2.write("-->\t");
	      out2.write("Multi Predicted Classes\n");
	
	      Iterator iter = destDm.entity.entrySet().iterator();
	      while (iter.hasNext()) {
	        Map.Entry e = (Map.Entry) iter.next();
	        Integer ti = (Integer) e.getKey();
	        out2.write(ti.intValue() + "\t");
	        String[] a2 = (String[]) e.getValue();
	        for (int i = 1; i <= numOfCols; i++) {
	          out2.write(a2[i] + "\t");
	        }
	        out2.write(a2[0] + "\t"); //write the org. class
	        Rule rl = (Rule) pc.get(ti);
	        //Rule rl=(Rule)mp.get(rlId);
	        String bestClass = rl.clss[0];
	        int bestOcc = rl.clssOcc[0];
	        int bestJi = 0;
	        for (int ji = 1; ji < rl.numOfClasses; ji++) {
	          if (rl.clssOcc[ji] > bestOcc) {
	            bestClass = rl.clss[ji];
	            bestOcc = rl.clssOcc[ji];
	            bestJi = ji;
	          }
	        }
	        boolean b = false;
	        double dd = 0;
	        double singleClss = 0;
	        double muliSched = 0;
	        double multiFadi = 0;
	        double multiSuhel = 0;
	        double singleBest = 0;
	        for (int j = 0; j < rl.numOfClasses; j++) {
	          if (a2[0].equals(rl.clss[j])) {
	            if (j == 0) {
	              singleClss = 1.0;
	              countSingleClss += singleClss;
	            }
	            muliSched = 1.0;
	            countMultiSched += muliSched;
	            multiFadi = (double) rl.clssOcc[j] / rl.clssOcc[0];
	            countMultiFadi += multiFadi;
	            multiSuhel = (double) rl.clssOcc[j] / rl.allOcc;
	            countMultiSuhel += multiSuhel;
	            if (j == bestJi) {
	              singleBest = 1.0;
	              countSingleBest += singleBest;
	            }
	            break;
	          }
	          ;
	        }
	        out2.write("" + bestClass + "\t");
	        out2.write("" + singleClss + "\t" + muliSched + "\t" + multiFadi + "\t" +
	                   multiSuhel + "\t" + singleBest + "\t");
	        out2.write("\t");
	        int space = 12;
	        for (int j = 0; j < rl.numOfClasses; j++) {
	          out2.write(rl.clss[j] + "\t");
	          space--;
	        }
	        //added by new suhel add
	        for (; space > 0; space--)
	          out2.write("\t");
	        for (int j = 0; j < rl.numOfClasses; j++) {
	          out2.write(rl.clssOcc[j] + "\t");
	        }
	        out2.write("\n");
	      }
	      out2.write("singlePred\t");
	      out2.write("" + countSingleClss + "\n");
	      out2.write("multiSched\t");
	      out2.write("" + countMultiSched + "\n");
	      out2.write("multiFadi\t");
	      out2.write("" + countMultiFadi + "\n");
	      out2.write("multiSuhel\t");
	      out2.write("" + countMultiSuhel + "\n");
	      out2.write("singleBest\t");
	      out2.write("" + countSingleBest + "\n");
	      out2.close();
	    }
	    public void applyToDataMineFileAndSaveTo(String testFileName,
	                                             String resultFile) throws
	        IOException {
	      DataMine testDataMine = new DataMine(testFileName);
	      applyToDatamineAndSaveTo(testDataMine, resultFile);
	    }
	    public void applyToDataMineFileAndSaveTo2(String testFileName,
	                                              String resultFile) throws
	        IOException {
	      DataMine testDataMine = new DataMine(testFileName);
	      applyToDatamineAndSaveTo2(testDataMine, resultFile);
	
	    }
	    /**
	     * build the classifier
	     * @return
	     * @throws IOException
	     */
	    public Set checkRules() throws IOException {
	      if (rankedRuleSet.size() == 0)return new HashSet();
	      Set deletedLines = new HashSet();
	      int sz = 0;
	      Iterator itr = rankedRuleSet.entrySet().iterator();
	      Map.Entry e = (Map.Entry) itr.next();
	      long[] a = (long[]) e.getValue();
	//    while (itr.hasNext()) {
	//      Map.Entry e=(Map.Entry)itr.next();
	//      long[] a=(long[])e.getValue();
	      Column clmn = (Column) dm.existingColumns.get(new Long(a[0]));
	      Sccl itm = (Sccl) clmn.items.get(new Integer( (int) a[1]));
	      String cls = itm.classId;
	      TreeSet ts = new TreeSet(itm.lines);
	      ts.retainAll( (Set) dm.classCol.items.get(cls));
	      ts.removeAll(deletedLines);
	      if (ts.size() == 0)return new HashSet();
	      addRule(a[0], (int) a[1], ts.size(), cls);
	      deletedLines.addAll(ts);
	//    }
	
	      accOcc += deletedLines.size(); //+addDefaultClass(deletedLines)
	
	      return deletedLines;
	    }
	
	    public Set checkRules2() throws IOException {
	      Set deletedLines = new HashSet();
	      int sz = 0;
	      //to test the order of the ranked rules
	      System.out.println(printRankedRuls());
	//    System.out.println("inside Ckeck rules ,All lines :"+ dm.allLines);
	      // end test
	      Iterator itr = rankedRuleSet.entrySet().iterator();
	      while (itr.hasNext()) {
	        Map.Entry e = (Map.Entry) itr.next();
	        long[] a = (long[]) e.getValue();
	        //JOptionPane.showMessageDialog(null,"ranked col" +a[0]+" "+a[1]);
	        Column clmn = (Column) dm.existingColumns.get(new Long(a[0]));
	        Sccl itm = (Sccl) clmn.items.get(new Integer( (int) a[1]));
	        //JOptionPane.showMessageDialog(null,"clmnId" +clmn.columnId);
	        //JOptionPane.showMessageDialog(null,"itm lines " +itm.lines);
	        String cls = itm.classId;
	        TreeSet ts = new TreeSet(itm.lines);
	        ts.retainAll( (Set) dm.classCol.items.get(cls));
	        ts.removeAll(deletedLines);
	        if (ts.size() == 0)continue;
	        addRule(a[0], (int) a[1], ts.size(), cls);
	        //JOptionPane.showMessageDialog(null,"a"+a[0]+"\t"+a[1]);
	        deletedLines.addAll(ts);
	      }
	      //JOptionPane.showMessageDialog(null,"DELETED lINES ="+ deletedLines.size());
	
	      accOcc += deletedLines.size(); //+addDefaultClass(deletedLines)
	
	      return deletedLines;
	    }
	
	    public int addDefaultClass(Set ts2) {
	
	      Map tm = new HashMap();
	      Set ts = new HashSet(dm.allLines);
	      ts.removeAll(ts2);
	      Iterator itr = ts.iterator();
	      while (itr.hasNext()) {
	        String stcls = dm.classArray[ ( (Integer) itr.next()).intValue()];
	        Integer freq = (Integer) tm.get(stcls);
	        tm.put(stcls,
	               (freq == null ? new Integer(1) : new Integer(freq.intValue() + 1)));
	      }
	      // calculate the max confidence
	      String maxClass = " ";
	      int maxInt = 0;
	      for (Iterator i = tm.entrySet().iterator(); i.hasNext(); ) {
	        Map.Entry e = (Map.Entry) i.next();
	        int j = ( (Integer) e.getValue()).intValue();
	        if (j > maxInt) {
	          maxInt = j;
	          maxClass = (String) e.getKey();
	        }
	      }
	      String[] cols = new String[numOfCols];
	      for (int j = 0; j < numOfCols; j++) {
	        cols[j] = "*";
	      }
	      int allOcc = maxInt;
	      int numOfClasses = 1;
	      String[] clss = new String[100];
	      clss[0] = maxClass;
	      int[] clssOcc = new int[100];
	      clssOcc[0] = maxInt;
	      Rule rl = new Rule(numOfCols,
	                         cols,
	                         allOcc,
	                         numOfClasses,
	                         clss,
	                         clssOcc);
	      addRule(rl);
	      return maxInt;
	    }
	    public void iterate2(double minRemainInst, double conf, double supp,
	                         int numOfItr) throws IOException {
	//  JOptionPane.showMessageDialog(null,"iterate "+numOfItr);
	      minOcc = (int) Math.round(orgSize * supp + 0.5);
	      int minRemainInstOcc = (int) Math.round(orgSize * minRemainInst + 0.5);
	      System.out.print("\nMInimum (inside Iterate)" + minOcc);
	      int RemainInstOcc = orgSize;
	      Set deletedRows = new HashSet();
	      int iter = 0;
	      while (iter < numOfItr) {
	        iter++;
	        int te = RemainInstOcc;
	        rankedRuleSet.clear();
	        dm.generateColumns(supp, conf);
	        deletedRows = checkRules2();
	        RemainInstOcc -= deletedRows.size();
	        dm.delLines(deletedRows);
	      }
	      addDefaultClass(deletedRows);
	    }
	
	    public void iterate(double minRemainInst, double conf, double supp,
	                        int numOfItr) throws IOException {
	//   JOptionPane.showMessageDialog(null,"iterate "+numOfItr);
	      minOcc = (int) Math.round(orgSize * supp + 0.5);
	      int minRemainInstOcc = (int) Math.round(orgSize * minRemainInst);
	      int RemainInstOcc = orgSize;
	      Set deletedRows = new HashSet();
	      int iter = 1;
	      int te = RemainInstOcc;
	      ///
	      idis.clear();
	      rankedRuleSet.clear();
	      dm.generateColumns(supp, conf);
	      deletedRows = checkRules();
	      RemainInstOcc -= deletedRows.size();
	      dm.delLines(deletedRows);
	      int rem = dm.TOTAL_ENTITIES;
	      System.out.println(" 1  lines remained :" + rem + "\titeration :" + iter +
	                         "\trankedRules :" + rankedRuleSet.size());
	      if (minRemainInstOcc >= dm.TOTAL_ENTITIES) {
	        JOptionPane.showMessageDialog(null, "l   ast iteratiom " + iter);
	      }
	      deletedRows.clear();
	      ///
	      while (true) {
	        iter++;
	        rankedRuleSet.clear();
	        dm.generateColumnsNew();
	        deletedRows = checkRules();
	        RemainInstOcc -= deletedRows.size();
	        dm.delLines(deletedRows);
	        rem = dm.TOTAL_ENTITIES;
	        System.out.println("lines remained :" + rem + "\titeration :" + iter +
	                           "\trankedRules :" + rankedRuleSet.size());
	        if (minRemainInstOcc >= dm.TOTAL_ENTITIES) {
	          JOptionPane.showMessageDialog(null, "last iteratiom " + iter);
	          break;
	        }
	        deletedRows.clear();
	//      if (te==RemainInstOcc)return;
	      }
	      //   accOcc+=
	      System.out.print("\nRemain lines" + dm.allLines);
	      addDefaultClass(deletedRows);
	
	    }
	
	    void setRulesDataMine() {
	      Map entts = new HashMap(mp.size() + 100);
	      String[] aClass = new String[mp.size() + 1];
	      for (int i = 0; i < al.size(); i++) {
	        String[] row = new String[numOfCols + 1];
	        Rule rl = (Rule) mp.get( (String) al.get(i));
	        System.arraycopy(rl.cols, 0, row, 1, numOfCols);
	        row[0] = rl.getId();
	        aClass[i + 1] = rl.getId();
	        entts.put(new Integer(i + 1), row);
	      }
	      rulesDataMine = new DataMine(entts, aClass);
	      rulesDataMine.generateOccuranceColumns();
	    }
	    public void applyToDatamineAndSaveTo2(DataMine testDm, String desFile) throws
	        IOException {
	      setRulesDataMine();
	      pc.clear();
	      //   System.out.println(rulesDataMine.printDataMine());
	      int counter = 0;
	      Iterator iter = testDm.entity.entrySet().iterator();
	      while (iter.hasNext()) {
	        Map.Entry e = (Map.Entry) iter.next();
	        TreeSet remainRules = new TreeSet(rulesDataMine.entity.keySet());
	        String[] row = (String[]) e.getValue();
	        String condit = "";
	        for (int cd = 1; cd <= numOfCols; cd++) { //for test to be deleted later
	          condit += row[cd] + " ";
	        }
	        for (int i = 1; i <= numOfCols; i++) {
	          long intCol = (long) Math.pow(2, i - 1);
	          TreeSet ts = new TreeSet();
	          ts.addAll( (Set) rulesDataMine.getValueOccurancesInColumn(row[i],
	              intCol));
	          ts.addAll( (Set) rulesDataMine.getValueOccurancesInColumn("*", intCol));
	          remainRules.retainAll(ts);
	        }
	        Rule rl = pickARule(remainRules);
	        pc.put( (Integer) e.getKey(), rl);
	        counter++;
	      }
	      destDm = testDm;
	    }
	    /*  Rule pickARule(TreeSet PRemainRules){
	        ///
	        int lastDefaultLine=-1;
	        if(PRemainRules.size()>1)
	          lastDefaultLine=((Integer)PRemainRules.last()).intValue();
	//    System.out.println("remain new:"+PRemainRules);
	        Map tm= new HashMap();
	        Iterator itr=PRemainRules.iterator();
	        while( itr.hasNext()){
	          int line=((Integer)itr.next()).intValue();
	          if(line==lastDefaultLine)continue;
	          String ruleId=(String)al.get(line-1);
	          Rule rl=(Rule)mp.get(ruleId);
	      //    String stcls;
	          for(int i=0; i<rl.numOfClasses; i++){
	            String stcls=rl.clss[i];
	            Integer freq=(Integer)tm.get(stcls);
	            if(freq==null){
	              tm.put(stcls,new Integer(rl.clssOcc[i]));
	            }else{
	              tm.put(stcls,new Integer(freq.intValue()+rl.clssOcc[i]));
	            }
	          }
	        }
	        // calculate the max class
	        String[] sortClass=new String[tm.size()] ;
	        int[] sortOcc=new int[tm.size()];
	        //fill array with the values from tm
	        int allOcc=0;
	        int index=0;
	        Iterator iter=tm.entrySet().iterator();
	        while(iter.hasNext()){
	          Map.Entry e=(Map.Entry)iter.next();
	          sortClass[index]=(String)e.getKey();
	          sortOcc[index]=((Integer)e.getValue()).intValue();
	          allOcc+=((Integer)e.getValue()).intValue();
	          index++;
	        }
	        ///buble sort according to the occoransces
	        for(int i=0; i<sortOcc.length; i++){
	          for(int j=0; j<sortOcc.length-1;j++){
	            if(sortOcc[j+1]>sortOcc[j]){
	              String tempS=sortClass[j];
	              sortClass[j]=sortClass[j+1];
	              sortClass[j+1]=tempS;
	              int tempI=sortOcc[j];
	              sortOcc[j]=sortOcc[j+1];
	              sortOcc[j+i]=tempI;
	            }//end if
	          }//end for 1
	        }//enf for 2
	        Rule resultRule=new Rule(numOfCols,new String[numOfCols],allOcc,
	                                    sortClass.length,sortClass,sortOcc);
	        return resultRule;
	      }
	     */
	    public StringBuffer returnPredictedClass(DataMine dm3) throws IOException {
	      return new StringBuffer();
	    }
	    public StringBuffer applyToDatamine(DataMine testDataMine) throws
	        IOException {
	      return new StringBuffer();
	    }
	    public void applyToDatamineAndSaveToOld(DataMine testDm, String desFile) throws
	        IOException {
	      setRulesDataMine();
	      pc.clear();
	      //   System.out.println(rulesDataMine.printDataMine());
	      int counter = 0;
	      Iterator iter = testDm.entity.entrySet().iterator();
	      while (iter.hasNext()) {
	        Map.Entry e = (Map.Entry) iter.next();
	        TreeSet remainRules = new TreeSet(rulesDataMine.entity.keySet());
	        String[] row = (String[]) e.getValue();
	        String condit = "";
	        for (int cd = 1; cd <= numOfCols; cd++) { //for test to be deleted later
	          condit += row[cd] + " ";
	        }
	        for (int i = 1; i <= numOfCols; i++) {
	          long intCol = (long) Math.pow(2, i - 1);
	          TreeSet ts = new TreeSet();
	          ts.addAll( (Set) rulesDataMine.getValueOccurancesInColumn(row[i],
	              intCol));
	          ts.addAll( (Set) rulesDataMine.getValueOccurancesInColumn("*", intCol));
	          remainRules.retainAll(ts);
	        }
	        Rule rl = pickARuleOld(remainRules);
	        pc.put( (Integer) e.getKey(), rl);
	        counter++;
	      }
	      destDm = testDm;
	
	    }
	    public void applyToDataMineFileAndSaveToOld(String testFileName,
	                                                String resultFile) throws
	        IOException {
	      DataMine testDataMine = new DataMine(testFileName);
	      applyToDatamineAndSaveToOld(testDataMine, resultFile);
	
	    }
	    Rule pickARule(TreeSet PRemainRules) {
	      int line = ( (Integer) PRemainRules.first()).intValue();
	      String ruleId = (String) al.get(line - 1);
	      Rule rl = (Rule) mp.get(ruleId);
	      return rl;
	    }
	    Rule pickARuleOld(TreeSet PRemainRules) {
	      int line = ( (Integer) PRemainRules.first()).intValue();
	      String ruleId = (String) al.get(line - 1);
	      Rule rl = (Rule) mp.get(ruleId);
	      return rl;
	    }
	///
	    double[] getAccuracy(DataMine testDataMine) {
	      double[] resultArray = new double[5];
	      double countSingleClss = 0;
	      double countMultiSched = 0;
	      double countMultiFadi = 0;
	      double countMultiSuhel = 0;
	      double countSingleBest = 0;
	      Iterator iter = testDataMine.allLines.iterator();
	      while (iter.hasNext()) {
	        Integer lineNum = (Integer) iter.next();
	        String[] LineValues = (String[]) testDataMine.entity.get(lineNum);
	        Rule rl = (Rule) pc.get(lineNum);
	        //Rule rl=(Rule)mp.get(rlId);
	        String bestClass = rl.clss[0];
	        int bestOcc = rl.clssOcc[0];
	        int bestJi = 0;
	        for (int ji = 1; ji < rl.numOfClasses; ji++) {
	          if (rl.clssOcc[ji] > bestOcc) {
	            bestClass = rl.clss[ji];
	            bestOcc = rl.clssOcc[ji];
	            bestJi = ji;
	          }
	        }
	        boolean b = false;
	        double dd = 0;
	        double singleClss = 0;
	        double muliSched = 0;
	        double multiFadi = 0;
	        double multiSuhel = 0;
	        double singleBest = 0;
	        String currentClass = LineValues[0];
	        for (int j = 0; j < rl.numOfClasses; j++) {
	          if (currentClass.equals(rl.clss[j])) {
	            if (j == 0) {
	              singleClss = 1.0;
	              countSingleClss += singleClss;
	            }
	            muliSched = 1.0;
	            countMultiSched += muliSched;
	            multiFadi = (double) rl.clssOcc[j] / rl.clssOcc[0];
	            countMultiFadi += multiFadi;
	            multiSuhel = (double) rl.clssOcc[j] / rl.allOcc;
	            countMultiSuhel += multiSuhel;
	            if (j == bestJi) {
	              singleBest = 1.0;
	              countSingleBest += singleBest;
	            }
	            break;
	          }
	        }
	      }
	      double allLinesSize = (double) testDataMine.allLines.size();
	      resultArray[0] = countSingleClss / allLinesSize;
	      resultArray[1] = countMultiSched / allLinesSize;
	      resultArray[2] = countMultiFadi / allLinesSize;
	      resultArray[3] = countMultiSuhel / allLinesSize;
	      resultArray[4] = countSingleBest / allLinesSize;
	      return resultArray;
	    }
	
	    //extentions added later to check the classifier
	    public String prntCounterMap(String conName) {
	      String result = "";
	      SCounter tsc = (SCounter) counterMap.get(conName);
	
	      if (tsc == null)return "\nno" + conName + "counter found.";
	      else
	        result = "\nSCounter :" + conName + "\tsize =" + getCounterSize(conName) +
	            "\tCounter Hits = " + getSCounter(conName).getHitCounter() + "\n" +
	            tsc.toString();
	
	      return result;
	    }
	    public String prntCounterMaps() {
	      String result = "";
	      result += prntCounterMap("confidence");
	      result += prntCounterMap("support");
	      result += prntCounterMap("numOfAttribs");
	      result += prntCounterMap("occ_orgSize");
	      result += prntCounterMap("rndCol");
	      result += prntCounterMap("rndRow");
	
	      /*
	           Iterator itr =counterMap.entrySet().iterator();
	           while (itr.hasNext()){
	        Map.Entry me =(Map.Entry) itr.next();
	        String ts=(String)me.getKey();
	        result += prntCounterMap(ts);
	           }
	       */
	      return result;
	    }
	
	    public void resetCounters() {
	      Iterator itr = counterMap.values().iterator();
	      while (itr.hasNext()) {
	        SCounter tsc = (SCounter) itr.next();
	        tsc.reset();
	      }
	    }
	
	    public int getCounterSize(String name) {
	      SCounter tsc = (SCounter) counterMap.get(name);
	      return tsc.getSize();
	    }
	
	    public String prntCounterSizes() {
	      String result = "\n\n Counters Sizes";
	      /*
	         Iterator itr =counterMap.entrySet().iterator();
	         while (itr.hasNext()){
	           Map.Entry me =(Map.Entry) itr.next();
	           String ts=(String)me.getKey();
	
	           result += "\nSCounter :" + ts + "\tsize ="+  getCounterSize(ts) ;
	         }*/
	      result += "\nSCounter : confidence \tsize =" +
	          getCounterSize("confidence") +
	          "\tHits =" + getSCounter("confidence").getHitCounter() + "\tEquals= " +
	          getSCounter("confidence").getEqualsCounter();
	      result += "\nSCounter : support\tsize =" + getCounterSize("support") +
	          "\tHits =" + (getSCounter("support")).getHitCounter() + "\tEquals= " +
	          getSCounter("support").getEqualsCounter();
	      result += "\nSCounter : numOfAttribs\tsize =" +
	          getCounterSize("numOfAttribs") +
	          "\tHits =" + getSCounter("numOfAttribs").getHitCounter() +
	          "\tEquals= " +
	          getSCounter("numOfAttribs").getEqualsCounter();
	      result += "\nSCounter : occ_orgSize\tsize =" +
	          getCounterSize("occ_orgSize") +
	          "\tHits =" + getSCounter("occ_orgSize").getHitCounter() +
	          "\tEquals= " +
	          getSCounter("occ_orgSize").getEqualsCounter();
	      result += "\nSCounter : rndCol\tsize =" + getCounterSize("rndCol") +
	          "\tHits ="
	          + getSCounter("rndCol").getHitCounter() + "\tEquals= " +
	          getSCounter("rndCol").getEqualsCounter();
	      result += "\nSCounter : rndRow\tsize =" +
	          getCounterSize("rndRow") + "\tHits =" +
	          getSCounter("rndRow").getHitCounter() + "\tEquals= " +
	          getSCounter("rndRow").getEqualsCounter();
	
	      return result;
	
	    }
	
	    SCounter getSCounter(String scntr) {
	      return (SCounter) counterMap.get(scntr);
	    }
	    ;
	
	  }
