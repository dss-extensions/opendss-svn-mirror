//  ----------------------------------------------------------
//  Copyright (c) 2009, Electric Power Research Institute, Inc.
//  All rights reserved.
//  ----------------------------------------------------------

// package epri.com.opendss.cim ;

// JENA API
import java.io.*;

import com.hp.hpl.jena.ontology.*;
import com.hp.hpl.jena.query.*;
import com.hp.hpl.jena.rdf.model.*;
import com.hp.hpl.jena.util.FileManager;

public class CDPSM_to_DSS extends Object {
  static final String nsCIM = "http://iec.ch/TC57/2009/CIM-schema-cim14#";
  static final String nsRDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
  static final String baseURI = "http://opendss";

  static final String balancedOwl = "CDPSM_Balanced.owl";
  static final String unbalancedOwl = "CDPSM-unbalanced.owl";
  static final String connectOwl = "CDPSM-connectivity.owl";

  static String SafeProperty (Resource r, Property p, String def) {
    if (r.hasProperty(p)) return r.getProperty(p).getString();
    return def;
  }

  static double SafeDouble (Resource r, Property p, double def) {
    if (r.hasProperty(p)) return r.getProperty(p).getDouble();
    return def;
  }

  static int SafeInt (Resource r, Property p, int def) {
    if (r.hasProperty(p)) return r.getProperty(p).getInt();
    return def;
  }

  static String DSS_Guid (String arg) {
    int hash = arg.lastIndexOf ("#_");
    return arg.substring (hash + 2);
  }

  static String DSS_Name (String arg) {
    String s1 = arg.replace (' ', '_');
    String s2 = s1.replace ('.', '_');
    String s3 = s2.replace ('(', '_');
    String s4 = s3.replace (')', '_');
    return s4.replace ('=', '_');
  }

  static String SafeResName (Resource r, Property p) {
    String s;
    if (r.hasProperty(p)) {
      s = r.getProperty(p).getString();
    } else {
      s = r.getLocalName();
    }
    return DSS_Name (s);
  }

  static String SafeResourceLookup (Model mdl, Property ptName, Resource r, Property p, String def) {
    if (r.hasProperty(p)) {
      Resource res = mdl.getResource (r.getProperty(p).getResource().toString());
      String s = SafeResName (res, ptName);
      return s;
    }
    return def;
  }

  static int GetMatIdx (int n, int row, int col) {
    int seq = -1;
    int i, j;
    for (j = 0; j < col; j++) {
      seq += (n - j);
    }
    for (i = col; i <= row; i++) {
      ++seq;
    }
    return seq;
  }

  static String GetImpedanceMatrix (Model mdl, Property ptName, Resource r) {
    int nphases, seq, size, i, j;

    Property ptCount = mdl.getProperty (nsCIM, "PerLengthPhaseImpedance.conductorCount");
    Property ptData = mdl.getProperty (nsCIM, "PhaseImpedanceData.PhaseImpedance");
    Property ptSeq = mdl.getProperty (nsCIM, "PhaseImpedanceData.sequenceNumber");
    Property ptR = mdl.getProperty (nsCIM, "PhaseImpedanceData.r");
    Property ptX = mdl.getProperty (nsCIM, "PhaseImpedanceData.x");
    Property ptB = mdl.getProperty (nsCIM, "PhaseImpedanceData.b");
    nphases = r.getProperty(ptCount).getInt();

    size = 0;
    for (i = 0; i < nphases; i++) {
      for (j = i; j < nphases; j++) {
        ++size;
      }
    }
    double [] rMat = new double [size];
    double [] xMat = new double [size];
    double [] cMat = new double [size];
    for (i = 0; i < size; i++) {
      rMat[i] = 0.0;
      xMat[i] = 0.0;
      cMat[i] = 0.0;
    }
    double len = 1.0; // 5280.0;

    ResIterator iter = mdl.listResourcesWithProperty (ptData, r);
    while (iter.hasNext()) {
      Resource rData = iter.nextResource();
      seq = rData.getProperty(ptSeq).getInt() - 1;  // zero-based arrays in Java, 1-based in CIM
      if (rData.hasProperty(ptR)) {
        rMat[seq] = len * rData.getProperty(ptR).getDouble();
      }
      if (rData.hasProperty(ptX)) {
        xMat[seq] = len * rData.getProperty(ptX).getDouble();
      }
      if (rData.hasProperty(ptB)) {
        cMat[seq] = len * rData.getProperty(ptB).getDouble() * 1.0e9 / 377.0;
      }
    }

    StringBuilder buf = new StringBuilder ("nphases=" + Integer.toString(nphases));
    StringBuilder rBuf = new StringBuilder (" rmatrix=[");
    StringBuilder xBuf = new StringBuilder (" xmatrix=[");
    StringBuilder cBuf = new StringBuilder (" cmatrix=[");

    for (i = 0; i < nphases; i++) {  // lower triangular, go across the rows for OpenDSS
      for (j = 0; j <= i; j++) {
        seq = GetMatIdx (nphases, i, j);
        rBuf.append (Double.toString (rMat[seq]) + " ");
        xBuf.append (Double.toString (xMat[seq]) + " ");
        cBuf.append (Double.toString (cMat[seq]) + " ");
      }
      if ((i+1) < nphases) {
        rBuf.append ("| ");
        xBuf.append ("| ");
        cBuf.append ("| ");
      }
    }

    buf.append (rBuf + "]");
    buf.append (xBuf + "]");
    buf.append (cBuf + "]");
    return buf.toString();
  }

  static String Phase_String (String arg) {
    int hash = arg.lastIndexOf ("#PhaseCode.");
    return arg.substring (hash + 11);
  }

  static String Phase_Conn (String arg) {
    String phs = Phase_String (arg);
    if (phs.contains("N")) {
      return "w";
    }
    int cnt = phs.length();
    if (cnt == 2) {
      return "d";
    }
    return "w";
  }

  static int Phase_Count (String arg, boolean shunt) {
    String phs = Phase_String (arg);
    if (phs.equals ("N")) {
      return 3;
    }
    int cnt = phs.length();
    if (phs.contains ("N")) {
      --cnt;
    } else if (shunt == true) { // shunt without N ==> delta, either 1 or 3 phases
      if (cnt == 2) {
        cnt = 1;
      }
    }
    return cnt;
  }

  static String Bus_Phases (String arg) {
    String phs = Phase_String (arg);
    if (phs.contains ("ABC")) {
      return ".1.2.3";
    } else if (phs.contains ("AB")) {
      return ".1.2";
    } else if (phs.contains ("AC")) {
      return ".1.3";
    } else if (phs.contains ("BC")) {
      return ".2.3";
    } else if (phs.contains ("A")) {
      return ".1";
    } else if (phs.contains ("B")) {
      return ".2";
    } else if (phs.contains ("C")) {
      return ".3";
    } else {
      return "";  // defaults to 3 phases
    }
  }

  static String GetWdgConnection (Resource r, Property p, String def) {
    if (r.hasProperty(p)) {
      String arg = r.getProperty(p).getObject().toString();
      int hash = arg.lastIndexOf ("#WindingConnection.");
      return arg.substring (hash + 19);  // TODO - change Y to W
    }
    return def;
  }

  static String GetPropValue (Model mdl, String uri, String prop) {
    Resource res = mdl.getResource (uri);
    Property p = mdl.getProperty (nsCIM, prop);
    return res.getProperty(p).getString();
  }

  static String GetLoadModel (Model mdl, Resource rLoad) {
    Property ptResponse = mdl.getProperty (nsCIM, "EnergyConsumer.LoadResponse");
    if (rLoad.hasProperty (ptResponse)) {
      Resource rModel = rLoad.getProperty(ptResponse).getResource();
      Property ptPv = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pVoltageExponent");
      Property ptQv = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qVoltageExponent");
      Property ptPz = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantImpedance");
      Property ptPi = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantCurrent");
      Property ptPp = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.pConstantPower");
      Property ptQz = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantImpedance");
      Property ptQi = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantCurrent");
      Property ptQp = mdl.getProperty (nsCIM, "LoadResponseCharacteristic.qConstantPower");
      double Pv = new Double (SafeProperty (rModel, ptPv, "0"));
      double Qv = new Double (SafeProperty (rModel, ptQv, "0"));
      double Pz = new Double (SafeProperty (rModel, ptPz, "0"));
      double Pi = new Double (SafeProperty (rModel, ptPi, "0"));
      double Pp = new Double (SafeProperty (rModel, ptPp, "0"));
      double Qz = new Double (SafeProperty (rModel, ptQz, "0"));
      double Qi = new Double (SafeProperty (rModel, ptQi, "0"));
      double Qp = new Double (SafeProperty (rModel, ptQp, "0"));
      if (Pv == 1 && Qv == 2) {
        return "model=4";
      }
      if (Pz == 100 && Qz == 100) {
        return "model=2";
      }
      if (Pp == 100 && Qz == 100) {
        return "model=3";
      }
      if (Pi == 100 && Qi == 100) {
        return "model=5";
      }
    }
    return "model=1";
  }

  static String GetBusName (Model mdl, String eq_id, int seq) {
    String strSeq = Integer.toString (seq);
    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptEquip = mdl.getProperty (nsCIM, "Terminal.ConductingEquipment");
    Property ptSeq = mdl.getProperty (nsCIM, "Terminal.sequenceNumber");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");
    Resource resID = mdl.getResource (eq_id);
    ResIterator iter = mdl.listResourcesWithProperty (ptEquip, resID);
    while (iter.hasNext()) {
      Resource res = iter.nextResource(); // this is a terminal of eq_id
      if (res.hasProperty (ptSeq, strSeq)) {  // at the end we are looking for
        Resource CN = res.getProperty(ptNode).getResource();
        if (CN.hasProperty(ptName)) {
          return CN.getProperty(ptName).getString();
        } else {
          return CN.getLocalName();
        }
      }
    }
    return "x";
  }

  static String GetXfmrBuses (Model mdl, String xf_id, String code_id) {
    Property ptCode = mdl.getProperty (nsCIM, "WindingInfo.TransformerInfo");
    Property ptXfmr = mdl.getProperty (nsCIM, "DistributionTransformerWinding.Transformer");
    Property ptWdgInfo = mdl.getProperty (nsCIM, "DistributionTransformerWinding.WindingInfo");
    Property ptPhs = mdl.getProperty (nsCIM, "ConductingEquipment.phases");
    StringBuilder buf = new StringBuilder ("[");

    // issue: listResourcesWithProperty can return wdg and wdgInfo in different order
    // strategy: iterate over wdgInfo to match order in XfmrCode, 
    //           then select wdg from this xfmr using that wdgInfo
    Resource xfRes = mdl.getResource (xf_id);
    Resource xfCode = mdl.getResource (code_id);
    ResIterator infIter = mdl.listResourcesWithProperty (ptCode, xfCode);
    while (infIter.hasNext()) {
      Resource wdgInfo = infIter.nextResource();
      ResIterator iter = mdl.listResourcesWithProperty (ptXfmr, xfRes);
      while (iter.hasNext()) {
        Resource wdg = iter.nextResource();
        // look for the wdg that uses wdgInfo
        if (wdg.hasProperty (ptWdgInfo, wdgInfo)) {
          String phs = wdg.getProperty(ptPhs).toString();
          buf.append (GetBusName (mdl, wdg.toString(), 1));
          buf.append (Bus_Phases (phs));
          if (infIter.hasNext()) {
            buf.append (",");
          } else {
            buf.append ("]");
          }
        }
      }
    }
    return buf.toString();
  }

  static String GetRegulatorData (Model mdl, Resource reg) {
    StringBuffer buf = new StringBuffer("");

    // TODO - find the winding number without using sequenceNumber
    Property ptWdg = mdl.getProperty (nsCIM, "RatioTapChanger.Winding");
    Property ptXfmr = mdl.getProperty (nsCIM, "DistributionTransformerWinding.Transformer");
    Property ptWdgInf = mdl.getProperty (nsCIM, "DistributionTransformerWinding.WindingInfo");
    Property ptWdgSeq = mdl.getProperty (nsCIM, "WindingInfo.sequenceNumber");
    Property ptName = mdl.getProperty (nsCIM, "IdentifiedObject.name");

    Resource rWdg = mdl.getProperty(reg,ptWdg).getResource();
    Resource rXf = mdl.getProperty(rWdg,ptXfmr).getResource();
    Resource rInf = mdl.getProperty(rWdg,ptWdgInf).getResource();
    String xfName = mdl.getProperty(rXf,ptName).getString();
    int nWdg = mdl.getProperty(rInf,ptWdgSeq).getInt();
    buf.append (" transformer=" + xfName + " winding=" + Integer.toString(nWdg));

    // TODO - all the other parameters
    Property ptBand = mdl.getProperty (nsCIM, "DistributionTapChanger.bandVoltage");
    Property ptSet = mdl.getProperty (nsCIM, "DistributionTapChanger.targetVoltage");
    Property ptR = mdl.getProperty (nsCIM, "DistributionTapChanger.lineDropX");
    Property ptX = mdl.getProperty (nsCIM, "DistributionTapChanger.lineDropR");
    Property ptPT = mdl.getProperty (nsCIM, "DistributionTapChanger.ptRatio");
    Property ptCT = mdl.getProperty (nsCIM, "DistributionTapChanger.ctRating");

    // TODO - pull SvTapStep

    double CT = SafeDouble (reg, ptCT, 1.0);
    double PT = SafeDouble (reg, ptPT, 1.0);
    double ldcR = SafeDouble (reg, ptR, 0.0);
    double ldcX = SafeDouble (reg, ptX, 0.0);
    double Vreg = SafeDouble (reg, ptSet, 120.0);
    double Vband = SafeDouble (reg, ptBand, 2.0);
    Vreg /= PT;
    Vband /= PT;

    buf.append (" ctprim=" + Double.toString(CT) +
                " ptratio=" + Double.toString(PT) +
                " vreg=" + Double.toString(Vreg) +
                " vband=" + Double.toString(Vband) +
                " r=" + Double.toString(ldcR) +
                " x=" + Double.toString(ldcX));
    return buf.toString();
  }

  static String GetXfmrCode (Model mdl, String id) {
    Property ptInfo = mdl.getProperty (nsCIM, "WindingInfo.TransformerInfo");
    Property ptU = mdl.getProperty (nsCIM, "WindingInfo.ratedU");
    Property ptS = mdl.getProperty (nsCIM, "WindingInfo.ratedS");
    Property ptR = mdl.getProperty (nsCIM, "WindingInfo.r");
    Property ptC = mdl.getProperty (nsCIM, "WindingInfo.connectionKind");
    Property ptFrom = mdl.getProperty (nsCIM, "DistributionWindingTest.FromWinding");
    Property ptType = mdl.getProperty (nsRDF, "type");
    Property ptTo = mdl.getProperty (nsCIM, "ShortCircuitTest.FromWinding"); // TODO - handle more than 3 windings
    Property ptNLL = mdl.getProperty (nsCIM, "OpenCircuitTest.noLoadLoss");
    Property ptImag = mdl.getProperty (nsCIM, "OpenCircuitTest.excitingCurrent");
    Property ptZsc = mdl.getProperty (nsCIM, "ShortCircuitTest.leakageImpedance");
    StringBuilder bufU = new StringBuilder ("kvs=[");
    StringBuilder bufS = new StringBuilder ("kvas=[");
    StringBuilder bufC = new StringBuilder ("conns=[");
    StringBuilder bufR = new StringBuilder ("%Rs=[");
    StringBuilder bufOC = new StringBuilder ("");
    StringBuilder bufSC = new StringBuilder ("");
    String sPhases = "phases=3 ";
    int nWindings = 0;
    int nOffset = nsRDF.length() - 2;

    Resource xfRes = mdl.getResource (id);
    ResIterator iter = mdl.listResourcesWithProperty (ptInfo, xfRes);
    while (iter.hasNext()) {
      Resource wdg = iter.nextResource();
      ++nWindings;
      double dU = SafeDouble (wdg, ptU, 1);
      double dS = SafeDouble (wdg, ptS, 1);
      double dR = SafeDouble (wdg, ptR, 0);
      double Zbase = 1000.0 * dU * dU / dS;
      dR = 100.0 * dR / Zbase;
      String U = Double.toString(dU);
      String S = Double.toString(dS);
      String R = Double.toString(dR);
      String C = GetWdgConnection (wdg, ptC, "W");
      if (C.equals ("I")) {
        sPhases = "phases=1 ";
        C = "W";
      }
      if (iter.hasNext()) {
        bufU.append (U + ",");
        bufS.append (S + ",");
        bufC.append (C + ",");
        bufR.append (R + ",");
      } else {
        bufU.append (U + "] ");
        bufS.append (S + "] ");
        bufC.append (C + "] ");
        bufR.append (R + "] ");
      }
      ResIterator iterTest = mdl.listResourcesWithProperty (ptFrom, wdg);
      while (iterTest.hasNext()) {
        Resource test = iterTest.nextResource();
        String sType = test.getProperty (ptType).getObject().toString().substring(nOffset);
        if (sType.equals("OpenCircuitTest")) {
          double dNLL = SafeDouble (test, ptNLL, 0);
          double dImag = SafeDouble (test, ptImag, 0);
          dNLL = 100 * dNLL / dS;
          bufOC.append ("%imag=" + Double.toString(dImag) + " %noloadloss=" + Double.toString(dNLL) + " ");
        } else if (sType.equals("ShortCircuitTest")) {
          double dXsc = SafeDouble (test, ptZsc, 0.0001);
          dXsc = 100.0 * dXsc / Zbase;
          bufSC.append ("Xhl=" + Double.toString(dXsc) + " ");
        }
      }
    }
    String sWindings = "windings=" + Integer.toString(nWindings) + " ";
    return sWindings + sPhases + bufSC.toString() + bufOC.toString() + bufU.toString() + bufS.toString() + bufC.toString() + bufR.toString();
  }

  static String GetBusPositionString (Model mdl, String id) {
    Property ptX = mdl.getProperty (nsCIM, "PositionPoint.xPosition");
    Property ptY = mdl.getProperty (nsCIM, "PositionPoint.yPosition");
    Property ptPosSeq = mdl.getProperty (nsCIM, "PositionPoint.sequenceNumber");
    Property ptLoc = mdl.getProperty (nsCIM, "PositionPoint.Location");
    Property ptGeo = mdl.getProperty (nsCIM, "PowerSystemResource.GeoLocation");

    Property ptBank = mdl.getProperty (nsCIM, "DistributionTransformer.TransformerBank");
    Property ptXfmr = mdl.getProperty (nsCIM, "DistributionTransformerWinding.Transformer");

    Property ptNode = mdl.getProperty (nsCIM, "Terminal.ConnectivityNode");
    Property ptTrmSeq = mdl.getProperty (nsCIM, "Terminal.sequenceNumber");
    Property ptEquip = mdl.getProperty (nsCIM, "Terminal.ConductingEquipment");

    Resource bus = mdl.getResource (id);
    Resource trm, eq;
    String trmSeq = "1";

    Resource geo = null;
    Resource bankGeo = null;

    // first look for a terminal equipment that directly has a GeoLocation
    ResIterator terms = mdl.listResourcesWithProperty (ptNode, bus);
    while (terms.hasNext() && geo == null) {
      trm = terms.nextResource();
      eq = trm.getProperty(ptEquip).getResource();
      if (eq.hasProperty (ptGeo)) {
        geo = eq.getProperty(ptGeo).getResource();
        trmSeq = trm.getProperty(ptTrmSeq).getString();
      } else if (eq.hasProperty (ptXfmr)) {
        Resource xf = eq.getProperty (ptXfmr).getResource();
        if (xf.hasProperty (ptBank)) {
          Resource bank = xf.getProperty(ptBank).getResource();
          if (bank.hasProperty (ptGeo)) {
            bankGeo = bank.getProperty(ptGeo).getResource();
            trmSeq = "1"; // because winding terminals always exported with sequence number 1
          }
        }
      }
    }
    if (geo == null) {
      geo = bankGeo;
    }

    if (geo != null) {
      ResIterator iter = mdl.listResourcesWithProperty (ptLoc, geo);
      Resource pos = null;
      while (iter.hasNext()) {
        pos = iter.nextResource();
        if (pos.hasProperty (ptPosSeq, trmSeq)) { // at the end we are looking for
          return pos.getProperty(ptX).getString() + ", " + pos.getProperty(ptY).getString();
        }
      }
      if (pos != null) {
        return pos.getProperty(ptX).getString() + ", " + pos.getProperty(ptY).getString();
      }
    }

    return "";
  }

  public static void main (String args[]) throws UnsupportedEncodingException, FileNotFoundException {

    if (args.length != 3) {
      System.out.println ("Usage: CDPSM_to_DSS -{c|u|b} input.xml output_root");
    }
    String fProfile = "";
    String fName = args[1];
    String fOut = args[2] + "_base.dss";
    String fBus = args[2] + "_busxy.dss";
    String fGuid = args[2] + "_guids.dss";

    if (args[0].equals("-c")) {
      fProfile = connectOwl;
    } else if (args[0].equals("-b")) {
      fProfile = balancedOwl;
    } else if (args[0].equals("-u")) {
      fProfile = unbalancedOwl;
    }

    ModelMaker maker = ModelFactory.createFileModelMaker (fProfile);
    Model tmpModel = maker.createDefaultModel();
    Model model = ModelFactory.createOntologyModel (OntModelSpec.OWL_DL_MEM, tmpModel);
       
    InputStream in = FileManager.get().open(fName);
    if (in == null) {
      throw new IllegalArgumentException( "File: " + fName + " not found");
    }
        
    PrintWriter out = new PrintWriter (fOut);
    PrintWriter outBus = new PrintWriter (fBus);
    PrintWriter outGuid = new PrintWriter (fGuid);

    model.read(new InputStreamReader(in, "UTF8"), baseURI, "RDF/XML");
        
    String qPrefix = "PREFIX r: <" + nsRDF + "> PREFIX c: <" + nsCIM + "> ";
    Query query;
    QueryExecution qexec;
    ResultSet results;
    QuerySolution soln;
    Resource res;
    String id, name, phs, bus_phs, bus1, bus2, phs_conn;
    int phs_cnt;
    Property ptName = model.getProperty (nsCIM, "IdentifiedObject.name");
    Property ptOpen = model.getProperty (nsCIM, "Switch.normalOpen");

    // ConnectivityNode ==> bus coordinate CSV 
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:ConnectivityNode}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      String strPos = GetBusPositionString (model, id);
      if (strPos.length() > 0) {
        outBus.println (name + ", " + strPos);
      } else {
        outBus.println (name + ", *****");
      }
    }
    outBus.println ();
    outBus.close ();
    
    // EnergySource ==> Circuit
    int NumCircuits = 0;
    int NumSources = 0;

    out.println ("clear");
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs ?v ?ckt where {?s r:type c:EnergySource. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:EnergySource.nominalVoltage ?v;" +
                                 "   c:Equipment.EquipmentContainer ?ckt" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      ++NumSources;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String vSrce = soln.get ("?v").toString();
      String ckt = soln.get ("?ckt").toString();

      phs_cnt = Phase_Count (phs, false);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      String srcClass = "Vsource.";
      if (NumCircuits < 1) { // name.equals ("source")
        srcClass = "Circuit.";
        name = GetPropValue (model, ckt, "IdentifiedObject.name");
        NumCircuits = 1;
      }

      out.println ("new " + srcClass + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " basekv=" + vSrce);
      outGuid.println (srcClass + name + "\t" + DSS_Guid (id));
    }
    if (NumCircuits < 1) {  // try the first breaker
      query = QueryFactory.create (qPrefix + "select ?s ?phs where {?s r:type c:Breaker. " + 
                                   "?s c:ConductingEquipment.phases ?phs}");
      qexec = QueryExecutionFactory.create (query, model);
      results=qexec.execSelect();
      while (results.hasNext()) {
        soln = results.next();
        id = soln.get ("?s").toString();

        phs = soln.get ("?phs").toString();
        phs_cnt = Phase_Count (phs, false);
        bus_phs = Bus_Phases (phs);
        bus1 = GetBusName (model, id, 1) + bus_phs;

        res = model.getResource (id);
        name = SafeResName (res, ptName);
        out.println ("new Circuit." + name + " phases=" + Integer.toString(phs_cnt) + 
                     " bus1=" + bus1 + " basekv=1");
//        outGuid.println ("Circuit." + name + "\t" + DSS_Guid (id));
      }
    }

    // EnergyConsumer ==> Load
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?phs where {?s r:type c:EnergyConsumer. " + 
                                 "?s c:ConductingEquipment.phases ?phs}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptP = model.getProperty (nsCIM, "EnergyConsumer.pfixed");
    Property ptQ = model.getProperty (nsCIM, "EnergyConsumer.qfixed");
    Property ptCust = model.getProperty (nsCIM, "EnergyConsumer.customerCount");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      phs = soln.get ("?phs").toString();

      phs_cnt = Phase_Count (phs, true);
      phs_conn = Phase_Conn (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      res = model.getResource (id);
      name = SafeResName (res, ptName);
      String pLoad = SafeProperty (res, ptP, "1.0");
      String qLoad = SafeProperty (res, ptQ, "0.0");
      String nCust = SafeProperty (res, ptCust, "1");
      String loadModel = GetLoadModel (model, res);

      out.println ("new Load." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " conn=" + phs_conn + " kw=" + pLoad + " kvar=" + qLoad + " numcust=" + nCust + 
                   " kv=1 " + loadModel);
      outGuid.println ("Load." + name + "\t" + DSS_Guid (id));
    }

    // ShuntCompensator ==> Capacitor
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs where {?s r:type c:ShuntCompensator. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptNomQ = model.getProperty (nsCIM, "ShuntCompensator.nomQ");
    Property ptNomU = model.getProperty (nsCIM, "ShuntCompensator.nomU");
    Property ptNumSteps = model.getProperty (nsCIM, "ShuntCompensator.maximumSections");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();

      res = model.getResource (id);
      String nomQ = SafeProperty (res, ptNomQ, "0.1");
      String nomU = SafeProperty (res, ptNomU, "1.0");
      String numSteps = SafeProperty (res, ptNumSteps, "1");

      phs_cnt = Phase_Count (phs, true);
      bus_phs = Bus_Phases (phs);
      phs_conn = Phase_Conn (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      out.println ("new Capacitor." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " conn=" + phs_conn + " numsteps=" + numSteps + " kv=" + nomU + " kvar=" + nomQ);
      outGuid.println ("Capacitor." + name + "\t" + DSS_Guid (id));
    }


    // WireData
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s where {?s r:type c:WireType}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptGMR = model.getProperty (nsCIM, "WireType.gmr");
    Property ptWireRadius = model.getProperty (nsCIM, "WireType.radius");
    Property ptWireDiameter = model.getProperty (nsCIM, "WireType.diameter");
    Property ptWireCurrent = model.getProperty (nsCIM, "WireType.ratedCurrent");
    Property ptWireR25 = model.getProperty (nsCIM, "WireType.rAC25");
    Property ptWireR50 = model.getProperty (nsCIM, "WireType.rAC50");
    Property ptWireR75 = model.getProperty (nsCIM, "WireType.rAC75");
    Property ptWireRdc = model.getProperty (nsCIM, "WireType.rDC20");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      res = model.getResource (id);
      name = SafeResName (res, ptName);

      double normamps = SafeDouble (res, ptWireCurrent, 0.0);

      double radius = SafeDouble (res, ptWireRadius, 0.0);
      if (radius <= 0) {
        radius = 0.5 * SafeDouble (res, ptWireDiameter, 0.0);
      }

      double gmr = SafeDouble (res, ptGMR, 0.0);
      if (gmr <= 0) {
        gmr = 0.7788 * radius;
      }

      double wireRac = SafeDouble (res, ptWireR50, 0.0);
      if (wireRac <= 0) {
        wireRac = SafeDouble (res, ptWireR25, 0.0);
      }
      if (wireRac <= 0) {
        wireRac = SafeDouble (res, ptWireR75, 0.0);
      }
      double wireRdc = SafeDouble (res, ptWireRdc, 0.0);
      if (wireRdc <= 0) {
        wireRdc = wireRac;
      } else if (wireRac <= 0) {
        wireRac = wireRdc;
      }

      out.println ("new WireData." + name + " gmr=" + Double.toString(gmr) + " radius=" + Double.toString(radius) +
                   " rac=" + Double.toString(wireRac) + " rdc=" + Double.toString(wireRdc) + " normamps=" + Double.toString(normamps) + 
                   " Runits=kft Radunits=in gmrunits=in");
      outGuid.println ("WireData." + name + "\t" + DSS_Guid (id));
    }

    // LineGeometries
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:OverheadConductorInfo. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptWireX = model.getProperty (nsCIM, "WireArrangement.mountingPointX");
    Property ptWireY = model.getProperty (nsCIM, "WireArrangement.mountingPointY");
    Property ptWireP = model.getProperty (nsCIM, "WireArrangement.position");
    Property ptWireType = model.getProperty (nsCIM, "WireArrangement.WireType");
    Property ptWireInfo = model.getProperty (nsCIM, "WireArrangement.ConductorInfo");
    Property ptGeoPhases = model.getProperty (nsCIM, "ConductorInfo.phaseCount");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);

      int nconds = 0;
      int icond;
      String wireType, wireX, wireY;
      StringBuffer buf = new StringBuffer();
      int nphases = SafeInt (res, ptGeoPhases, 1);
      ResIterator wIter = model.listResourcesWithProperty (ptWireInfo, res);
      while (wIter.hasNext()) {
        Resource wa = wIter.nextResource();
        icond = SafeInt (wa, ptWireP, 1);
        if (icond > nconds) {
          nconds = icond;
        }
        wireX = SafeProperty (wa, ptWireX, "0");
        wireY = SafeProperty (wa, ptWireY, "0");
        wireType = SafeResourceLookup (model, ptName, wa, ptWireType, "**");
        buf.append ("~ cond=" + Integer.toString(icond) + " wire=" + wireType + " x=" + wireX + " y=" + wireY + "\n");
      }

      out.println ("new LineGeometry." + name + " nconds=" + Integer.toString(nconds) + " nphases=" + Integer.toString(nphases) + 
                   " reduce=y units=ft");
      out.println (buf.toString());
      outGuid.println ("LineGeometry." + name + "\t" + DSS_Guid (id));
    }

    // LineCodes
    int NumLineCodes = 0;
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PerLengthPhaseImpedance. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      ++NumLineCodes;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      String zMat = GetImpedanceMatrix (model, ptName, res);

      out.println ("new LineCode." + name + " " + zMat);
      outGuid.println ("LineCode." + name + "\t" + DSS_Guid (id));
    }
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PerLengthSequenceImpedance. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptSeqR1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.r");
    Property ptSeqR0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.r0");
    Property ptSeqX1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.x");
    Property ptSeqX0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.x0");
    Property ptSeqB1 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.bch");
    Property ptSeqB0 = model.getProperty (nsCIM, "PerLengthSequenceImpedance.b0ch");
    while (results.hasNext()) {
      soln = results.next();
      ++NumLineCodes;

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);

      double sqR1 = SafeDouble (res, ptSeqR1, 0);
      double sqR0 = SafeDouble (res, ptSeqR0, 0);
      double sqX1 = SafeDouble (res, ptSeqX1, 0);
      double sqX0 = SafeDouble (res, ptSeqX0, 0);
      if (sqR0 <= 0) {
        sqR0 = sqR1;
      }
      if (sqX0 <= 0) {
        sqX0 = sqX1;
      }
      String seqR1 = Double.toString(sqR1);
      String seqR0 = Double.toString(sqR0);
      String seqX1 = Double.toString(sqX1);
      String seqX0 = Double.toString(sqX0);

      double bch = SafeDouble (res, ptSeqB1, 0);
      String seqC1 = Double.toString(bch * 1.0e9 / 314.0);  // TODO: only for EdF during 2009 interop tests
      bch = SafeDouble (res, ptSeqB0, 0);
      String seqC0 = Double.toString(bch * 1.0e9 / 314.0);  // TODO: only for EdF during 2009 interop tests

      out.println ("new LineCode." + name + " nphases=3 r1=" + seqR1 + " x1=" + seqX1 + " c1=" + seqC1 +
                   " r0=" + seqR0 + " x0=" + seqX0 + " c0=" + seqC0);
      outGuid.println ("LineCode." + name + "\t" + DSS_Guid (id));
    }
    if (NumLineCodes < 1) {
      out.println ("new LineCode.dummy_linecode_1 nphases=1 rmatrix={0} xmatrix={0.001} cmatrix={0}");
      out.println ("new LineCode.dummy_linecode_2 nphases=2 rmatrix={0|0 0} xmatrix={0.001|0 0.001} cmatrix={0|0 0}");
      out.println ("new LineCode.dummy_linecode_3 nphases=3 r1=0 x1=0.001 c1=0 r0=0 x0=0.001 c0=0");
      // TODO - do we want this GUID or not?
    }

    // DistributionLineSegment ==> Line
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?len ?phs where {?s r:type c:DistributionLineSegment. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:Conductor.length ?len" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    Property ptPhsZ = model.getProperty (nsCIM, "DistributionLineSegment.PhaseImpedance");
    Property ptSeqZ = model.getProperty (nsCIM, "DistributionLineSegment.SequenceImpedance");
    Property ptInfZ = model.getProperty (nsCIM, "DistributionLineSegment.ConductorInfo");
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String len = soln.get ("?len").toString();
      phs_cnt = Phase_Count (phs, false);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      res = model.getResource (id);
      String zPhase = SafeResourceLookup (model, ptName, res, ptPhsZ, "");
      String zSequence = SafeResourceLookup (model, ptName, res, ptSeqZ, "");
      String zInfo = SafeResourceLookup (model, ptName, res, ptInfZ, "");
      String linecode;
      if (zPhase.length() > 0) {
        linecode = " linecode=" + zPhase;
      } else if (zSequence.length() > 0) {
        linecode = " linecode=" + zSequence;
      } else if (zInfo.length() > 0) {
        linecode = " geometry=" + zInfo;
      } else {
        if (phs_cnt == 1) {
          linecode = " linecode=dummy_linecode_1";
        } else if (phs_cnt == 2) {
          linecode = " linecode=dummy_linecode_2";
        } else {
          linecode = " linecode=dummy_linecode_3";
        }
      }

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " length=" + len + linecode);
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // LoadBreakSwitch ==> Line switch=y
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs ?open where {?s r:type c:LoadBreakSwitch. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:Switch.normalOpen ?open" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String open = soln.get ("?open").toString();

      phs_cnt = Phase_Count (phs, false);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM LoadBreakSwitch");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Fuse ==> Line switch=y
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs ?open where {?s r:type c:Fuse. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:Switch.normalOpen ?open" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String open = soln.get ("?open").toString();

      phs_cnt = Phase_Count (phs, false);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM Fuse");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Breaker ==> Line switch=y  (NOTE: a source may be attached to the first instance)
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs where {?s r:type c:Breaker. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      res = model.getResource (id);
      String open = SafeProperty (res, ptOpen, "false");

      phs_cnt = Phase_Count (phs, false);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y // CIM Breaker");
      if (open.equals("false")) {
        out.println ("  close Line." + name + " 1");
      } else {
        out.println ("  open Line." + name + " 1");
      }
      outGuid.println ("Line." + name + "\t" + DSS_Guid (id));
    }

    // Transformers and Regulators
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:TransformerInfo. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());

      out.println ("new XfmrCode." + name + " " + GetXfmrCode (model, id));
      outGuid.println ("XfmrCode." + name + "\t" + DSS_Guid (id));
    }

    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?bank ?code where {?s r:type c:DistributionTransformer. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:DistributionTransformer.TransformerBank ?bank;" +
                                 "   c:DistributionTransformer.TransformerInfo ?code" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      String bank = soln.get ("?bank").toString();
      String code = soln.get ("?code").toString();

      String xfmrcode = DSS_Name (GetPropValue (model, code, "IdentifiedObject.name"));
      String xfmrbank = DSS_Name (GetPropValue (model, bank, "IdentifiedObject.name"));

      out.println ("new Transformer." + name + " xfmrcode=" + xfmrcode + " bank=" + xfmrbank + " buses=" + GetXfmrBuses (model, id, code));
      outGuid.println ("Transformer." + name + "\t" + DSS_Guid (id));
    }
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:DistributionTapChanger. " + 
                                 "?s c:IdentifiedObject.name ?name}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      res = model.getResource (id);
      String sReg = GetRegulatorData (model, res);

      out.println ("new RegControl." + name + " " + sReg);
      outGuid.println ("RegControl." + name + "\t" + DSS_Guid (id));
    }

    // unsupported stuff
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?phs where {?s r:type c:Junction. " + 
                                 "?s c:ConductingEquipment.phases ?phs}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      phs = soln.get ("?phs").toString();
      phs_cnt = Phase_Count (phs, false);
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      out.println ("// new Junction." + name + " phases=" + Integer.toString(phs_cnt));
    }
    query = QueryFactory.create (qPrefix + "select ?s ?phs where {?s r:type c:BusbarSection. " + 
                                 "?s c:ConductingEquipment.phases ?phs}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      phs = soln.get ("?phs").toString();
      phs_cnt = Phase_Count (phs, false);
      res = model.getResource (id);
      name = SafeResName (res, ptName);
      out.println ("// new BusbarSection." + name + " phases=" + Integer.toString(phs_cnt));
    }

    // wrapup
    out.println ();
    out.println ();
    out.println ("// guids " + fGuid);
    out.println ();
    if (NumCircuits > 0) {
      out.println ("set voltagebases=[115, 20.0, 4.16, 0.48]");
    } else {
      out.println ("set voltagebases=[1.0]");
    }
    out.println ("calcv");
    out.println ("solve");
    out.println ("buscoords " + fBus);
    out.println ();
    out.close ();

    outGuid.println ();
    outGuid.close ();
  }
}

