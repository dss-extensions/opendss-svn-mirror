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

  static String DSS_Guid (String arg) {
    int hash = arg.lastIndexOf ("#_");
    return arg.substring (hash + 2);
  }

  static String DSS_Name (String arg) {
    return arg.replace ('=', '_');
  }

  static String Phase_String (String arg) {
    int hash = arg.lastIndexOf ("#PhaseCode.");
    return arg.substring (hash + 11);
  }

  static int Phase_Count (String arg) {
    String phs = Phase_String (arg);
    int cnt = phs.length();
    if (phs.contains ("N")) {
      --cnt;
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
      return "";
    }
  }

  static String GetWdgConnection (String arg) {
    int hash = arg.lastIndexOf ("#WindingConnection.");
    return arg.substring (hash + 19);
  }

  static String GetPropValue (Model mdl, String uri, String prop) {
    Resource res = mdl.getResource (uri);
    Property p = mdl.getProperty (nsCIM, prop);
    return res.getProperty(p).getString();
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
        return CN.getProperty(ptName).getString();
      }
    }
    return "x";
  }

  static String GetXfmrBuses (Model mdl, String id) {
    Property ptXfmr = mdl.getProperty (nsCIM, "DistributionTransformerWinding.Transformer");
    Property ptInfo = mdl.getProperty (nsCIM, "DistributionTransformerWinding.WindingInfo");
    Property ptPhs = mdl.getProperty (nsCIM, "ConductingEquipment.phases");
    StringBuilder buf = new StringBuilder ("[");

    Resource xfRes = mdl.getResource (id);
    ResIterator iter = mdl.listResourcesWithProperty (ptXfmr, xfRes);
    while (iter.hasNext()) {
      Resource wdg = iter.nextResource();
      String phs = wdg.getProperty(ptPhs).toString();
      buf.append (GetBusName (mdl, wdg.toString(), 1));
      buf.append (Bus_Phases (phs));
      if (iter.hasNext()) {
        buf.append (",");
      } else {
        buf.append ("]");
      }
    }
    return buf.toString();
  }

  static String GetXfmrCode (Model mdl, String id) {
    Property ptInfo = mdl.getProperty (nsCIM, "WindingInfo.TransformerInfo");
    Property ptU = mdl.getProperty (nsCIM, "WindingInfo.ratedU");
    Property ptS = mdl.getProperty (nsCIM, "WindingInfo.ratedS");
    Property ptC = mdl.getProperty (nsCIM, "WindingInfo.connectionKind");
    StringBuilder bufU = new StringBuilder ("kvs=[");
    StringBuilder bufS = new StringBuilder ("kvas=[");
    StringBuilder bufC = new StringBuilder ("conns=[");
    String sPhases = "phases=3 ";

    Resource xfRes = mdl.getResource (id);
    ResIterator iter = mdl.listResourcesWithProperty (ptInfo, xfRes);
    while (iter.hasNext()) {
      Resource wdg = iter.nextResource();
      String U = wdg.getProperty(ptU).getString();
      String S = wdg.getProperty(ptS).getString();
      String C = GetWdgConnection (wdg.getProperty(ptC).getObject().toString());
      if (C.equals ("I")) {
        sPhases = "phases=1 ";
        C = "Y";
      }
      if (iter.hasNext()) {
        bufU.append (U + ",");
        bufS.append (S + ",");
        bufC.append (C + ",");
      } else {
        bufU.append (U + "] ");
        bufS.append (S + "] ");
        bufC.append (C + "] ");
      }
    }
    return sPhases + bufU.toString() + bufS.toString() + bufC.toString();
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
      while (iter.hasNext()) {
        Resource pos = iter.nextResource();
        if (pos.hasProperty (ptPosSeq, trmSeq)) { // at the end we are looking for
          return pos.getProperty(ptX).getString() + ", " + pos.getProperty(ptY).getString();
        }
      }
    }

    return "0, 0";
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
    String id, name, phs, bus_phs, bus1, bus2;
    int phs_cnt;

    // ConnectivityNode ==> bus coordinate CSV 
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:ConnectivityNode. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();
      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      outBus.println (name + ", " + GetBusPositionString (model, id));
    }
    outBus.println ();
    outBus.close ();
    
    // EnergySource ==> Circuit  (TODO - select the main circuit source first, then the others)
    out.println ();
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

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String vSrce = soln.get ("?v").toString();
      String ckt = soln.get ("?ckt").toString();

      phs_cnt = Phase_Count (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      String srcClass = "Vsource.";
      if (name.equals ("source")) {
        srcClass = "Circuit.";
        name = GetPropValue (model, ckt, "IdentifiedObject.name");
      }

      out.println ("new " + srcClass + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " basekv=" + vSrce);
      outGuid.println (srcClass + name + "\t" + DSS_Guid (id));
    }

    // EnergyConsumer ==> Load
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs ?p ?q ?c where {?s r:type c:EnergyConsumer. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:EnergyConsumer.pfixed ?p;" +
                                 "   c:EnergyConsumer.qfixed ?q;" +
                                 "   c:EnergyConsumer.customerCount ?c" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String pLoad = soln.get ("?p").toString();
      String qLoad = soln.get ("?q").toString();
      String nCust = soln.get ("?c").toString();

      phs_cnt = Phase_Count (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      out.println ("new Load." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                   " kw=" + pLoad + " kvar=" + qLoad + " numcust=" + nCust);
      outGuid.println ("Load." + name + "\t" + DSS_Guid (id));
    }

    // ShuntCompensator ==> Capacitor
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?phs ?q ?u where {?s r:type c:ShuntCompensator. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:ShuntCompensator.nomQ ?q;" +
                                 "   c:ShuntCompensator.nomU ?u" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String nomQ = soln.get ("?q").toString();
      String nomU = soln.get ("?u").toString();

      phs_cnt = Phase_Count (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;

      out.println ("new Capacitor." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + 
                          " kv=" + nomU + " kvar=" + nomQ);
      outGuid.println ("Capacitor." + name + "\t" + DSS_Guid (id));
    }

    // LineCodes
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name where {?s r:type c:PerLengthPhaseImpedance. " + 
                                 "?s c:IdentifiedObject.name ?name" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      out.println ("new LineCode." + name + " r1=0 x1=0.001 c1=0 r0=0 x0=0.001 c0=0");
      outGuid.println ("LineCode." + name + "\t" + DSS_Guid (id));
    }

    // DistributionLineSegment ==> Line
    out.println ();
    out.println ();
    query = QueryFactory.create (qPrefix + "select ?s ?name ?len ?phs ?zph where {?s r:type c:DistributionLineSegment. " + 
                                 "?s c:IdentifiedObject.name ?name;" +
                                 "   c:ConductingEquipment.phases ?phs;" +
                                 "   c:Conductor.length ?len;" +
                                 "   c:DistributionLineSegment.PhaseImpedance ?zph" +
                                 "}");
    qexec = QueryExecutionFactory.create (query, model);
    results=qexec.execSelect();
    while (results.hasNext()) {
      soln = results.next();

      id = soln.get ("?s").toString();
      name = DSS_Name (soln.get ("?name").toString());
      phs = soln.get ("?phs").toString();
      String len = soln.get ("?len").toString();
      String zph = soln.get ("?zph").toString();

      String linecode = GetPropValue (model, zph, "IdentifiedObject.name");
      phs_cnt = Phase_Count (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " length=" + len + " linecode=" + linecode);
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

      phs_cnt = Phase_Count (phs);
      bus_phs = Bus_Phases (phs);
      bus1 = GetBusName (model, id, 1) + bus_phs;
      bus2 = GetBusName (model, id, 2) + bus_phs;

      out.println ("new Line." + name + " phases=" + Integer.toString(phs_cnt) + " bus1=" + bus1 + " bus2=" + bus2 
                          + " switch=y");
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

      out.println ("new Transformer." + name + " xfmrcode=" + xfmrcode + " bank=" + xfmrbank + " buses=" + GetXfmrBuses (model, id));
      outGuid.println ("Transformer." + name + "\t" + DSS_Guid (id));
    }

    // wrapup
    out.println ();
    out.println ();
    out.println ("// guids " + fGuid);
    out.println ();
    out.println ("calcv");
    out.println ("buscoords " + fBus);
    out.println ();
    out.close ();

    outGuid.println ();
    outGuid.close ();
  }
}

