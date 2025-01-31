
"""
Created on Thu Jan  20 10:23:05 2025

@author: Davis Montenegro - davis_montenegro@hotmail.com

Implements an example that illustrate how to crete a GIS shapefile using QGIS based on an OpenDSS model.
The model MUST have GIS coordinates (see https://opendss.epri.com/GISCoords1.html ), make sure the coordinates match
with the coordinate system defined in line 47. This example works only with lines but feel free to add more if needed. 
 
"""


from osgeo import ogr
import os
import sys
from PyQt5.QtWidgets import QListWidget, QListWidgetItem
import pathlib
import traceback
import win32com.client


try:
                            
    FilterLName = "myShapeFile"     # This is the name for your loadshape 
    
    dataPath = "C:/Temp/"           # This is the location where your ShapeFile will be stored
    print("Initializing the system...")
    # Initialize OpenDSS (late binding)
    DSSObj = win32com.client.dynamic.Dispatch("OpenDSSEngine.DSS")
    DSSText = DSSObj.Text
    DSSCircuit = DSSObj.ActiveCircuit
    DSSSolution = DSSCircuit.Solution
    DSSParallel = DSSCircuit.Parallel;
    DSSBus = DSSCircuit.ActiveBus
    DSSLine = DSSCircuit.Lines
    DSSObj.Start(0)

    print('Loading up the model...')
    DSSText.Command='Clear'
    # This is where your model is
    DSSText.Command=r'compile "C:\Temp\TYN203-202-201\master.dss"' 
    DSSText.Command='set maxiterations=1000 maxcontroliter=1000' 
    DSSSolution.Solve                       # Solves Actor 1
    print('Model loaded successfully...')
    # Here creates the layer and defines its fields
    geographic_coordsys = "EPSG:4326"  # e.g. WGS84, NAD83(CSRS)
    geo_csrs = QgsCoordinateReferenceSystem(geographic_coordsys)
    fields = QgsFields()
    fields.append(QgsField("id", QVariant.String))
    fields.append(QgsField("line_name", QVariant.String))
    fields.append(QgsField("bus1", QVariant.String))
    fields.append(QgsField("bus2", QVariant.String))
    shpfile = os.path.join(dataPath, f"{FilterLName}.shp")
    writer = QgsVectorFileWriter(
        shpfile, "UTF8", fields, QgsWkbTypes.Unknown, driverName="ESRI Shapefile"
    )
    
    print("Process started...")
    Lidx = DSSLine.First
    features= []
    idx = 0
    while Lidx != 0:
        Bus1 = DSSLine.Bus1.split(".")[0]
        DSSCircuit.SetActiveBus(Bus1)
        long1 = DSSBus.Longitude
        lat1 = DSSBus.Latitude
        Bus2 = DSSLine.Bus2.split(".")[0]
        DSSCircuit.SetActiveBus(Bus2)
        long2 = DSSBus.Longitude
        lat2 = DSSBus.Latitude

        if ((long1 != 0) and (lat1 != 0)) and ((long2 != 0) and (lat2 != 0)):
            f = QgsFeature()
            f.setGeometry(
                QgsGeometry.fromPolylineXY(
                    [QgsPointXY(long1,lat1), QgsPointXY(long2,lat2)]
                )
            )
            f.setAttributes([idx, DSSBus.Name, Bus1, Bus2])
            features.append(f)
            idx += 1
        Lidx = DSSLine.Next
        
    writer.addFeatures(features)
    del writer
    processing.run("qgis:definecurrentprojection", {"INPUT": shpfile, "CRS": geo_csrs})

    layer = QgsVectorLayer(shpfile, f"{FilterLName}", "ogr")
    QgsProject.instance().addMapLayer(layer)
    print("Process done")
except Exception as e:
    pass
    error_type = type(e).__name__
    print("Error: " + error_type + "-> ", e)
    traceback.print_exc()


