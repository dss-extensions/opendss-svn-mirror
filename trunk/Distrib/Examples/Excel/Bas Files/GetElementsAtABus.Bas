Public Sub GetElementsAtABus(BusName As String)

' Write a file with a list of elements at a bus
    
    Dim iElem As Long, i As Long
    Dim DSSCktElement As OpenDSSengine.CktElement
    Dim MyBusNames As Variant
    Dim TestBusName1 As String, TestBusName2 As String, DotPos As Long, TestLength As Long
    
    TestBusName1 = UCase(BusName)  ' upper case for comparison
    

    Open "ElementAtBusList.TXT" For Output As #1
    
        For iElem = 1 To DSSCircuit.NumCktElements
            Set DSSCktElement = DSSCircuit.CktElements(iElem)   ' cycle through all circuit elements
            MyBusNames = DSSCktElement.BusNames    'Array of strings of all bus names
            
            For i = LBound(MyBusNames) To UBound(MyBusNames)
                TestBusName2 = UCase(MyBusNames(i))
                DotPos = InStr(TestBusName2, ".")  ' get rid of node designation for test
                If DotPos > 0 Then TestLength = DotPos - 1 Else TestLength = Len(TestBusName2)
                
                If StrComp(Left(TestBusName2, TestLength), TestBusName1) = 0 Then Print #1, DSSCktElement.Name
            
            Next i
            
                   
        Next iElem
    
    
    Close #1
    
    ' show the file in OpenDSS Editor
    
    DSSText.Command = "FileEdit ElementAtBusList.Txt"


End Sub