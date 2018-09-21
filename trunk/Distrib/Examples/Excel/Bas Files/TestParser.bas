Public Sub TestParser()

Dim Mydbl As Double
Dim Myint As Long
Dim MyArray As Variant
Dim MyMatrix As Variant
Dim ParamName As String, TokenValue As String
Dim i As Long


' VBA Example of using features of DSSParser interface to parse a text line

With DSSParser

    .AutoIncrement = False

    .CmdString = "cmd=ParseThiscmd dblvalue=1.234  intvalue=56  strvalue=teststring  Array=[10 11 12 13] Matrix=[10 11 | 12 13]"

    
    ' Open an output file
    Open "Parsertest.txt" For Output As #1
    
        Print #1, "Command String"
        Print #1, .CmdString
        Print #1,
    ' cmd
        ParamName = .NextParam
        TokenValue = .strValue
        Print #1, ParamName; "="; TokenValue
        
    ' dbl
        ParamName = .NextParam
        TokenValue = .strValue
        Mydbl = .DblValue
        Print #1, ParamName; "="; TokenValue; ": "; Mydbl
        
     ' int
        ParamName = .NextParam
        TokenValue = .strValue
        Myint = .IntValue
        Print #1, ParamName; "="; TokenValue; ": "; Myint
       
    ' strvalue
        ParamName = .NextParam
        TokenValue = .strValue
        Print #1, ParamName; "="; TokenValue
        
    ' Array of dbls
        ParamName = .NextParam
        TokenValue = .strValue
        MyArray = .Vector(10) ' specify expected size bigger than array; Parser will correct
        Print #1, ParamName; "="; TokenValue; ": ";
        For i = LBound(MyArray) To UBound(MyArray)
            Print #1, " ", MyArray(i);
        Next i
        Print #1,
        
    ' 2 x 2 Matrix of dbls
        ParamName = .NextParam
        TokenValue = .strValue
        MyArray = .Matrix(2) ' return a 2x2 in column order
        Print #1, ParamName; "="; TokenValue; ": "
            Print #1, "Row 1: ", MyArray(0), ", ", MyArray(2)
            Print #1, "Row 2: ", MyArray(1), ", ", MyArray(3)
            
    
            
    ' Now repeat the same code with AutoIncrement ON
    ' NextParam is automatically called after the token is retrieved
    ' ParamName is not set and can't call strValue without advancing token pointer
    
    Print #1,
    Print #1, "------------------- with AutoIncrement -------------------------------------"
    Print #1,
    
    .AutoIncrement = True
    .CmdString = "cmd=ParseThiscmd dblvalue=1.234  intvalue=56  strvalue=teststring  Array=[10 11 12 13] Matrix=[10 11 | 12 13]"
        Print #1, "Command String"
        Print #1, .CmdString
        Print #1,
        
    ' cmd
        TokenValue = .strValue
        Print #1, TokenValue
        
    ' dbl
        Mydbl = .DblValue
        Print #1, Mydbl
        
     ' int
        Myint = .IntValue
        Print #1, Myint
       
    ' strvalue
        Print #1, .strValue
        
       .AutoIncrement = False ' can't use autoincrement with Vector and Matrix
       
    ' Array of dbls
        ParamName = .NextParam
        MyArray = .Vector(10) ' specify expected size bigger than array. Parser will correct.
        Print #1, "Array: ";
        For i = LBound(MyArray) To UBound(MyArray)
            Print #1, " ", MyArray(i);
        Next i
        Print #1,
        
    ' 2 x 2 Matrix of dbls
        ParamName = .NextParam
        MyArray = .Matrix(2) ' return a 2x2 in column order
        Print #1, "2 x 2 Matrix"; ": "
            Print #1, "Row 1: ", MyArray(0), ", ", MyArray(2)
            Print #1, "Row 2: ", MyArray(1), ", ", MyArray(3)
            
            
    Close #1
    
    
    
End With

    Shell ("notepad Parsertest.txt")

End Sub