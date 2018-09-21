Public Sub Solve()

' Execute the Solve method in the Solution interface of the active circuit
    DSSobj.ActiveCircuit.Solution.Solve
    If DSSSolution.Converged Then
       '  MsgBox "Solution Converged"
       Beep
    Else
       MsgBox "Solution did not Converge"
    End If
    
End Sub