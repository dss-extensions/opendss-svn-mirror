
{External declarations for KLUSolve.dll}

FUNCTION NewSparseSet(nBus:LongWord):LongWord;                      stdcall;external 'KLUSolve.dll';
FUNCTION SolveSparseSet(id:LongWord; x,b:pComplexArray):LongWord;               stdcall;external 'KLUSolve.dll';
FUNCTION DeleteSparseSet(id:LongWord):LongWord;                     stdcall;external 'KLUSolve.dll';
FUNCTION AddMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   stdcall;external 'KLUSolve.dll';
FUNCTION GetMatrixElement(id:LongWord; i,j:LongWord; Value:pComplex):LongWord;   stdcall;external 'KLUSolve.dll';

// FUNCTION ZeroSparseSet(id:LongWord):LongWord;                                   stdcall;external 'KLUSolve.dll';
// FUNCTION FactorSparseMatrix(id:LongWord):LongWord;                              stdcall;external 'KLUSolve.dll';

