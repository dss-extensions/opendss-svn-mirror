// KLUSolve.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"
#include "klusolve.h"

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
    return TRUE;
}

// exported function definitions

extern "C" int FAR PASCAL EXPORT NewSparseSet(int nBus)
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT SetActiveSparseSet(int nID)
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT ZeroSparseSet()
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT FactorSparseMatrix()
{
    int rc = 0;
	return rc;
}

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
extern "C" int FAR PASCAL EXPORT SolveSparseSet(complex *_acxX, complex *_acxB)
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT DeleteSparseSet(int n)
{
    int rc = 0;
	return rc;
}

/* i and j are 1-based for these */
extern "C" int FAR PASCAL EXPORT AddMatrixElement(int i, int j, complex *pcxVal)
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT SetMatrixElement(int i, int j, complex *pcxVal)
{
    int rc = 0;
	return rc;
}

extern "C" int FAR PASCAL EXPORT GetMatrixElement(int i, int j, complex *pcxVal)
{
    int rc = 0;
	return rc;
}
