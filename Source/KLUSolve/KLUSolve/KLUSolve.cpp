// KLUSolve.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"

#define KLU_API extern "C" __declspec(dllexport)
#include "klusolve.h"
#include "klusystem.h"

#define MAXSET 10

KLUSystem   *apSys[MAXSET];
unsigned    idCurrentSet;

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call) {
		case DLL_PROCESS_ATTACH:
			for (unsigned i = 0; i < MAXSET; i++) {
				apSys[i] = NULL;
			}
			break;
		case DLL_PROCESS_DETACH:
			for (unsigned i = 0; i < MAXSET; i++) {
				if (apSys[i]) delete apSys[i];
				apSys[i] = NULL;
			}
			break;
		case DLL_THREAD_ATTACH:
			break;
		case DLL_THREAD_DETACH:
			break;
	}
    return TRUE;
}

// exported function definitions

KLUSystem *GetKLU() {
	if (idCurrentSet) return apSys [idCurrentSet - 1];
	return NULL;
}

KLU_API int NewSparseSet(int nBus)
{
    int rc = 0;

	for (unsigned i=0; i<MAXSET; i++) {
        if (apSys[i] == 0) {
            apSys[i] = new KLUSystem();
            KLUSystem *pSys = apSys[i];
            if (pSys) {
                pSys->Initialize(nBus, 0, nBus);
                rc = i+1;
                idCurrentSet = rc;
                break;
             }
        }
    }
	return rc;
}

KLU_API int SetActiveSparseSet(int nID)
{
    int rc = 0;

	if (nID > 0 && nID <= MAXSET) {
        if (apSys[nID-1]) {
            idCurrentSet = nID;
            rc = nID;
        }
    }

	return rc;
}

KLU_API int ZeroSparseSet()
{
    int rc = 0;
	KLUSystem *pSys = GetKLU();
	if (pSys) {
		pSys->zero();
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

KLU_API int FactorSparseMatrix()
{
    int rc = 0;
	KLUSystem *pSys = GetKLU();
	if (pSys) {
		if (pSys->FactorSystem() == 0) { // success
			rc = 1;
		} else { // singular
			rc = -1;
		}
	}
	return rc;
}

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
KLU_API int SolveSparseSet(complex *_acxX, complex *_acxB)
{
    int rc = 0;
	KLUSystem *pSys = GetKLU();
	if (pSys) {
		if (pSys->bFactored == false) {
			pSys->FactorSystem();
		}
		if (pSys->bFactored) {
			pSys->SolveSystem (_acxX, _acxB);
			rc = 1;
		} else {
			rc = -1;
		}
	}
	return rc;
}

KLU_API int DeleteSparseSet(int nID)
{
    int rc = 0;

	if (nID > 0 && nID <= MAXSET) {
        if (apSys[nID-1]) {
			delete apSys[nID-1];
			apSys[nID-1] = NULL;
            idCurrentSet = 0;
            rc = 1;
        }
    }

	return rc;
}

/* i and j are 1-based for these */
KLU_API int AddMatrixElement(int i, int j, complex *pcxVal)
{
    int rc = 0;
	KLUSystem *pSys = GetKLU();
	if (pSys) {
		pSys->AddElement (i, j, *pcxVal, TRUE);
#ifdef SYMMETRIC_MATRIX
		if (i != j) pSys->AddElement (j, i, *pcxVal, TRUE);
#endif
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

// TODO - not really implemented
KLU_API int SetMatrixElement(int i, int j, complex *pcxVal)
{
	return AddMatrixElement (i, j, pcxVal);
}

KLU_API int GetMatrixElement(int i, int j, complex *pcxVal)
{
    int rc = 0;
	KLUSystem *pSys = GetKLU();
	if (pSys) {
		pSys->GetElement (i, j, *pcxVal);
		rc = 1;
	}
	return rc;
}
