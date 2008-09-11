// KLUSolve.cpp : Defines the entry point for the DLL application.
//

#include "stdafx.h"

#define KLU_API extern "C" __declspec(dllexport) unsigned int __stdcall
#include "klusolve.h"
#include "klusystem.h"

#define SYMMETRIC_MATRIX

#undef LOG_FILE

#ifdef LOG_FILE

static FILE *lfp = NULL;

static void write_lfp (char *fmt, ...)
{
	va_list args;
	va_start (args, fmt);

	if (lfp == NULL) {
		lfp = fopen ("KLUSolve.log", "w");
	}
	if (lfp) {
		vfprintf (lfp, fmt, args);
		fflush (lfp);
	}

	va_end (args);
}

#endif

BOOL APIENTRY DllMain( HANDLE hModule, 
                       DWORD  ul_reason_for_call, 
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call) {
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			break;
		case DLL_THREAD_ATTACH:
			break;
		case DLL_THREAD_DETACH:
			break;
	}
    return TRUE;
}

// exported function definitions

KLU_API NewSparseSet (unsigned int nBus)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("NewSparseSet %u\n", nBus);
#endif

    KLUSystem *pSys = new KLUSystem ();
    if (pSys) {
        pSys->Initialize(nBus, 0, nBus);
        rc = reinterpret_cast<unsigned int> (pSys);
    }
	return rc;
}

KLU_API ZeroSparseSet (unsigned int hSparse)
{
    unsigned long rc = 0;

#ifdef LOG_FILE
	write_lfp ("ZeroSparseSet\n");
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (rc);
	if (pSys) {
		pSys->zero();
		pSys->bFactored = false;
		rc = 1;
	}
	return rc;
}

KLU_API FactorSparseMatrix (unsigned int hSparse)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("FactorSparseMatrix\n");
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
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
KLU_API SolveSparseSet(unsigned int hSparse, complex *_acxX, complex *_acxB)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("SolveSparseSet\n");
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
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

KLU_API DeleteSparseSet(unsigned int hSparse)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("DeleteSparseSet %u\n", hSparse);
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		delete pSys;
		rc = 1;
    }

	return rc;
}

/* i and j are 1-based for these */
KLU_API AddMatrixElement(unsigned int hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("AddMatrixElement [%u,%u] = %G + j%G\n", i, j, pcxVal->x, pcxVal->y);
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
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

KLU_API GetMatrixElement (unsigned int hSparse, unsigned int i, unsigned int j, complex *pcxVal)
{
    unsigned int rc = 0;

#ifdef LOG_FILE
	write_lfp ("GetMatrixElement [%u,%u]\n", i, j);
#endif

	KLUSystem *pSys = reinterpret_cast<KLUSystem *> (hSparse);
	if (pSys) {
		pSys->GetElement (i, j, *pcxVal);
		rc = 1;
	}
	return rc;
}
