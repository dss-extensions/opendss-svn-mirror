#ifndef klusolve_included
#define klusolve_included

#ifndef _COMPLEX_DEFINED
#define _COMPLEX_DEFINED
typedef struct _complex {double x, y;} complex;
#endif

#ifndef KLU_API
// import definitions
#define KLU_API __declspec(dllimport) unsigned int __stdcall
#endif

#ifdef __cplusplus
extern "C" {
#endif

// return handle of new sparse set, 0 if error
// be sure to DeleteSparseSet using the returned handle
KLU_API NewSparseSet (unsigned int nBus);

// return 1 if successful, 0 if not
KLU_API ZeroSparseSet (unsigned int hSparse);

// return 1 if successful, 2 if singular, 0 if other error
KLU_API FactorSparseMatrix (unsigned int hSparse);

/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
// return 1 if successful, 2 if singular, 0 if other error
KLU_API SolveSparseSet (unsigned int hSparse, complex *_acxX, complex *_acxB);

// return 1 if successful, 0 if not
KLU_API DeleteSparseSet (unsigned int hSparse);

/* i and j are 1-based for these */
// return 1 if successful, 0 if not
KLU_API AddMatrixElement (unsigned int hSparse, unsigned int i, unsigned int j, complex *pcxVal);

// return 1 if successful, 0 if not
KLU_API GetMatrixElement (unsigned int hSparse, unsigned int i, unsigned int j, complex *pcxVal);

#ifdef __cplusplus
}
#endif

#endif // klusolve_included