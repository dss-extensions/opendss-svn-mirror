#ifndef klusolve_included
#define klusolve_included

#ifndef _COMPLEX_DEFINED
#define _COMPLEX_DEFINED
typedef struct _complex {double x, y;} complex;
#endif

#ifndef KLU_API
// import definitions
#define KLU_API __declspec(dllimport)
#endif

#ifdef __cplusplus
extern "C" {
#endif

// return ID of new sparse set, 0 if error
KLU_API int NewSparseSet(int nBus);
// return ID of active sparse set, 0 if error
KLU_API int SetActiveSparseSet(int nID);
// return 1 if successful, 0 if not
KLU_API int ZeroSparseSet();
// return 1 if successful, -1 if singular, 0 if no solver
KLU_API int FactorSparseMatrix();
/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
// return 1 if successful, -1 if singular, 0 if no solver
KLU_API int SolveSparseSet(complex *_acxX, complex *_acxB);
// return 1 if successful, 0 if not
KLU_API int DeleteSparseSet(int nID);
/* i and j are 1-based for these */
// return 1 if successful, 0 if not
KLU_API int AddMatrixElement(int i, int j, complex *pcxVal);
// return 1 if successful, 0 if not
KLU_API int SetMatrixElement(int i, int j, complex *pcxVal);
// return 1 if successful, 0 if not
KLU_API int GetMatrixElement(int i, int j, complex *pcxVal);

#ifdef __cplusplus
}
#endif

#endif // klusolve_included