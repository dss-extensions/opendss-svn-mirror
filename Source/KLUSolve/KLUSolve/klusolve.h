#ifndef __cplusplus
#include <math.h>
typedef struct _complex complex;
#endif

#ifdef __cplusplus
#include <complex>
extern "C" {
#endif  /* __cplusplus */

int FAR PASCAL NewSparseSet(int nBus);
int FAR PASCAL SetActiveSparseSet(int nID);
int FAR PASCAL ZeroSparseSet();
int FAR PASCAL FactorSparseMatrix();
/* 
  input: current injections in zero-based _acxB
  output: node voltages in zero-based _acxX
  no provision for voltage sources
*/
int FAR PASCAL SolveSparseSet(complex *_acxX, complex *_acxB);
int FAR PASCAL DeleteSparseSet(int n);
/* i and j are 1-based for these */
int FAR PASCAL AddMatrixElement(int i, int j, complex *pcxVal);
int FAR PASCAL SetMatrixElement(int i, int j, complex *pcxVal);
int FAR PASCAL GetMatrixElement(int i, int j, complex *pcxVal);

#ifdef __cplusplus
}
#endif
