#ifndef YmatrixH
#define YmatrixH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Unit to manage System Y matrix

   6-11-00  Created from Solution.Pas
*/


#include "System.h"
#include "Sysutils.h"

#include "Ucomplex.h"
#include "DSSClass.h"
#include "DSSObject.h"

#ifdef OPENDSSC_KLUSOLVEX
#ifndef klusparseset_t
typedef void* klusparseset_t;
#endif
#include "KLUSolveX.h"
#else
#include "klusolve.h" // klusparseset_t
#endif

/*Options for building Y matrix*/

namespace YMatrix
{

	//class EEsolv32Problem;


	const int SERIESONLY = 1;
	const int WHOLEMATRIX = 2;
	const int PDE_ONLY = 3;


	//class EEsolv32Problem: public EXCEPTion {
	//  typedef EXCEPTion inherited;
	//};
	// removed given the lack of EXCEPTion
	class EEsolv32Problem {

	};


	void BuildYMatrix(int BuildOption, bool AllocateVI, int ActorID);
	void ResetSparseMatrix(klusparseset_t* hY, int Size, int ActorID);
	void InitializeNodeVbase(int ActorID);
	String CheckYMatrixforZeroes(int ActorID);

} // namespace Ymatrix

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace YMatrix;
#endif

#endif //  YmatrixH








