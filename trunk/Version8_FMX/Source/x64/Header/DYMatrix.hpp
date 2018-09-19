// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DYMatrix.pas' rev: 32.00 (Windows)

#ifndef DymatrixHPP
#define DymatrixHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Arraydef.hpp>
#include <Ucomplex.hpp>
#include <Solution.hpp>

//-- user supplied -----------------------------------------------------------

//-- forward type declarations -----------------------------------------------
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE unsigned __cdecl InitAndGetYparams(NativeUInt &hY, unsigned &nBus, unsigned &nNZ);
extern DELPHI_PACKAGE void __cdecl GetCompressedYMatrix(NativeUInt hY, unsigned nBus, unsigned nNz, pIntegerArray &ColPtr, pIntegerArray &RowIdx, pComplexArray &cVals);
extern DELPHI_PACKAGE void __cdecl ZeroInjCurr(void);
extern DELPHI_PACKAGE void __cdecl GetSourceInjCurrents(void);
extern DELPHI_PACKAGE void __cdecl GetPCInjCurr(void);
extern DELPHI_PACKAGE int __cdecl SystemYChanged(int mode, int arg);
extern DELPHI_PACKAGE void __cdecl BuildYMatrixD(int BuildOps, int AllocateVI);
extern DELPHI_PACKAGE int __cdecl UseAuxCurrents(int mode, int arg);
extern DELPHI_PACKAGE void __cdecl AddInAuxCurrents(int SType);
extern DELPHI_PACKAGE void __cdecl getIpointer(pNodeVarray &IvectorPtr);
extern DELPHI_PACKAGE void __cdecl getVpointer(pNodeVarray &VvectorPtr);
extern DELPHI_PACKAGE int __cdecl SolveSystem(pNodeVarray &NodeV);
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DymatrixHPP
