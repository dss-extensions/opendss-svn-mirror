#ifndef klusystem_included
#define klusystem_included

extern "C" {
#include "cs.h"
#include "csz.h"
#include "klu.h"
}

struct matrix_complex {
	complex *acx;
	unsigned nRow, nCol;
	complex get_acx (unsigned i, unsigned j) {
		return acx[i*nCol+j];
    }
};

/* Kron reduction not supported, this version just solves

|Y22| * |V| = |I|

KLU manages complex values as interleaved real/imag in double arrays
KLU arrays are zero-based
*/

class KLUSystem
{
private:
    unsigned	nBus;  // Kron reduction not exposed in the interface
	complex		*acx;
	
	// admittance matrix blocks in compressed-column storage, like Matlab
	cs *Y22;

	// admittance matrix blocks in triplet storage
	cs *T22;

	klu_symbolic *Symbolic;
	klu_numeric *Numeric;

	unsigned m_nBus;    // number of nodes
	unsigned m_nX;      // number of unknown voltages, hardwired to m_nBus
	unsigned m_NZpre;   // number of non-zero entries before factoring
	unsigned m_NZpost;  // number of non-zero entries after factoring
	unsigned m_fltBus;  // row number of a bus causing singularity

	void InitDefaults ();
    void clear ();
	void zero_indices ();
	void null_pointers ();
	cs *process_triplet (cs **T);
	void compress_partitions ();

protected:

public:
	KLUSystem ();
    KLUSystem (unsigned nBus, unsigned nV = 0, unsigned nI = 0);
    ~KLUSystem ();

    bool bFactored;      //  system has been factored

    int FactorSystem ();
    int SolveSystem (complex *_acxX, complex *_acxB);
    int Initialize (unsigned n, unsigned _nV, unsigned _nI);
	unsigned GetSize () {return nBus;}
	// this resets and reinitializes the sparse matrix, nI = nBus
    void initialize (unsigned nBus, unsigned nV = 0, unsigned nI = 0);

	unsigned GetnSparse () {return m_NZpost;}
	unsigned GetidxFillin () {return m_NZpre;}

	// bSum is ignored
    void AddMatrix (unsigned far *aidBus, matrix_complex *pcxm, int bSum);
	
	// returns 1 for success, -1 for a singular matrix
	// returns 0 for another KLU error, most likely the matrix is too large for int32
    int Factor ();
	
	// input: acxVbus[0] is ground voltage
	//        acxVbus[1..nBus] are current injections
	// output: acxVbus[1..nBus] are solved voltages
    void Solve (complex *acxVbus);  

	// returns the number of connected components (cliques) in the whole system graph
	//  (i.e., considers Y11, Y12, and Y21 in addition to Y22)
	// if more than 1, store lists of each clique's connected buses in paaidBus
	unsigned FindIslands (unsigned ***paaidBus);
	
	// returns the row > 0 if a zero appears on the diagonal
	// calls Factor if necessary
	// note: the EMTP terminology is "floating subnetwork"
	unsigned FindDisconnectedSubnetwork ();

	// The following were added for ESolv32:
	// maintains allocations, zeros matrix values
    void zero();
	// bSum is ignored
    void AddElement(unsigned iRow, unsigned iCol, complex &cpxVal, int bSum);
	// return the sum of elements at 1-based [iRow, iCol]
    void GetElement(unsigned iRow, unsigned iCol, complex &cpxVal);
};

#endif // klusystem_included