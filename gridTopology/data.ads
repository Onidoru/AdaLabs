generic
	N : Integer;
	P : Integer;
	H : Integer;
	HRow : Integer;

package Data is

  type UnconstraightVector is array (Integer range <>) of Integer;
	subtype VectorOfN is UnconstraightVector(1..N);
	subtype VectorOfH is UnconstraightVector(1..H);
	subtype VectorOfHRow is UnconstraightVector(1..HRow);

	type UnconstraightMatrix is array (Integer range <>) of VectorOfN;
	subtype MatrixOfN is UnconstraightMatrix(1..N);
	subtype MatrixOfH is UnconstraightMatrix(1..H);
	subtype MatrixOfHRow is UnconstraightMatrix(1..HRow);

		-- type ID_Numbers is range 0..228;
	type ID_Path is array (Integer range 1..1000) of Integer;

	type VarID is (id_A, id_B, id_C, id_D, id_K, id_O, id_T, id_X, id_F, id_Z);

  procedure Generate_Matrix(M : in out UnconstraightMatrix; v : in Integer);
	procedure Generate_Vector(A : in out UnconstraightVector; v : in Integer);
	procedure Generate_Path(A : in out ID_Path; v : in Integer);


	procedure Output_Matrix(MA : in UnconstraightMatrix;  m_id, n_id : Integer);
	procedure Output_Vector(A : in UnconstraightVector; m_id, n_id : Integer);
	procedure Output_Path(A : in ID_Path; id : Integer);


	procedure SumTo(ML : in UnconstraightMatrix; MR : in UnconstraightMatrix; MG : out UnconstraightMatrix);
	procedure SubTo(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG: out UnconstraightMatrix);

	procedure SumTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector);
	procedure SubTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector);

	procedure Mult(ML : in UnconstraightMatrix; MR : in UnconstraightMatrix; MG : out UnconstraightMatrix);
	procedure Mult(Left : in UnconstraightVector; Right : in UnconstraightMatrix; result : out UnconstraightVector);
	procedure Mult(e : in Integer; MK : in UnconstraightMatrix; result : out UnconstraightMatrix);
	procedure Mult(e : in Integer; K : in UnconstraightVector; result : out UnconstraightVector);

	procedure Copy(A : in UnconstraightVector; B : in out UnconstraightVector; From, To : in Integer);
	procedure Copy(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To : in Integer);

	procedure CopyCareful(A : in out UnconstraightVector; B : in UnconstraightVector; fromA, toA: in Integer);

	procedure CopyCareful(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To, fromR : in Integer);


	function FindTheWay(fromNode, toNode : Integer) return ID_Path;

	function Find_Minimum(MA : in UnconstraightMatrix) return Integer;

	function Scalar(A, B : in UnconstraightVector) return Integer;
	--------------------------------------------------------------------------------




end Data;
