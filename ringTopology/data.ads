generic
	N : Integer;
	P : Integer;
	H : Integer;

package Data is

  type UnconstraightVector is array (Positive range <>) of Integer;
  type UnconstraightMatrix is array (Positive range <>, Positive range <>) of Integer;

	subtype VectorOfN is UnconstraightVector(1..N);
	subtype MatrixOfN is UnconstraightMatrix(1..N, 1..N);

	subtype VectorOfH is UnconstraightVector(1..H);
	subtype MatrixOfH is UnconstraightMatrix(1..H, 1..H);

  procedure Generate_Matrix(M : in out UnconstraightMatrix; v : in Integer);
	procedure Generate_Vector(A : in out UnconstraightVector; v : in Integer);

	procedure Output_Matrix(MA : in UnconstraightMatrix);
	procedure Output_Vector(A : in UnconstraightVector);

	procedure SumTo(ML : in UnconstraightMatrix; MR : in UnconstraightMatrix; MG : out UnconstraightMatrix);
	procedure SubTo(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG: out UnconstraightMatrix);

	procedure SumTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector);
	procedure SubTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector);

	procedure Mult(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG : out UnconstraightMatrix);
	procedure Mult(Left : in UnconstraightVector; Right : in UnconstraightMatrix; result : out UnconstraightVector);
	procedure Mult(e : in Integer; MK : in UnconstraightMatrix; result : out UnconstraightMatrix);
	procedure Mult(e : in Integer; K : in UnconstraightVector; result : out UnconstraightVector);

	procedure Copy(A : in UnconstraightVector; B : in out UnconstraightVector; From, To : in Integer);
	procedure Copy(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To : in Integer);

end Data;
