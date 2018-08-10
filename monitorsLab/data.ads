generic
	N : Integer;

package Data is


  type Vector is array(1..N) of Integer;
  type Matrix is array(1..N) of Vector;

  procedure Input_Vector(Name: in string; A: out Vector);
  procedure Input_Matrix(Name: in String; MA: out Matrix);

  procedure Generate_Vector(A : out Vector; v : in Integer);
  procedure Generate_Matrix(MA : out Matrix; v : in Integer);

  procedure Output_Vector(A: in Vector);
  procedure Output_Matrix(MA: in Matrix);

	procedure SumTo(ML: in Matrix; MR: in Matrix; MG: in out Matrix; first :in Integer; last :in Integer);
	procedure SubTo(ML: in Matrix; MR: in Matrix; MG: out Matrix; first :in Integer; last :in Integer);

	procedure SumTo(L: in Vector; R: in Vector; G: out Vector; first :in Integer; last :in Integer);
	procedure SubTo(L: in Vector; R: in Vector; G: out Vector; first :in Integer; last :in Integer);

	function Mult(ML: in Matrix; MR: in Matrix; first : in Integer; last : in Integer) return Matrix;
	function Mult(Left : in Vector; Right : in Matrix; From : Integer; To : in Integer) return Vector;

	function Mult(e : in Integer; MK : in Matrix; first :in Integer; last :in Integer) return Matrix;
	function Mult(e : in Integer; K : in Vector; first : in Integer; last : in Integer) return Vector;

	procedure MultTo(Left : in Vector; Right : in Matrix; ToVector : in out Vector; From : Integer; To : in Integer);

	procedure MultTo(A, B: in Matrix; C : in out Matrix; From, To: in Integer);


	procedure Copy(A : in Vector; B : in out Vector; From, To : in Integer);

	function Scalar(A : in Vector; B : in Vector; from : in Integer; to : in Integer) return Integer;

	function Min(A : in Vector; from : in Integer; to : in Integer) return Integer;

	function Max(A : in Vector; from : in Integer; to : in Integer) return Integer;

private

    --type Vector is array(1..N) of Integer;
    --type Matrix is array(1..N) of Vector;

    function "-"(L, R : in Vector) return Vector;
    function "+"(L, R : in Vector) return Vector;
    function "*"(L, R : in Vector) return Integer;

    function "+"(ML, MR : in Matrix) return Matrix;
    function Max(MR : in Matrix) return Integer;

    function "*"(A : in Vector; e : in Integer) return Vector;

    function "*"(R : in Vector; MR : in Matrix) return Vector;
    function Max(V : in Vector) return Integer;

end Data;
