with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Numerics.Discrete_Random, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_Io;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_IO;

package body Data is
  procedure Generate_Matrix(M : in out UnconstraightMatrix; v : in Integer) is
  begin
    for i in 1..M'Length loop
      for j in 1..M'Length loop
        M(i, j) := i;
      end loop;
    end loop;
  end Generate_Matrix;

  procedure Generate_Vector(A : in out UnconstraightVector; v : in Integer) is
  begin
      for i in A'First..A'Length loop
        A(i) := i;
      end loop;
  end Generate_Vector;

  procedure Output_Vector(A : in UnconstraightVector) is
    Str : Unbounded_String := To_Unbounded_String("");
  begin
      for i in A'First..A'Length loop
        if (i /= A'First) then
          Append(Str, ", ");
        end if;
        Append(Str, Integer'Image(A(i)));
      end loop;
      Put_Line(str);
  end Output_Vector;

  procedure Output_Matrix(MA : in UnconstraightMatrix) is
  begin
      for i in 1..MA'Length loop
        for j in 1..MA'Length loop
          Put(MA(i,j));
        end loop;
        New_Line;
      end loop;
  end Output_Matrix;

  --------------------------------------------------------------------------------

  procedure SumTo(ML : in UnconstraightMatrix; MR : in UnconstraightMatrix; MG : out UnconstraightMatrix) is
  begin
     for i in ML'Range loop
          for j in 1..ML'Length loop
           MG(i, j) := ML(i, j) + MR(i, j);
          end loop;
     end loop;
  end SumTo;

  procedure SubTo(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG: out UnconstraightMatrix) is
  begin
     for i in ML'Range loop
          for j in ML'Range (2) loop
           MG(i, j) := ML(i, j) - MR(i, j);
          end loop;
     end loop;
  end SubTo;

  --------------------------------------------------------------------------------

  procedure SumTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector) is
  begin
     for i in L'Range loop
       G(i) := L(i) + R(i);
     end loop;
  end SumTo;

  procedure SubTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector) is
  begin
     for i in L'Range loop
       G(i) := L(i) - R(i);
     end loop;
  end SubTo;

  --------------------------------------------------------------------------------

  procedure Mult(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG : out UnconstraightMatrix) is
  begin
    for i in ML'Range loop
           for j in 1..N loop
               MG(i, j) := 0;
               for k in 1..N loop
                   MG(i, j) := MG(i, j) + ML(i, j) * MR(k, j);
               end loop;
           end loop;
    end loop;
  end Mult;

  procedure Mult(Left : in UnconstraightVector; Right : in UnconstraightMatrix; result : out UnconstraightVector) is
  begin -- Mult
     for i in Left'Range loop
        result(i) := 0;
        for j in Left'Range loop
           result(i) := result(i) + Left(j) * Right(i, j);
        end loop;
     end loop;
  end Mult;

  procedure Mult(e : in Integer; MK : in UnconstraightMatrix; result : out UnconstraightMatrix) is
  begin
     for i in MK'Range loop
       for J in 1..N loop
              result(i, j) := MK(i, j)  * e;
        end loop;
     end loop;
  end Mult;

  procedure Mult(e : in Integer; K : in UnconstraightVector; result : out UnconstraightVector) is
  begin
     for i in K'Range loop
        result(i) := e * K(i);
     end loop;
  end Mult;

  --------------------------------------------------------------------------------

  procedure Copy(A : in UnconstraightVector; B : in out UnconstraightVector; From, To : in Integer) is
  begin -- Copy
    for i in From..To loop
      B(i) := A(i);
    end loop;
  end Copy;

  procedure Copy(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To : in Integer) is
  begin -- Copy
    for i in From..To loop
      for j in MA'Range loop
        MB(i,j) := MA(i,j);
      end loop;
    end loop;
  end Copy;


end Data;
