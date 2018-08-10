with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Numerics.Discrete_Random, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_Io;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_IO;

package body Data is


    procedure Generate_Vector(A : out Vector; v : in Integer) is
        begin
            for i in 1..N loop
                A(i) := v;
            end loop;
        end Generate_Vector;

    procedure Generate_Matrix(MA : out Matrix; v : in Integer) is
        begin
            for i in 1..N loop
                Generate_Vector(MA(i), v);
            end loop;
        end Generate_Matrix;

    procedure Input_Vector(Name: in string; A: out Vector) is
        begin
            Put("Input vector");
            Put_Line(Name);
            for i in 1..N loop
                Get(A(i));
            end loop;
        end Input_Vector;

    procedure Input_Matrix(Name: in String; MA: out Matrix) is
        A : Vector;
        begin
            Put("Input matrix ");
            Put_Line(Name);
            for i in 1..N loop
                for j in 1..N loop
                    Get(A(j));
                end loop;
                MA(i) := A;
            end loop;
        end Input_Matrix;

    procedure Output_Vector(A : in Vector) is
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

    procedure Output_Matrix(MA: in Matrix) is
        begin
            for i in MA'First..MA'Last loop
                Output_Vector(MA(i));
            end loop;
        end Output_Matrix;

    function "+"(L, R : in Vector) return Vector is
        S : Vector;
        begin
            for i in 1..N loop
                S(i) := L(i) + R(i);
            end loop;
            return S;
        end "+";

    function "-"(L, R : in Vector) return Vector is
        S : Vector;
        begin
            for i in 1..N loop
                S(i) := L(i) - R(i);
            end loop;
            return S;
        end "-";

    function "+"(ML, MR : in Matrix) return Matrix is
        MC : Matrix;
        begin
            for i in 1..N loop
                for j in 1..N loop
                    MC(i)(j) := ML(i)(j) + MR(i)(j);
                end loop;
            end loop;
            return MC;
        end "+";

    function "*"(L, R : in Vector) return Integer is
        p : Integer;
        begin
            p := 0;
            for i in 1..N loop
                p := p + L(i) * R(i);
            end loop;
            return p;
        end "*";



      function "*"(A : in Vector; e : in Integer) return Vector is
        B : Vector;
        begin
            for i in 1..N loop
                 B(i) := A(i)  * e;
            end loop;

            return B;
      end "*";




    function "*"(R : in Vector; MR : in Matrix) return Vector is
        P : Vector;
        begin
            for i in 1..N loop
                P(i) := 0;
                for j in 1..N loop
                    P(i) := P(i) + MR(i)(j) * R(j);
                end loop;
            end loop;
            return P;
    end "*";


    function Max(V : in Vector) return Integer is
        max : Integer;
        begin
            max := 0;
            for i in 1..N loop
                if V(i) > max then
                    max := V(i);
                end if;
            end loop;
            return max;
    end Max;

    function Max(MR : in Matrix) return Integer is
        max : Integer;
        begin
            max := 0;
            for i in 1..N loop
             for j in 1..N loop
                if MR(i)(j) > max then
                    max := MR(i)(j);
                end if;
              end loop;
            end loop;
         return max;
    end Max;

--------------------------------------------------------------------------------

procedure SumTo(ML: in Matrix; MR: in Matrix; MG: in out Matrix; first :in Integer; last :in Integer) is
begin
   for i in first..Last loop
        for j in 1..N loop
         MG(i)(j) := ML(i)(j) + MR(i)(j);
        end loop;
   end loop;
end SumTo;

procedure SubTo(ML: in Matrix; MR: in Matrix; MG: out Matrix; first :in Integer; last :in Integer) is
begin
   for i in first..Last loop
        for j in 1..N loop
         MG(i)(j) := ML(i)(j) - MR(i)(j);
        end loop;
   end loop;
end SubTo;

--------------------------------------------------------------------------------

procedure SumTo(L: in Vector; R: in Vector; G: out Vector; first :in Integer; last :in Integer) is
begin
   for i in first..Last loop
     G(i) := L(i) + R(i);
   end loop;
end SumTo;

procedure SubTo(L: in Vector; R: in Vector; G: out Vector; first :in Integer; last :in Integer) is
begin
   for i in first..Last loop
     G(i) := L(i) - R(i);
   end loop;
end SubTo;

--------------------------------------------------------------------------------

procedure MultTo(A, B: in Matrix; C : in out Matrix; From, To: in Integer) is
   buf: Integer;
begin
   for i in 1..N loop
      for j in from..to loop
         buf := 0;
         for k in 1..N loop
            buf := buf + A(i)(k) * B(k)(j);
         end loop;
         C(i)(j) := Buf;
      end loop;
   end loop;
end MultTo;


function Mult(ML: in Matrix; MR: in Matrix; first : in Integer; last : in Integer) return Matrix is
MG : Matrix;
begin
  for i in first..last loop
         for j in 1..N loop
             MG(i)(j) := 0;
             for k in 1..N loop
                 MG(i)(j) := MG(i)(j) + ML(i)(j) * MR(k)(j);
             end loop;
         end loop;
  end loop;
 return MG;
end Mult;

function Mult(Left : in Vector; Right : in Matrix; From : Integer; To : in Integer) return Vector is
 result : Vector;
begin -- Mult
   for i in From..To loop
      result(i) := 0;
      for j in Left'range loop
         result(i) := result(i) + Left(j) * Right(i)(j);
      end loop;
   end loop;
   return result;
end Mult;


function Mult(e : in Integer; MK : in Matrix; first :in Integer; last :in Integer) return Matrix is
  result : Matrix;
begin
   for i in first..last loop
     for J in 1..N loop
            result(i)(j) := MK(i)(j)  * e;
      end loop;
   end loop;
   return result;
end Mult;

function Mult(e : in Integer; K : in Vector; first : in Integer; last : in Integer) return Vector is
  result : Vector;
begin
   for i in first..last loop
             result(i) := e * K(i);
   end loop;
   return result;
end Mult;

procedure MultTo(Left : in Vector; Right : in Matrix; ToVector : in out Vector; From : Integer; To : in Integer) is
begin -- Mult
   for i in From..To loop
      ToVector(i) := 0;
      for j in Left'range loop
         ToVector(i) := ToVector(i) + Left(j) * Right(i)(j);
      end loop;
   end loop;
end MultTo;

--------------------------------------------------------------------------------

procedure Copy(A : in Vector; B : in out Vector; From, To : in Integer) is
begin -- Copy
  for i in From..To loop
    B(i) := A(i);
  end loop;
end Copy;

--------------------------------------------------------------------------------

function Scalar(A : in Vector; B : in Vector; from : in Integer; to : in Integer) return Integer is
  result : Integer;
begin
  result := 0;
  for i in from..to loop
    result := result + A(i) * B(i);
  end loop;
  return result;
end Scalar;

--------------------------------------------------------------------------------

function Min(A : in Vector; from : in Integer; to : in Integer) return Integer is
  result : Integer;
begin
  result := 1337000;
  for i in from..to loop
    result := Integer'Min(result, A(i));
  end loop;
  return result;
end Min;

function Max(A : in Vector; from : in Integer; to : in Integer) return Integer is
  result : Integer;
begin
  result := -1337000;
  for i in from..to loop
    result := Integer'Max(result, A(i));
  end loop;
  return result;
end Max;

end Data;
