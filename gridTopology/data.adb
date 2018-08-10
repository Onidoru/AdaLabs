with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Numerics.Discrete_Random, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_Io, GNAT.OS_Lib;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings, Ada.Strings.Unbounded, Ada.Text_IO.Unbounded_IO, GNAT.OS_Lib;

package body Data is
  procedure Generate_Matrix(M : in out UnconstraightMatrix; v : in Integer) is
  begin
    for i in M'First..M'Last loop
      for j in M(i)'First..M(i)'Last loop
        M(i)(j) := v;
      end loop;
    end loop;
  end Generate_Matrix;

  procedure Generate_Vector(A : in out UnconstraightVector; v : in Integer) is
  begin
      for i in A'First..A'Length loop
        A(i) := v;
      end loop;
  end Generate_Vector;

  procedure Generate_Path(A : in out ID_Path; v : in Integer) is
  begin
      for i in A'First..A'Length loop
        A(i) := v;
      end loop;
  end Generate_Path;

  procedure Output_Vector(A : in UnconstraightVector; m_id, n_id : Integer) is
    Str : Unbounded_String := To_Unbounded_String("[" & Integer'Image(m_id) & Integer'Image(n_id) & "]" );
  begin
      for i in A'First..A'Last loop
        if (i /= A'First) then
          Append(Str, ", ");
        end if;
        Append(Str, Integer'Image(A(i)));
      end loop;
      Put_Line(str);
  end Output_Vector;

  procedure Output_Path(A : in ID_Path; id : Integer) is
        Str : Unbounded_String := To_Unbounded_String("[" & "]");
  begin
      for i in A'First..P loop
        if (i /= A'First) then
          Append(Str, ", ");
        end if;
        Append(Str, Integer'Image(A(i)));
      end loop;
      Put_Line(str);
  end Output_Path;

  procedure Output_Matrix(MA : in UnconstraightMatrix; m_id, n_id : Integer) is
  begin
    for i in MA'First..MA'Last loop
        Output_Vector(MA(i), m_id, n_id);
        -- New_Line;
      end loop;
  end Output_Matrix;

  --------------------------------------------------------------------------------

  procedure SumTo(ML : in UnconstraightMatrix; MR : in UnconstraightMatrix; MG : out UnconstraightMatrix) is
  begin
     for i in MG'First..MG'Last loop
          for j in 1..N loop
           MG(i)(j) := ML(i)(j) + MR(i)(j);
          end loop;
     end loop;
  end SumTo;

  procedure SubTo(ML: in UnconstraightMatrix; MR: in UnconstraightMatrix; MG: out UnconstraightMatrix) is
  begin
     for i in ML'First..ML'Last loop
          for j in 1..N loop
           MG(i)(j) := ML(i)(j) + MR(i)(j);
          end loop;
     end loop;
  end SubTo;

  --------------------------------------------------------------------------------

  procedure SumTo(L: in UnconstraightVector; R: in UnconstraightVector; G: out UnconstraightVector) is
    k : Integer := 0;
  begin
     for i in L'Range loop
       G(G'First+k) := L(i) + R(i);
       k := k + 1;
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
    Generate_Matrix(MG, 0);

    for i in ML'First..ML'Last loop
           for j in MR(i)'First..MR(i)'Last loop
               for k in MR'First..MR'Last loop
                  MG(i)(j) := MG(i)(j) + ML(i)(k) * MR(k)(j);
               end loop;
           end loop;
    end loop;

  end Mult;

  procedure Mult(Left : in UnconstraightVector; Right : in UnconstraightMatrix; result : out UnconstraightVector) is
  begin -- Mult

     for i in Right'Range loop
        result(i) := 0;
        for j in Left'Range loop
           result(i) := result(i) + Left(j) * Right(i)(j);
        end loop;
     end loop;

  end Mult;

  procedure Mult(e : in Integer; MK : in UnconstraightMatrix; result : out UnconstraightMatrix) is
  begin
     for i in MK'Range loop
       for J in 1..N loop
              result(i)(j) := MK(i)(j)  * e;
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
    j : Integer := 1;
  begin -- Copy
    for i in from..to loop
      B(j) := A(i);
      j := j+1;
    end loop;
  end Copy;

  procedure Copy(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To : in Integer) is
    k : Integer := 1;
  begin -- Copy
    -- MB := MA(from..to);

    for i in From..To loop
    --   -- for j in 1..N loop
        Copy(MA(i), MB(k), 1, N);
    --     MB(k) := MA(i);
        k := k+1;
      end loop;
    -- end loop;
  end Copy;

  procedure CopyCareful(MA : in UnconstraightMatrix; MB : in out UnconstraightMatrix; From, To, fromR : in Integer) is
    k : Integer := 1;
  begin -- Copy
    for i in From..To loop
      Copy(MA(i), MB(fromR+k), 1, N);
      k := k + 1;
    end loop;
  end CopyCareful;

  procedure CopyCareful(A : in out UnconstraightVector; B : in UnconstraightVector; fromA, toA: in Integer) is
  begin -- Copy
    for i in fromA..toA loop
      A(i) := B(i);
    end loop;
  end CopyCareful;

  --------------------------------------------------------------------------------

  -- DEPRECATED
  -- Most of the topology logic is hidden there.
  -- (it's actually in the ./main binary)
  -- Current realized topology is a ring connection between nodes.
  -- Kinda meme thing, because it's originally written in go, and Im just 
  -- trying to reuse my own code here. But it seems it came to far.
  function FindTheWay(fromNode, toNode : Integer) return ID_Path is
    Result    : Integer;

    pRaw : String := Integer'Image(P);
    fromRaw : String := Integer'Image(fromNode-1);
    toRaw : String := Integer'Image(toNode-1);

    pToCommand : String :=  pRaw(2 .. pRaw'Last);
    fromToCommand : String :=  fromRaw(2 .. fromRaw'Last);
    toToCommand : String :=  toRaw(2 .. toRaw'Last);

    Arguments : Argument_List :=
                  (  1 => new String'("-P"),
                     2 => new String'(pToCommand),
                     3 => new String'("-from"),
                     4 => new String'(fromToCommand),
                     5 => new String'("-to"),
                     6 => new String'(toToCommand)
                  );
     File : File_Type;
     resultArr : ID_Path;
     i : Integer := 1;

  begin
    Generate_Path(resultArr, 0);
    Spawn
    (  Program_Name           => "./main",
       Args                   => Arguments,
       Output_File_Descriptor => Standout,
       Return_Code            => Result
    );
    for Index in Arguments'Range loop
       Free (Arguments (Index));
    end loop;

    Open (File => File,
           Mode => In_File,
           Name => "result.txt");
     loop
        exit when End_Of_File (File);
        resultArr(i) := Integer'Value(Get_Line(File)) + 1;
        i := i + 1;
     end loop;
     Close (File);


    return resultArr;
  end FindTheWay;

  function Find_Minimum(MA : in UnconstraightMatrix) return Integer is
    min : Integer := 13370;
  begin -- Find_Minimum
    for i in MA'First..MA'Last loop
      for j in MA(i)'First..MA(i)'Last loop
        if (MA(i)(j) < min) then
          min := MA(i)(j);
        end if;
      end loop;
    end loop;
    return min;
  end Find_Minimum;

  function Scalar(A, B : in UnconstraightVector) return Integer is
    result : Integer;
  begin
    result := 0;
    for i in A'First..A'Last loop
      result := result + A(i) * B(i);
    end loop;
    return result;
  end Scalar;

end Data;
