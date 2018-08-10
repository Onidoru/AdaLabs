-- PPKS
-- Lab 7
-- A = (B * C) * Z + d * T * (MO * MC)

with Data,Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control, Ada.Calendar;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control, Ada.Calendar;

procedure Lab1v2 is

nSize : constant Integer := 1000;
P : constant Integer := 100;

mMax : constant Integer := 10;
nMax : constant Integer := 10;

HRow : constant Integer := nSize / mMax;
H : constant Integer := nSize / P;

package Int_Data is new Data(nSize, P, H, HRow);
use Int_Data;

procedure Start_Tasks is
task type TNode(m, n : Integer) is
  pragma Storage_Size(1_975_000_000);

  entry TransferFromFirst(tB, tZ : UnconstraightVector; tMO : UnconstraightMatrix);
  entry TransferFromLast(tC, tT : UnconstraightVector; tMK : UnconstraightMatrix; td : Integer);
  entry TransferVector(tV : UnconstraightVector; fromIndex, toIndex : Integer);
  entry TransferInteger(tI : Integer);
end;

type TNode_Pointer is access TNode;

TNodeArray : array (Integer range 1..228, Integer range 1..228) of TNode_Pointer;

task body TNode is
  botLimitRow, topLimitRow : Integer;
  botLimitNode, topLimitNode : Integer;

  A, B, C, Z, T : VectorOfN;
  MO, MK : MatrixOfN;
  d : Integer;

  local_id : VarID;

  local_B, local_C, local_Z : VectorOfH;
  local_T : VectorOfN;

  local_MO : MatrixOfH;
  local_MK : MatrixOfN;
  local_d : Integer;

  local_s : Integer;
  local_V1 : VectorOfH;
  local_V2, local_V3 : VectorOfN;
  local_M1, local_M2, local_M3, local_M4 : MatrixOfH;

begin
  Put_Line("TNode" & Integer'Image(m)& "," & Integer'Image(n) & " has started");

  -- Calculate indexes
  botLimitRow := HRow * (m - 1) + 1;
  topLimitRow := HRow * m;
  botLimitNode := botLimitRow + h * (n - 1);
  topLimitNode := botLimitRow + h * n - 1;


  -- Generate and send B, Z, MO to the next row
  if ((m = 1) and (n = 1)) then
    Generate_Vector(B, 1);
    Generate_Vector(Z, 1);
    Generate_Matrix(MO, 1);

    Z(4) := 100;

    -- Send variables to the node 2,1
    if (mMax >= 2) then
      local_B := B(botLimitNode..topLimitNode);
      local_Z := Z(botLimitNode..topLimitNode);
      local_MO := MO(botLimitNode..topLimitNode);
      TNodeArray(2, 1).TransferFromFirst(
        B(topLimitRow+1..nSize),
        Z(topLimitRow+1..nSize),
        MO(topLimitRow+1..nSize)
      );
    end if;

    -- Send variables to the node 1,2
    TNodeArray(1, 2).TransferFromFirst(
      B(topLimitNode+1..topLimitRow),
      Z(topLimitNode+1..topLimitRow),
      MO(topLimitNode+1..topLimitRow)
    );

  end if;

  -- Nodes in the first column receive TransferFromFirst from the previous node
  -- in the column and send it to the next node in the column and to the next
  -- node in the row.
  if ((m /= 1) and (n = 1)) then

    declare
      tempB, tempZ : UnconstraightVector(botLimitRow..nSize);
      tempMO : UnconstraightMatrix(botLimitRow..nSize);
    begin

      accept TransferFromFirst(tB, tZ : UnconstraightVector; tMO : UnconstraightMatrix) do
        tempB := tB(tB'First..tB'Last);
        tempZ := tZ(tZ'First..tZ'Last);
        tempMO := tMO(tMO'First..tMO'Last);
      end TransferFromFirst;

      -- Take local variables for the current node
      local_B := tempB(botLimitNode..topLimitNode);
      local_Z := tempZ(botLimitNode..topLimitNode);
      local_MO := tempMO(botLimitNode..topLimitNode);

      -- Send variables to the next node in column
      if (m /= mMax) then
        TNodeArray(m+1, 1).TransferFromFirst(
          tempB(topLimitRow+1..tempB'Last),
          tempZ(topLimitRow+1..tempZ'Last),
          tempMO(topLimitRow+1..tempMO'Last)
        );
      end if;

      -- Send variables to the next node in the row
      TNodeArray(m, n+1).TransferFromFirst(
        tempB(topLimitNode+1..topLimitRow),
        tempZ(topLimitNode+1..topLimitRow),
        tempMO(topLimitNode+1..topLimitRow)
      );

    end;

  end if;

  -- Receive TransferFromFirst and send message to the next node in the row
  if (n /= 1) then

    declare
      tempB, tempZ : UnconstraightVector(botLimitNode..topLimitRow);
      tempMO : UnconstraightMatrix(botLimitNode..topLimitRow);
    begin

      accept TransferFromFirst(tB, tZ : UnconstraightVector; tMO : UnconstraightMatrix) do
        tempB := tB(tB'First..tB'Last);
        tempZ := tZ(tZ'First..tZ'Last);
        tempMO := tMO(tMO'First..tMO'Last);
      end TransferFromFirst;

      local_B := tempB(botLimitNode..topLimitNode);
      local_Z := tempZ(botLimitNode..topLimitNode);
      local_MO := tempMO(botLimitNode..topLimitNode);

      if (n /= nMax) then
        TNodeArray(m, n+1).TransferFromFirst(
          tempB(topLimitNode+1..topLimitRow),
          tempZ(topLimitNode+1..topLimitRow),
          tempMO(topLimitNode+1..topLimitRow)
        );
      end if;

    end;

  end if;

  -- ---------------------------------------------------------------------------

  -- Generate C, T, MK, d and send to the previous row
  if ((m = mMax) and (n = nMax)) then
    Generate_Vector(C, 1);
    Generate_Vector(T, 1);
    Generate_Matrix(MK, 1);
    d := 1;

    -- Send variables to the node m-1, n
    if (mMax >= 2) then
      local_C := C(botLimitNode..topLimitNode);
      local_T := T;
      local_MK := MK;
      local_d := d;
      TNodeArray(m-1, n).TransferFromLast(
        C(1..botLimitRow-1),
        T,
        MK,
        d
      );
    end if;

    -- Send variables to the node m,n-1
    TNodeArray(m, n-1).TransferFromLast(
      C(botLimitRow..botLimitNode-1),
      T,
      MK,
      d
    );
  end if;

  -- Nodes in the last column receive TransferFromLast from the next node
  -- in the colum and send it to the previous node in the column and previous
  -- node in the row.
  if ((m /= mMax) and (n = nMax)) then
    declare
      tempC : UnconstraightVector(1..topLimitNode);
    begin

      accept TransferFromLast(tC, tT : UnconstraightVector; tMK : UnconstraightMatrix; td : Integer) do
        tempC := tC(tC'First..tC'Last);
        local_T := tT;
        local_MK := tMK;
        local_d := td;
      end TransferFromLast;

      local_C := tempC(botLimitNode..topLimitNode);

      -- Send variables to the previous node in the row
      TNodeArray(m, n-1).TransferFromLast(
        tempC(botLimitRow..botLimitNode-1),
        local_T,
        local_MK,
        local_d
      );

      -- Send variables to the previous node in the column
      if (m /= 1) then
        TNodeArray(m-1, n).TransferFromLast(
          tempC(1..botLimitRow-1),
          local_T,
          local_MK,
          local_d
        );
      end if;

    end;

  end if;

  -- Receive TransferFromLast and send message to the previous node in the row
  if (n /= nMax) then

    declare
      tempC : UnconstraightVector(botLimitRow..topLimitNode);
    begin

      accept TransferFromLast(tC, tT : UnconstraightVector; tMK : UnconstraightMatrix; td : Integer) do
        tempC := tC(tC'First..tC'Last);
        local_T := tT;
        local_MK := tMK;
        local_d := tD;

      end TransferFromLast;

      local_C := tempC(botLimitNode..topLimitNode);

      if (n /= 1) then
        TNodeArray(m, n-1).TransferFromLast(
          tempC(botLimitRow..botLimitNode-1),
          local_T,
          local_MK,
          local_d
        );
      end if;

    end;

  end if;

  -- --------------------------------------------------------------------------

  -- counting scalar
  local_s := Scalar(local_B, local_C);

  -- Sending scalar to the 1,1 node
  -- All nodes excepting m,nMax need to receive at least one scalar
  if (n /= nMax) then
    accept TransferInteger(tI : Integer) do
      local_s := local_s + tI;
    end TransferInteger;
  end if;

  -- All nodes excepting m,1 need to send a scalar to the previous
  -- node in the row.
  if (n /= 1) then
    TNodeArray(m, n-1).TransferInteger(local_s);
  end if;

  -- All nodes in the first column need to receive a scalar from the next node
  -- in the column and send it to the previous node in the first colunn
  if (n = 1) then
    if (m = mMax) then
      TNodeArray(m-1, 1).TransferInteger(local_s);
    else
      accept TransferInteger(tI : Integer) do
        local_s := local_s + tI;
      end TransferInteger;
      if (m /= 1) then
        TNodeArray(m-1, 1).TransferInteger(local_s);
      end if;
    end if;
  end if;

  -- Send final scalar to all other nodes
  -- Generate and send B, Z, MO to the next row
  if ((m = 1) and (n = 1)) then
    -- Send variables to the node 2,1
    if (mMax >= 2) then
      TNodeArray(2, 1).TransferInteger(local_s);
    end if;

    -- Send variables to the node 1,2
    TNodeArray(1, 2).TransferInteger(local_s);

  end if;

  -- Nodes in the first column receive TransferFromFirst from the previous node
  -- in the column and send it to the next node in the column and to the next
  -- node in the row.
  if ((m /= 1) and (n = 1)) then

      accept TransferInteger(tI : Integer) do
        local_s := tI;
      end TransferInteger;

      -- Send variables to the next node in column
      if (m /= mMax) then
        TNodeArray(m+1, 1).TransferInteger(local_s);
      end if;

      -- Send variables to the next node in the row
      TNodeArray(m, n+1).TransferInteger(local_s);

  end if;

  -- Receive TransferFromFirst and send message to the next node in the row
  if (n /= 1) then

      accept TransferInteger(tI : Integer) do
        local_s := tI;
      end TransferInteger;

      if (n /= nMax) then
        TNodeArray(m, n+1).TransferInteger(local_s);
      end if;

  end if;

  -- --------------------------------------------------------------------------

  declare
    local_A : UnconstraightVector(botLimitNode..topLimitNode);
    -- local_V1, local_V2, local_V3 : UnconstraightVector(botLimitNode..topLimitNode);
    -- local_M1, local_M2, local_M3, local_M4 : UnconstraightMatrix(botLimitNode..topLimitNode);
  begin
    -- Count partial answers
    Mult(local_s, local_Z, local_V1);
    Mult(local_MO, local_MK, local_M1);
    Mult(local_T, local_M1, local_V2);
    Mult(local_d, local_V2, local_V3);
    SumTo(local_V1, local_V3, local_A);

    -- Answers need to be sent to the previous node and be merged

    -- Sending vector to the 1,1 node
    declare
      tempA : UnconstraightVector(botLimitNode..topLimitRow);
    begin

      -- All nodes excepting m,nMax need to receive at least one part of vector
      CopyCareful(tempA, local_A, botLimitNode, topLimitNode);

      if (n /= nMax) then

        accept TransferVector(tV : UnconstraightVector; fromIndex, toIndex : Integer) do
          CopyCareful(tempA, tV, fromIndex, toIndex);
        end TransferVector;


      end if;

      -- All nodes excepting m,1 need to send a local vector to the previous
      -- node in the row.
      if (n /= 1) then
        TNodeArray(m, n-1).TransferVector(tempA, botLimitNode, topLimitRow);
      end if;

      -- All nodes in the first column need to receive a merged row vector from the
      -- next node in the column and send it to the previous node in the first colunn
      if (n = 1) then

        declare
          mergedA : UnconstraightVector(botLimitRow..nSize);
        begin

          if (m = mMax) then
            TNodeArray(m-1, 1).TransferVector(tempA, botLimitRow, topLimitRow);
          else
            CopyCareful(mergedA, tempA, botLimitRow, topLimitRow);
            accept TransferVector(tV : UnconstraightVector; fromIndex, toIndex : Integer) do
              CopyCareful(mergedA, tV, fromIndex, toIndex);
            end TransferVector;
            if (m /= 1) then
              TNodeArray(m-1, 1).TransferVector(mergedA, botLimitRow, nSize);
            end if;
          end if;

          if (m = 1) then
            Output_Vector(mergedA, m, n);
          end if;

        end;
      end if;

    end;

  end;

  Put_Line("TNode" & Integer'Image(m)& "," & Integer'Image(n) & " has finished");
end TNode;


begin
  for m in reverse 1..mMax loop
    for n in reverse 1..nMax loop
      if (((n /= 1) or (m /= 1)) and ((m /= mMax) or (n /= nMax))) then
        TNodeArray(m, n) := new TNode(m, n);
      end if;
    end loop;
  end loop;

  TNodeArray(mMax, nMax) := new TNode(mMax, nMax);
  TNodeArray(1, 1) := new TNode(1, 1);
end Start_tasks;

begin
  Start_Tasks;
end Lab1v2;
