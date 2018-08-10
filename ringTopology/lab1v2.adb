-- PPKS

with Data,Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control;

procedure Lab1v2 is

N : Integer := 20;
P : Integer := 5;
H : Integer := N / P;

type ID_Numbers is range 1..12;

package Int_Data is new Data(N, P, H);
use Int_Data;

procedure Start_Tasks is
task type TNode(ID : ID_Numbers) is
  pragma Storage_Size(1_975_000_000);

  -- entry TransferInt(i : in Integer);
  entry TransferVector(v : in UnconstraightVector);
  -- entry TransferMatrix(m : in UnconstraightMatrix);
end;

type TNode_Pointer is access TNode;

TNodeArray : array (Positive range 1..12) of TNode_Pointer;

Current_Node : TNode_Pointer;

task body TNode is
  VA : VectorOfN;
  VH, local_VH : VectorOfH;
  botLimit, topLimit, MLimit, intID : Integer;
begin
  Put_Line("TNode" & ID_Numbers'Image(ID) & " has started");

  -- define limits
  MLimit := (N - N / P) / 2 + H;
  intID := Integer(ID);
  if (intID <= P / 2) then
    botLimit := H * (intID - 1);
    topLimit := H * intID;
  else
    botLimit := MLimit + H * (P - intID);
    topLimit := MLimit + H * (P - intID + 1);
  end if;

  if (ID = 1) then
    Generate_Vector(VA, 1);
    local_VH := VA(1..H);
    Put_Line("Tran");
    TNodeArray(2).TransferVector(VA(5..10));

  elsif (ID /= 1) then
    select
      when (intID <= P / 2) =>
        accept TransferVector(v : in UnconstraightVector) do
          Output_Vector(v);
          Put_Line("asd");
        end TransferVector;
      or
      when (intID > P / 2) =>
        accept TransferVector(v : in UnconstraightVector) do
          local_VH := v(v'First..H);
        end TransferVector;
    end select;
  end if;


  Put_Line("TNode" & ID_Numbers'Image(ID) & " has finished");
end TNode;

begin
  for i in reverse 1..P loop
    TNodeArray(i) := new TNode(ID_Numbers(i));
  end loop;
end Start_tasks;

begin
  Put_Line("Starting...");
  Start_Tasks;
end Lab1v2;
