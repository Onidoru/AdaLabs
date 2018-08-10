-- PPKS

-- I/O:
-- 1\ MA, Z, MT
-- 2\ MO, MK

-- MA = max(Z) * MO + min(Z) * MT * MK
-- 1\ a_i = max(Z_H)
-- 2\ a = max(a, a_i)
-- 3\ b_i = min(Z_H)
-- 3\ b = min(b_i, b)
-- 4\ MA_H = a * MO_H + b * MT_H * MK
-- Shared resources: a, b, MK

with Data,Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Synchronous_Task_Control;

procedure Lab1v2 is

N : Integer := 4;
P : constant Integer := 4;
inputsExpected : constant Integer := 2;
L : Integer := N / P;


-- ID of threads, maximum number of allowed threads is 228.
type ID_Numbers is range 1..228;

package Int_Data is new Data(N);
  use Int_Data;

  Z : Vector;
  MA, MO, MT : Matrix;

  -- Sync & Resource monitors
  protected SyncMonitor is
    procedure InputSync;
    procedure MinZSync;
    procedure MaxZSync;
    procedure FinishCalcSync;

    entry WaitInputSync;
    entry WaitMinZSync;
    entry WaitMaxZSync;
    entry WaitFinishCalcSync;

    private
      InputFlag : Integer := 0;
      MinZFlag : Integer := 0;
      MaxZFlag : Integer := 0;
      FinishCalcFlag : Integer := 0;
  end SyncMonitor;

  -- Shared Resources: a, b, MK
  protected aResourceMonitor is
    procedure Set(v : in Integer);
    procedure Compare(v : in Integer);
    function Get return Integer;

    private
      a : Integer := -13370000;
  end aResourceMonitor;

  protected bResourceMonitor is
    procedure Set(v : in Integer);
    procedure Compare(v : in Integer);
    function Get return Integer;

    private
      b : Integer := 13370000;
  end bResourceMonitor;

  protected MKResourceMonitor is
    procedure Set(v : in Matrix);
    function Get return Matrix;

    private
      MK : Matrix;
  end MKResourceMonitor;

  -- Bodies of protected modules
  protected body SyncMonitor is
    procedure InputSync is
    begin
      InputFlag := InputFlag + 1;
    end InputSync;

    procedure MinZSync is
    begin
      MinZFlag := MinZFlag + 1;
    end MinZSync;

    procedure MaxZSync is
    begin
      MaxZFlag := MaxZFlag + 1;
    end MaxZSync;

    procedure FinishCalcSync is
    begin
      FinishCalcFlag := FinishCalcFlag + 1;
    end FinishCalcSync;

    entry WaitInputSync
      when InputFlag = inputsExpected is
        begin
          null;
    end WaitInputSync;

    entry WaitMinZSync
      when MinZFlag = P is
        begin
          null;
    end WaitMinZSync;

    entry WaitMaxZSync
      when MaxZFlag = P is
      begin
        null;
    end WaitMaxZSync;

    entry WaitFinishCalcSync
      when FinishCalcFlag = P is
      begin
        null;
    end WaitFinishCalcSync;

  end SyncMonitor;

  protected body aResourceMonitor is
    procedure Set(v : in Integer) is
    begin
      a := v;
    end Set;

    -- Finds maximum of a and v
    procedure Compare(v : in Integer) is
    begin
      a := Integer'Max(a, v);
    end Compare;

    function Get return Integer is
    begin
      return a;
    end Get;
  end aResourceMonitor;

  protected body bResourceMonitor is
    procedure Set(v : in Integer) is
    begin
      b := v;
    end Set;

    -- Finds minimum of b and v
    procedure Compare(v : in Integer) is
    begin
      b := Integer'Min(b, v);
    end Compare;

    function Get return Integer is
    begin
      return b;
    end Get;
  end bResourceMonitor;

  protected body MKResourceMonitor is
    procedure Set(v : in Matrix) is
    begin
      MK := v;
    end Set;

    function Get return Matrix is
    begin
      return MK;
    end Get;
  end MKResourceMonitor;


  -- Function to calculate.
  procedure ThreadFunc(botLimit, topLimit : in Integer) is
    local_a, local_b : Integer;
    local_MK, local_M1, local_M2, local_M3 : Matrix;
  begin
    SyncMonitor.WaitInputSync;

    Generate_Matrix(local_M1, 0);
    Generate_Matrix(local_M2, 0);
    Generate_Matrix(local_M3, 0);

    local_a := Max(Z, botLimit, topLimit);
    aResourceMonitor.Compare(local_a);
    SyncMonitor.MaxZSync;

    local_b := Min(Z, botLimit, topLimit);
    bResourceMonitor.Compare(local_b);
    SyncMonitor.MinZSync;

    SyncMonitor.WaitMaxZSync;
    SyncMonitor.WaitMinZSync;

    local_a := aResourceMonitor.Get;
    local_b := bResourceMonitor.Get;
    local_MK := MKResourceMonitor.Get;

    local_M1 := Mult(local_a, MO, botLimit, topLimit);
    local_M2 := Mult(local_b, MT, botLimit, topLimit);
    local_M3 := Mult(local_M2, local_MK, botLimit, topLimit);
    SumTo(local_M1, local_M3, MA, botLimit, topLimit);

    SyncMonitor.FinishCalcSync;
  end ThreadFunc;

  procedure Start_Tasks is
    -- All tasks are identical.
    task type TNode(ID : ID_Numbers) is
      pragma Storage_Size(1_975_000_000);
    end;

    type TNode_Pointer is access TNode;
    Current_Node : TNode_Pointer;

    task body TNode is
      botLimit, topLimit : Integer;
      local_MK : Matrix;
    begin
      Put_Line("TNode" &ID_Numbers'Image(ID)&" has started");

      if (ID = 1) then
        Generate_Matrix(MT, 1);
        Generate_Vector(Z, 1);

        SyncMonitor.InputSync;
      elsif (ID = 3) then
        Generate_Matrix(MO, 1);
        Generate_Matrix(local_MK, 1);
        MKResourceMonitor.Set(local_MK);

        SyncMonitor.InputSync;
      else
        null;
      end if;

      -- Parts of vectors/matrixes the concrete thread works with
      botLimit := (Integer(ID) - 1) * L + 1;
      topLimit := Integer(ID) * L;

      ThreadFunc(botLimit, topLimit);

      if (ID = 1) then
        SyncMonitor.WaitFinishCalcSync;
        if (N <= 24) then
          null;
          Output_Matrix(MA);
        end if;
      end if;

      Put_Line("TNode" &ID_Numbers'Image(ID)&" has finished");
    end TNode;

    begin
      for i in 1..P loop
        Current_Node := new TNode(ID_Numbers(i));
      end loop;
  end Start_tasks;

  begin
    Put_Line("Starting...");
    Start_Tasks;
end Lab1v2;
