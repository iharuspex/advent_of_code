with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Interfaces; use Interfaces;

procedure Main is
   type UInt3 is mod 2 ** 3;
   for UInt3'Size use 3;

   type Processor_Type is record
      Register_A : Unsigned_32;
      Register_B : Unsigned_32;
      Register_C : Unsigned_32;
   end record;

   type Program_Instruction_Type is record
      Instruction : Unsigned_32;
      Operand : Unsigned_32;
   end record;

   function "=" (Left, Right : Program_Instruction_Type) return Boolean is
   begin
      return (Left.Instruction = Right.Instruction);
   end;

   package Program_Lists is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Program_Instruction_Type,
      "=" => "=");

   subtype Program_Instr_List is Program_Lists.Vector;


   Input_Program : Program_Instr_List;



   Status : Boolean := True;

   function Get_Val_By_Combo (Regs : Processor_Type; C_Op : Unsigned_32) return Unsigned_32 is
      Ret : Unsigned_32 := 0;
   begin
      case C_Op is
         when 0 .. 3 =>
            Ret := C_Op;
         when 4 =>
            Ret := Regs.Register_A;
         when 5 =>
            Ret := Regs.Register_B;
         when 6 =>
            Ret := Regs.Register_C;
         when others =>
            Ret := 0;
      end case;

      return Ret;
   end Get_Val_By_Combo;

   -- opcode 0
   procedure Adv (Regs : in out Processor_Type; C_Op : Unsigned_32) is
      Val : Unsigned_32 := Get_Val_By_Combo (Regs, C_Op);
   begin
      Regs.Register_A := Regs.Register_A / (2 ** Natural (Val));
      null;
   end Adv;

   -- opcode 1
   procedure Bxl (Regs : in out Processor_Type; L_Op : Unsigned_32) is
   begin
      Regs.Register_B := Regs.Register_B xor L_Op;
   end Bxl;

   -- opcode 2
   procedure Bst (Regs : in out Processor_Type; C_Op : Unsigned_32) is
      Val : Unsigned_32 := Get_Val_By_Combo (Regs, C_Op);
   begin
      Regs.Register_B := (Val mod 8) and 2#0000_0111#;
   end Bst;

   -- opcode 3
   function Jnz (Regs : Processor_Type; L_Op : Unsigned_32) return Unsigned_32 is
   begin
      if Regs.Register_A /= 0 then
         return L_Op;
      end if;

      return -1;
   end Jnz;

   -- opcode 4
   procedure Bxc (Regs : in out Processor_Type; L_Op : Unsigned_32) is
   begin
      Regs.Register_B := Regs.Register_B xor Regs.Register_C;
   end Bxc;

   -- opcode 5
   procedure Out_I (Regs : in Processor_Type; C_Op : Unsigned_32) is
      Val : Unsigned_32 := Get_Val_By_Combo (Regs, C_Op);
   begin
      Val := Val mod 8;
      Put (Integer(Val), 1);
      Put (",");
   end Out_I;

   -- opcode 6
   procedure Bdv (Regs : in out Processor_Type; C_Op : Unsigned_32) is
      Val : Unsigned_32 := Get_Val_By_Combo (Regs, C_Op);
   begin
      Regs.Register_B := Regs.Register_A / (2 ** Natural (Val));
   end Bdv;

   --  -- opcode 7
   procedure Cdv (Regs : in out Processor_Type; C_Op : Unsigned_32) is
      Val : Unsigned_32 := Get_Val_By_Combo (Regs, C_Op);
   begin
      Regs.Register_C := Regs.Register_A / (2 ** Natural (Val));
   end Cdv;

begin
   --  Input_Program.Append ((2, 4));
   --  Input_Program.Append ((1, 1));
   --  Input_Program.Append ((7, 5));
   --  Input_Program.Append ((1, 5));
   --  Input_Program.Append ((4, 5));
   --  Input_Program.Append ((0, 3));
   --  Input_Program.Append ((5, 5));
   --  Input_Program.Append ((3, 0));

   Input_Program.Append ((0, 1));
   Input_Program.Append ((5, 4));
   Input_Program.Append ((3, 0));

   declare
      --  Processor : Processor_Type :=
      --    (Register_A => 30344604,
      --     Register_B => 0,
      --     Register_C => 0);

      Processor : Processor_Type :=
        (Register_A => 729,
         Register_B => 0,
         Register_C => 0);

      Instr, Op : Unsigned_32;

      Ep : Program_Lists.Cursor := Input_Program.First;

      Jnz_Status : Unsigned_32 := 999;
   begin
      while Status loop
         Instr := Input_Program (Ep).Instruction;
         Op := Input_Program (Ep).Operand;

         case Instr is
            when 0 =>
               Adv (Processor, Op);
            when 1 =>
               Bxl (Processor, Op);
            when 2 =>
               Bst (Processor, Op);
            when 3 =>
               Jnz_Status := Jnz (Processor, Op);
            when 4 =>
               Bxc (Processor, Op);
            when 5 =>
               Out_I (Processor, Op);
            when 6 =>
               Bdv (Processor, Op);
            when 7 =>
               Cdv (Processor, Op);
            when others =>
               null;
         end case;

         if Jnz_Status = -1 then
            Status := False;
         else
            if Jnz_Status /= 999 then
               Ep := Input_Program.Find ((Jnz_Status, -1));
               Jnz_Status := 999;
            else
               Ep := Program_Lists.Next (Ep);
            end if;
         end if;
      end loop;
   end;
end Main;
