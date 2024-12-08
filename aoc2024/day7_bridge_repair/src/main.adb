with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Interfaces; use Interfaces;
with Ada.Containers.Vectors;

procedure Main is
   type Operators_Type is array (Positive range <>) of Character;

   subtype Long_Natural is Long_Long_Integer range 0 .. +(2**63 - 1);

   package Natural_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Positive,
        Element_Type => Long_Natural);

   package Character_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Positive,
        Element_Type => Character);

   F : File_Type;


   --------------------
   -- Print_Equation --
   --------------------

   procedure Print_Equation (Val : Long_Natural; Num : Natural_Vectors.Vector) is
   begin
      Put (Val'Image & " :");
      for I in Num.First_Index..Num.Last_Index loop
         Put (Num.Element(I)'Image & " ");
      end loop;
      New_Line;
   end Print_Equation;

   ------------------
   -- Parse_String --
   ------------------

   procedure Parse_String (Line : String; Val : out Long_Natural; Vec : out Natural_Vectors.Vector) is
      Start_Idx : Natural := 1;
      End_Idx : Natural := 1;
      Last : Natural := 0;
      Num : Long_Natural := 0;
   begin
      -- get Val
      End_Idx := Index (Line, ":");
      Get(Line (Line'First .. End_Idx-1), Val, Last);

      -- get Vec
      Start_Idx := End_Idx + 2; -- colon+space
      loop
         End_Idx := Index (Line (Start_Idx .. Line'Last), " ");
         if End_Idx = 0 then
            End_Idx := Line'Last + 1;
         end if;

         Get (Line (Start_Idx .. End_Idx - 1), Num, Last);
         Vec.Append (Num);

         exit when End_Idx = Line'Last + 1;
         Start_Idx := End_Idx + 1;
      end loop;
   end Parse_String;

   ------------
   -- Concat --
   ------------

   function Concat (A, B : Long_Natural) return Long_Natural is
      A_Str : String := Long_Natural'Image(A);
      B_Str : String := Long_Natural'Image(B);
      Result_Str : String := A_Str(2 .. A_Str'Last) & B_Str(2 .. B_Str'Last);
      Result : Long_Natural;
   begin
      Result := Long_Natural'Value(Result_Str);
      return Result;
   end Concat;

   ---------------
   -- Calculate --
   ---------------

   function Calculate (Vec : Natural_Vectors.Vector; Operators : Operators_Type)
                       return Long_Natural
   is
      Operands : Natural_Vectors.Vector := Vec;
      Eq_Res : Long_Natural := Operands (1);
   begin
      for I in Operators'Range loop
         if (Operators (I) = '+') then
            Eq_Res := Eq_Res + Operands (I+1);
         elsif Operators (I) = '*' then
            Eq_Res := Eq_Res * Operands (I+1);
         else
            Eq_Res := Concat (Eq_Res, Operands (I+1));
         end if;

         exit when Operands.First_Index = Operands.Last_Index;
      end loop;

      return Eq_Res;
   end Calculate;

   ----------------------
   -- Process_Equation --
   ----------------------

   function Process_Equation_P1 (Val : Long_Natural; Operands : in out Natural_Vectors.Vector) return Boolean
   is
      Operators : Operators_Type (1 .. Operands.Last_Index-1) := (others => '+');
      Operators_Bits : Natural := Operands.Last_Index-1;
      Operators_Max_Value : Natural := 2**Operators_Bits - 1;

      Eq_Res : Long_Natural := 0;
   begin
      for Value in 0 .. Operators_Max_Value loop
         for Bit in 0 .. Operators_Bits - 1 loop
            if (Unsigned_16(Value) and Unsigned_16(2**Bit)) /= 0 then
               --  Put ("1");
               Put ("* ");
               Operators (Bit+1) := '*';
            else
               --  Put ("0");
               Put ("+ ");
               Operators (Bit+1) := '+';
            end if;
         end loop;
         Eq_Res := Calculate (Operands, Operators);
         Put_Line (" = " & Eq_Res'Image);

         exit when Eq_Res = Val;
      end loop;

      if Eq_Res = Val then
         return True;
      end if;

      return False;
   end Process_Equation_P1;

   -------------------------
   -- Process_Equation_P2 --
   -------------------------

   procedure Process_Equation_P2 (Val : Long_Natural;
                                  Operands : in out Natural_Vectors.Vector;
                                  Operators_Num : Natural;
                                  Operators : in out Operators_Type;
                                  Index : Natural;
                                  Stop : in out Boolean)
   is
      Operator : String := "+*|";
      Res_Eq : Long_Natural := 0;

   begin
      if Index > Operators_Num then
         for I in Operators'Range loop
            Put(Operators(I) & " ");
            --
            Res_Eq := Calculate (Operands, Operators);
            if Res_Eq = Val then
               Stop := True;
               exit;
            end if;
         end loop;
         New_Line;
      else
         for I in Operator'Range loop
            Operators(Index) := Operator(I);
            Process_Equation_P2(Val, Operands, Operators_Num, Operators, Index + 1, Stop);
            if Stop then
               exit;
            end if;
         end loop;
      end if;
   end Process_Equation_P2;

   --------------------
   -- Process_String --
   --------------------

   function Process_String_P1 (Line : String) return Long_Natural
   is
      Val : Long_Natural;
      Vec : Natural_Vectors.Vector;
      Stats : Boolean;
   begin
      Parse_String (Line, Val, Vec);
      Print_Equation (Val, Vec);

      Stats := Process_Equation_P1(Val, Vec);
      New_Line;

      if Stats = True then
         return Val;
      end if;

      return 0;
   end Process_String_P1;

   -----------------------
   -- Process_String_P2 --
   -----------------------

   function Process_String_P2 (Line : String) return Long_Natural
   is
      Val : Long_Natural;
      Vec : Natural_Vectors.Vector;
      --  Stats : Boolean;
      Stop : Boolean := False;

      Eq_Res : Long_Natural := 0;
   begin
      Parse_String (Line, Val, Vec);
      Print_Equation (Val, Vec);

      declare
         Operators : Operators_Type (1 .. Vec.Last_Index-1) := (others => '+');
         Operators_Num : Natural := Vec.Last_Index-1;
      begin
         Process_Equation_P2 (Val, Vec, Operators_Num, Operators, 1, Stop);
         New_Line;
      end;


      if Stop = True then
         return Val;
      end if;

      return 0;
   end Process_String_P2;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (F : File_Type) is
      Result : Long_Natural := 0;
   begin
      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
         begin
            Result := Result + Process_String_P1 (Line);
         end;
      end loop;

      Put_Line ("Part1: " & Result'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------

   procedure Part2 (F : File_Type) is
      Result : Long_Natural := 0;
   begin
      while not End_Of_File (F) loop
         declare
            Line : String := Get_Line (F);
         begin
            Result := Result + Process_String_P2 (Line);
         end;
      end loop;

      Put_Line ("Part2: " & Result'Image);
   end Part2;

begin
   Open (F, In_File, "input.txt");

   Part1 (F);
   Reset (F);
   Part2 (F);

   Close (F);

end Main;
