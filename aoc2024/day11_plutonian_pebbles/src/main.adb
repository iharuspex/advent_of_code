with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Main is
   --  Filename : constant String := "test.txt";
   Filename : constant String := "input.txt";

   type Integer_Pair is record
      X : Long_Integer;
      T : Integer;
   end record;

   function Hash (Key : Integer_Pair) return Ada.Containers.Hash_Type;
   function Equivalent_Keys (Left, Right : Integer_Pair) return Boolean;

   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Long_Integer);
   use Natural_Vectors;

   package Integer_Pair_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Integer_Pair,
      Element_Type => Long_Integer,
      Hash => Hash,
      Equivalent_Keys => Equivalent_Keys,
      "=" => "=");

   F      : File_Type;
   Memo   : Integer_Pair_Maps.Map;
   Stones : Vector;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Integer_Pair) return Ada.Containers.Hash_Type is
      use Ada.Containers;
      function Long_Integer_Hash (Value : Long_Integer) return Hash_Type is
         Temp   : Long_Integer := Value;
         Result : Hash_Type    := 0;
      begin
         while Temp /= 0 loop
            Result := Result xor Hash_Type (Temp mod 1_000);
            Temp   := Temp / 1_000;
         end loop;
         return Result;
      end Long_Integer_Hash;
   begin
      return Long_Integer_Hash (Key.X) xor Hash_Type (Key.T);
   end Hash;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys (Left, Right : Integer_Pair) return Boolean is
   begin
      return Left.X = Right.X and Left.T = Right.T;
   end Equivalent_Keys;

   ------------------
   -- Print_Vector --
   ------------------

   procedure Print_Vector (Vec : Vector) is
   begin
      for I of Vec loop
         Put (Item => I, Width => 1);
         Put (" ");
      end loop;
      New_Line;
   end Print_Vector;

   -----------
   -- Solve --
   -----------

   function Solve (X : Long_Integer; T : Integer) return Long_Integer is
      Pair : Integer_Pair := (X, T);
      Ret  : Long_Integer;

      Number_Str : String  := Trim (Long_Integer'Image (X), Ada.Strings.Left);
      Count      : Natural := Number_Str'Length;
   begin
      if Memo.Contains (Pair) then
         return Memo.Element (Pair);
      end if;

      if T = 0 then
         Ret := 1;
      elsif X = 0 then
         Ret := Solve (1, T - 1);
      elsif Count mod 2 = 0 then
         declare
            DStr      : Unbounded_String :=
              To_Unbounded_String (Long_Integer'Image (X));
            Left_Str  : String           := Number_Str (1 .. Count / 2);
            Right_Str : String           :=
              Number_Str ((Count / 2) + 1 .. Number_Str'Last);
            Left      : Long_Integer     := Long_Integer'Value (Left_Str);
            Right     : Long_Integer     := Long_Integer'Value (Right_Str);
         begin
            Ret := Solve (Left, T - 1) + Solve (Right, T - 1);
         end;
      else
         Ret := Solve (X * 2_024, T - 1);
      end if;

      Memo.Include (Pair, Ret);
      return Ret;
   end Solve;

   ---------------
   -- Solve_All --
   ---------------

   function Solve_All (Stones : Vector; T : Integer) return Long_Integer is
      Sum : Long_Integer := 0;
   begin
      for X of Stones loop
         Sum := Sum + Solve (X, T);
      end loop;
      return Sum;
   end Solve_All;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Num : Long_Integer := 0;
      begin
         Get (F, Num);
         Stones.Append (Num);
      end;
   end loop;

   Print_Vector (Stones);

   Put_Line ("Part1: " & Long_Integer'Image (Solve_All (Stones, 25)));
   Put_Line ("Part2: " & Long_Integer'Image (Solve_All (Stones, 75)));
end Main;
