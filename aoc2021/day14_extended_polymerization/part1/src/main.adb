with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;


procedure Main is

   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 3);
   use B_Str;

   function B_Str_Hash is new
     Ada.Strings.Bounded.Hash (B_Str);

   package Insertions_Rules is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => B_Str.Bounded_String,
        Element_Type    => B_Str.Bounded_String,
        Hash            => B_Str_Hash,
        Equivalent_Keys => B_Str."=",
        "="             => B_Str."=");
   use Insertions_Rules;

   procedure Print_Rules (R: Map) is
   begin
      for K in R.Iterate loop
         Put_Line (B_Str.To_String (Key (K)) & " -> " & B_Str.To_String (Element (K)));
      end loop;
   end Print_Rules;


   procedure String_Split
     (In_Str: in String;
      Split_By: in String;
      Part1: out B_Str.Bounded_String;
      Part2: out B_Str.Bounded_String)
   is
      Delimiter_Idx : Natural := 0;
   begin
      Delimiter_Idx := Index
        (Source => In_Str,
         Pattern => Split_By,
         From => In_Str'First);

      Part1 := B_Str.To_Bounded_String (In_Str (In_Str'First .. Delimiter_Idx - 1));
      Part2 := B_Str.To_Bounded_String (In_Str (Delimiter_Idx + Split_By'Length .. In_Str'Last));
   end String_Split;


   function Do_Insert (Str: String; Rules: Map) return Unbounded_String is
      Offset : Natural := 1;
      Result : String (1 .. Str'Length + Str'Length - 1);
   begin
      for I in Str'Range loop
         if I + 1 > Str'Last then
            Result (Offset) := Str (I);
            exit;
         end if;

         Result (Offset) := Str (I);
         Result (Offset + 1) := To_String (Rules (To_Bounded_String (Str (I .. I + 1))))(1);
         Offset := Offset + 2;
      end loop;

      return To_Unbounded_String (Result);
   end Do_Insert;


   function Solve (Str: String) return Natural is
      package Res is new
        Ada.Containers.Indefinite_Ordered_Maps
          (Key_Type        => Character,
           Element_Type    => Natural);

      Count : Res.Map := Res.Empty_Map;
      Max : Natural := 0;
      Min : Natural := 0;
   begin
      for C of Str loop
         if (Count.Contains (C)) then
            Count (C) := Count (C) + 1;
         else
            Count.Include (C, 1);
            end if;
      end loop;

      Max := Count.First_Element;
      Min := Count.First_Element;

      for K in Count.Iterate loop
         Put_Line (Res.Key (K) & ": " & Res.Element (K)'Image);

         if Max < Res.Element (K) then
            Max := Res.Element (K);
         end if;

         if Min > Res.Element (K) then
            Min := Res.Element (K);
         end if;
      end loop;


      return Max - Min;
   end;

   Input_File : File_Type;
   Rules : Map := Empty_Map;

begin
   Open (Input_File, In_File, "input.txt");

   declare
      Polymer_Template : String := Get_Line (Input_File);

      Polymer : Unbounded_String := To_Unbounded_String (Polymer_Template);
   begin
      Skip_Line (Input_File);

      while not End_Of_File (Input_File) loop
         declare
            Line : String := Get_Line (Input_File);

            K, E : B_Str.Bounded_String;
         begin
            String_Split (Line, " -> ", K, E);
            Rules.Include (K, E);
         end;
      end loop;

      Print_Rules (Rules);

      -- Steps!
      for I in 1 .. 10 loop
         Polymer := Do_Insert (To_String (Polymer), Rules);
         --Put_Line ("Step" & I'Image & ":" & To_String (Polymer));
      end loop;

      Put_Line ("Result: " & Solve(To_String (Polymer))'Image);
   end;

   Close (Input_File);

end Main;
