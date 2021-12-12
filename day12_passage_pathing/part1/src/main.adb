with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;

procedure Main is
   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 5);
   use B_Str;

   function Hash is new Ada.Strings.Bounded.Hash (B_Str);

   package Str_Vector is new
     Ada.Containers.Vectors (Positive, Bounded_String);
   use Str_Vector;

   package Path_HM is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Bounded_String,
        Element_Type    => Vector,
        Hash            => Hash,
        Equivalent_Keys => "=");
   use Path_HM;

   Input_File : File_Type;

   M : Map := Empty_Map;


   procedure String_Split (In_Str: in String;
                           Split_By: in String;
                           Part1: out Bounded_String;
                           Part2: out Bounded_String) is
      Delimiter_Idx : Natural := 0;
   begin
      Delimiter_Idx := Index
        (Source => In_Str,
         Pattern => Split_By,
         From => In_Str'First);

      Part1 := To_Bounded_String (In_Str (In_Str'First .. Delimiter_Idx - 1));
      Part2 := To_Bounded_String (In_Str (Delimiter_Idx + 1 .. In_Str'Last));
   end String_Split;

begin
   Open (Input_File, In_File, "input_test.txt");

   while not End_Of_File (Input_File) loop

      declare
         Line : String := Get_Line (Input_File);
         A, B : Bounded_String;
      begin
         String_Split (Line, "-", A, B);

         Put_Line (To_String (A) & " <-> " & To_String (B));

         if A = "start" or A = "end" then
            Put_Line ("AAA");
         elsif B = "start" or B = "end" then
            Put_Line ("EEE");
         end if;
      end;

   end loop;


   Close (Input_File);

end Main;
