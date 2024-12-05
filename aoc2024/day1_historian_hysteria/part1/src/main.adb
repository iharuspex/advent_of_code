with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Containers.Vectors;

procedure Main is
   Input_File : File_Type;

   Line : String (1 .. 5);

   package List_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => Integer);

   package List_Sorting is new
     List_Vectors.Generic_Sorting;
   use List_Sorting;

   List1 : List_Vectors.Vector;
   List2 : List_Vectors.Vector;

   Result : Integer := 0;

   Val1 : Integer;
   Val2 : Integer;

begin
   -- reading input data
   Open (File => Input_File, Mode => In_File, Name => "input.txt");

   while not End_Of_File (Input_File) loop
      Get (Input_File, Val1);
      Get (Input_File, Val2);
      Put_Line (Val1'Image & " - " & Val2'Image);
      List1.Append (Val1);
      List2.Append (Val2);
   end loop;

   -- sorting
   Sort (List1);
   Sort (List2);

   -- calculating
   for I in List1.First_Index .. List1.Last_Index loop
      if List1.Element (I) < List2.Element (I) then
         Result := Result + (List2.Element (I) - List1.Element (I));
      else
         Result := Result + (List1.Element (I) - List2.Element (I));
      end if;
   end loop;

   Put_Line (Result'Image);

end Main;
