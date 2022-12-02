with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;


procedure Main is
   Input_File : File_Type;
   Result    : Integer := 0;
   New_Value : Integer := 0;

   package Calories_Sum is new Ada.Containers.Vectors
    (Index_Type => Natural,
     Element_Type => Integer);
   package Sorter is new Calories_Sum.Generic_Sorting;

   Calories_List : Calories_Sum.Vector;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      declare
         Line : String := Get_Line (Input_File);
      begin
         if Line = "" then
            Calories_List.Append(New_Value);
            New_Value := 0; 
         else
            New_Value := New_Value + Integer'Value(Line);
         end if;
      end;
   end loop;
   Close (Input_File);

   Sorter.Sort(Calories_List);

   Put_Line ("First part answer: " & Calories_List.Last_Element'Image);

   for I in Calories_List.Last_Index - 2 .. Calories_List.Last_Index loop
      Result := Result + Calories_List (I);
   end loop;

   Put_Line ("Second part answer: " & Result'Image);

end Main;
