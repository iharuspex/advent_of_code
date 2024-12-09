with Ada.Text_IO; use Ada.Text_IO;


procedure Main is
   Input_File : File_Type;

begin
   Open (Input_File, In_File, "input.txt");

   Put_Line ("Hello from Advent of Code template program!");

   Close (Input_File);

end Main;
