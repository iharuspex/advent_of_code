with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   Filename : constant String := "test.txt";
   --  Filename : constant String := "input.txt";

   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Natural);
   use Natural_Vectors;

   F : File_Type;

begin
   Open (F, In_File, Filename);

   Close (F);
end Main;
