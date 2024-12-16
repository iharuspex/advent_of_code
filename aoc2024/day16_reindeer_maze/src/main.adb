with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Filename : constant String := "test_mini.txt";
   Map_Size : constant Natural := 7;


   F : File_Type;
begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      null;
   end loop;

   Close (F);
end Main;
