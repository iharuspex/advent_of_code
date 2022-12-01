with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


procedure Main is
   Input_File : File_Type;
   Result    : Integer := 0;
   New_Value : Integer := 0;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      declare
         Line : String := Get_Line (Input_File);
      begin
         if Line = "" then
            if Result < New_Value then
                Result := New_Value;
            end if;
            New_Value := 0;
         else
            New_Value := New_Value + Integer'Value(Line);
         end if;
      end;
   end loop;

   Put_Line (Result'Image);

   Close (Input_File);

end Main;
