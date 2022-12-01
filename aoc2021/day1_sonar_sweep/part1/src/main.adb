with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   Input_File : File_Type;

   Prev_Value : Integer := -1;
   Value      : Integer := 0;
   Counter    : Integer := 0;

begin
   Open(File => Input_File, Mode => In_File, Name => "input.txt");

   while not End_Of_File (Input_File) loop
      Get (File => Input_File, Item => Value);

      if Prev_Value /= -1 then
         if Value > Prev_Value then
            Counter := Counter + 1;
         end if;
      end if;

      Prev_Value := Value;
   end loop;

   Put_Line("Answer: " & Counter'Image);

   Close (Input_File);

end Main;
