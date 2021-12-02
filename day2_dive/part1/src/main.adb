with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   type Commands_Type is (forward, down, up);
   package Commands_IO is new Ada.Text_IO.Enumeration_IO (Commands_Type);

   Input_File     : File_Type;

   Command        : Commands_Type;
   Value          : Integer;

   Horizontal_Pos : Integer := 0;
   Depth          : Integer := 0;

begin
   Open (File => Input_File, Mode => In_File, Name => "input.txt");

   while not End_Of_File (Input_File) loop
      Commands_IO.Get (File => Input_File, Item => Command);
      Get (File => Input_File, Item => Value);

      Commands_IO.Put (Command);
      Put (Item => Value, Width => 1);

      New_Line;
   end loop;

   -- TODO

   Close (Input_File);
end Main;
