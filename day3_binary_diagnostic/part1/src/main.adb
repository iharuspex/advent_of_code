with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Direct_IO;

procedure Main is
   type Bit_Field is array (Natural range <>) of Boolean with Pack;

   subtype Val_12bit is Integer range 2#0# .. 2#1111_1111_1111#;

   Input_File  : File_Type;

   Value       : Val_12bit := 0;

   B           : Bit_Field (0 .. Value'Size - 1)
                     with Address => Value'Address, Import, Volatile;

begin
   Open (Input_File, In_File, "input2.txt");

   while not End_Of_File (Input_File) loop
      Get (Input_File, Value);

      Put (Item => Value, Base => 2);
      New_Line;

   end loop;

end Main;
