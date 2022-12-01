with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   type Measurements is array (Positive range <>) of Integer;

   Input_File    : File_Type;

   Prev_Value    : Integer := -1;
   Value         : Integer := 0;
   Counter       : Integer := 0;

   Data_Set      : Measurements (1 .. 2001) := (others => 0);
   Data_Set_Size : Integer := 0;

begin
   Open(File => Input_File, Mode => In_File, Name => "input.txt");

   for I in Data_Set'Range loop
      exit when I = Data_Set'Last or End_Of_File (Input_File);

      Get (File => Input_File,
           Item => Data_Set(I));
      Data_Set_Size := Data_Set_Size + 1;

      --  Put (Data_Set(I));
      --  New_Line;
   end loop;


   for I in 1 .. Data_Set_Size loop
      exit when I + 2 > Data_Set_Size;

      Value := Data_Set(I) + Data_Set(I + 1) + Data_Set(I + 2);

      Put ("Sum #" & I'Image & " = " & Value'Image & " ");

      if Prev_Value /= -1 then
         if Value > Prev_Value then
            Counter := Counter + 1;
            Put ("(increased)");
         end if;
      end if;

      New_Line;

      Prev_Value := Value;
   end loop;

   Put_Line("Answer: " & Counter'Image);

   Close (Input_File);

end Main;
