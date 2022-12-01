with Ada.Text_IO; use Ada.Text_IO;


procedure Main is
   Input_File : File_Type;

   -- type Diagram_Type is array (Integer range <>, Integer range <>) of Character;

   subtype Target_X is Integer range 20 .. 30;
   subtype Target_Y is Integer range -10 .. -5;

   Vx : Integer := 7;
   Vy : Integer := 10;

   Pos_x : Integer := 0;
   Pos_y : Integer := 0;

   -- Diagram : Diagram_Type (-50 .. 50, -50 .. 50) := (others => (others => '.'));

begin
   Open (Input_File, In_File, "input.txt");

   --  Diagram (0, 0) := 'S';
   --
   --  for I in Target_Y'Range loop
   --     for J in Target_X'Range loop
   --        Diagram (I, J) := 'T';
   --     end loop;
   --  end loop;


   loop
      Pos_x := Pos_x + Vx;
      Pos_y := Pos_y + Vy;

      if Vx /= 0 then
         Vx := Vx - 1;
      end if;

      Vy := Vy - 1;

      Put_Line ("X = " & Pos_x'Image & ", Y = " & Pos_y'Image);
      Put_Line ("Vx = " & Vx'Image & ", Vy = " & Vy'Image);
      --  Diagram (Pos_y, Pos_x) := '#';
      New_Line;

      if Pos_x in Target_X'Range and Pos_y in Target_Y'Range then
         New_Line;
         Put_Line ("Result: X = " & Pos_x'Image & ", Y = " & Pos_y'Image);
         exit;
      end if;
   end loop;

   --  for I in reverse Diagram'Range (1) loop
   --     for J in Diagram'Range (2) loop
   --        Put (Diagram (I, J) & " ");
   --     end loop;
   --     New_Line;
   --  end loop;

   Close (Input_File);

end Main;
