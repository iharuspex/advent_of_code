with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;


procedure Main is
   type Grid is array (Natural range 0 .. 99, Natural range 0 .. 99) of Natural;

   procedure Print_Grid (G: Grid) is
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            Put (G (I, J), 1);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Grid;

   Input_File : File_Type;
   Risk_Map   : Grid;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      for I in Risk_Map'Range(1) loop
         for J in Risk_Map'Range(2) loop
            Get (Input_File, Risk_Map (I, J), 1);

            if End_Of_Line (Input_File) then
               Skip_Line (Input_File);
            end if;
         end loop;
      end loop;
   end loop;

   Print_Grid (Risk_Map);

   for I in 1 .. Risk_Map'Last(1) loop
      Risk_Map (0, I) := Risk_Map (0, I) + Risk_Map (0, I - 1);
   end loop;

   for I in 1 .. Risk_Map'Last(1) loop
      Risk_Map (I, 0) := Risk_Map (I, 0) + Risk_Map (I - 1, 0);
   end loop;

   for I in 1 .. Risk_Map'Last(1) loop
      for J in 1 .. Risk_Map'Last(2) loop
         Risk_Map (I, J) := Risk_Map (I, J) + Natural'Min (Risk_Map (I - 1, J), Risk_Map (I, J - 1));
      end loop;
   end loop;

   New_Line;
   Print_Grid (Risk_Map);

   Put_Line (Natural'Image (Risk_Map (Risk_Map'Last(1), Risk_Map'Last(2)) - Risk_Map (0, 0)));

   Close (Input_File);

end Main;
