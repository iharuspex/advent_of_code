with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;


procedure Main is
   type Octopus is record
      Energy : Natural;
      Lock   : Boolean := False;
   end record;

   type Grid is
     array (Positive range 1 .. 10, Positive range 1 .. 10) of Octopus;


   Input_File   : File_Type;
   Octopus_Grid : Grid;
   ZC           : Natural := 0;
   Result       : Natural := 0;


   procedure Print_Grid (G: Grid) is
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            Put (G (I, J).Energy, 1);
         end loop;
         New_Line;
      end loop;
   end Print_Grid;

   procedure Energy_Up (G: in out Grid; X, Y: Positive);


   procedure Flash (G: in out Grid; X, Y: Positive) is
   begin
      G (X, Y).Energy := 0;
      G (X, Y).Lock := True;

      Result := Result + 1;

      if X - 1 in G'Range(1) and Y - 1 in G'Range(2) then
         --G (X - 1, Y - 1).Energy := 0;
         Energy_Up (G, X - 1, Y - 1);
      end if;

      if X + 1 in G'Range(1) and Y + 1 in G'Range(2) then
         --G (X + 1, Y + 1).Energy := 0;
         Energy_Up (G, X + 1, Y + 1);
      end if;

      if X + 1 in G'Range(1) and Y - 1 in G'Range(2) then
         --G (X + 1, Y - 1).Energy := 0;
         Energy_Up (G, X + 1, Y - 1);
      end if;

      if X - 1 in G'Range(1) and Y + 1 in G'Range(2) then
         --G (X - 1, Y + 1).Energy := 0;
         Energy_Up (G, X - 1, Y + 1);
      end if;

      if Y + 1 in G'Range(2) then
         --G (X, Y + 1).Energy := 0;
         Energy_Up (G, X, Y + 1);
      end if;

      if Y - 1 in G'Range(2) then
         --G (X, Y - 1).Energy := 0;
         Energy_Up (G, X, Y - 1);
      end if;

      if X - 1 in G'Range(1) then
         --G (X - 1, Y).Energy := 0;
         Energy_Up (G, X - 1, Y);
      end if;

      if X + 1 in G'Range(1) then
         --G (X + 1, Y).Energy := 0;
         Energy_Up (G, X + 1, Y);
      end if;
   end Flash;

   procedure Energy_Up (G: in out Grid; X, Y: Positive) is
   begin
      if G (X, Y).Energy + 1 > 9 then
         Flash (G, X, Y);
      else
         -- possibly useless condition
         if not G (X, Y).Lock then
            G (X, Y).Energy := G (X, Y).Energy + 1;
         end if;
      end if;
   end Energy_Up;

   procedure Step (G: in out Grid) is
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            Energy_Up (G, I, J);
         end loop;
      end loop;

      -- Step is over, then unlock all
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            G (I, J).Lock := False;
         end loop;
      end loop;
   end Step;


begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      for I in Octopus_Grid'Range(1) loop
         for J in Octopus_Grid'Range(2) loop
            Get (Input_File, Octopus_Grid (I, J).Energy, 1);

            if End_Of_Line (Input_File) then
               Skip_Line (Input_File);
            end if;

         end loop;
      end loop;

   end loop;



   for I in Positive'Range loop
      Step (Octopus_Grid);

      for I in Octopus_Grid'Range (1) loop
         for J in Octopus_Grid'Range (2) loop
            if Octopus_Grid (I, J).Energy = 0 then
               ZC := ZC + 1;
            end if;
         end loop;
      end loop;

      if ZC = 100 then
         Print_Grid (Octopus_Grid);
         New_Line;
         Put_Line ("Result: " & I'Image);
         exit;
      else
         ZC := 0;
      end if;
   end loop;



   Close (Input_File);

end Main;
