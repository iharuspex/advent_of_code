with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;


procedure Main is

   subtype Columns is Positive range 1 .. 100;
   subtype Rows is Positive range 1 .. 100;

   type Map is array (Rows, Columns) of Natural;

   type Map_Mask is array (Rows, Columns) of Boolean;

   package M_Vec is new Ada.Containers.Vectors (Positive, Natural);
   use M_Vec;

   package Vector_Sorter is new M_Vec.Generic_Sorting;

   Input_File     : File_Type;
   Cave_Map       : Map;
   Marked_Map     : Map_Mask := (others => (others => False));
   Minimum_Values : Vector;

   Result         : Natural := 1;

   procedure Print_Map (M: Map) is
   begin
      for I in Cave_Map'Range (1) loop
         for J in Cave_Map'Range (2) loop
            Put (Cave_Map (I, J), 1);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   procedure Print_Val (V: Vector) is
   begin
      New_Line;
      for I of Minimum_Values loop
         Put (I'Image & " ");
      end loop;
      New_Line;
   end Print_Val;


   function Calc_Basin_Size (M: in out Map; X, Y: Natural) return Natural is
   begin
      if M (X, Y) = 9 then
         return 0;
      end if;

      if X < 1 or Y < 1 or X > M'Last(2) or Y > M'Last(1) then
         return 0;
      end if;

      if Marked_Map (X, Y) then
         return 0;
      else
         Marked_Map (X, Y) := True;

         return 1 + Calc_Basin_Size (M, X - 1, Y) +
           Calc_Basin_Size (M, X + 1, Y) +
           Calc_Basin_Size (M, X, Y + 1) +
           Calc_Basin_Size (M, X, Y - 1);
      end if;
   end;


begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      for I in Cave_Map'Range (1) loop
         for J in Cave_Map'Range (2) loop
            Get (Input_File, Cave_Map (I, J), 1);

            if End_Of_Line (Input_File) then
               Skip_Line (Input_File);
            end if;

         end loop;
      end loop;

   end loop;

   Print_Map (Cave_Map);

   for I in Cave_Map'Range (1) loop
      for J in Cave_Map'Range (2) loop

         if I = Cave_Map'First (1) then
            -- Left upper corner
            if J = Cave_Map'First (2) then
               if Cave_Map (I, J) < Cave_Map (I, J + 1) and
                 Cave_Map (I, J) < Cave_Map (I + 1, J)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;

               -- Right upper corner
            elsif J = Cave_Map'Last (2) then
               if Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I + 1, J)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;

               -- Others
            else
               if Cave_Map (I, J) < Cave_Map (I + 1, J) and
                 Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I, J + 1)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;
            end if;

         elsif I = Cave_Map'Last (1) then
            -- Left bottom corner
            if J = Cave_Map'First (2) then
               if Cave_Map (I, J) < Cave_Map (I, J + 1) and
                 Cave_Map (I, J) < Cave_Map (I - 1, J)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;

               -- Right bottom corner
            elsif J = Cave_Map'Last (2) then
               if Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I - 1, J)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;

               -- Others
            else
               if Cave_Map (I, J) < Cave_Map (I - 1, J) and
                 Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I, J + 1)
               then
                  Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
               end if;
            end if;

            -- Left side
         elsif J = Cave_Map'First (2) then
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J + 1)
            then
               Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
            end if;

            -- Right side
         elsif J = Cave_Map'Last (2) then
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J - 1)
            then
               Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
            end if;

            -- Other cells
         else
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J - 1) and
              Cave_Map (I, J) < Cave_Map (I, J + 1)
            then
               Minimum_Values.Append (Calc_Basin_Size (Cave_Map, I, J));
            end if;

         end if;

      end loop;
   end loop;

   Vector_Sorter.Sort (Minimum_Values);

   New_Line;
   for I of Minimum_Values loop
      Put (I'Image & " ");
   end loop;
   New_Line;

   for I in Minimum_Values.Last_Index - 2 .. Minimum_Values.Last_Index loop
      Result := Result * Minimum_Values (I);
   end loop;

   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
