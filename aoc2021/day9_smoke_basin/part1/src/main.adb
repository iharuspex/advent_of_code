with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;


procedure Main is
   type Map is array (Positive range 1 .. 100, Positive range 1 .. 100) of Natural;

   package M_Vec is new Ada.Containers.Vectors (Positive, Natural);
   use M_Vec;

   Input_File     : File_Type;
   Cave_Map       : Map;
   Minimum_Values : Vector;

   Result         : Natural := 0;

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
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;

               -- Right upper corner
            elsif J = Cave_Map'Last (2) then
               if Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I + 1, J)
               then
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;

               -- Others
            else
               if Cave_Map (I, J) < Cave_Map (I + 1, J) and
                 Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I, J + 1)
               then
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;
            end if;

         elsif I = Cave_Map'Last (1) then
            -- Left bottom corner
            if J = Cave_Map'First (2) then
               if Cave_Map (I, J) < Cave_Map (I, J + 1) and
                 Cave_Map (I, J) < Cave_Map (I - 1, J)
               then
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;

               -- Right bottom corner
            elsif J = Cave_Map'Last (2) then
               if Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I - 1, J)
               then
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;

               -- Others
            else
               if Cave_Map (I, J) < Cave_Map (I - 1, J) and
                 Cave_Map (I, J) < Cave_Map (I, J - 1) and
                 Cave_Map (I, J) < Cave_Map (I, J + 1)
               then
                  Minimum_Values.Append (Cave_Map (I, J));
               end if;
            end if;

            -- Left side
         elsif J = Cave_Map'First (2) then
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J + 1)
            then
               Minimum_Values.Append (Cave_Map (I, J));
            end if;

            -- Right side
         elsif J = Cave_Map'Last (2) then
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J - 1)
            then
               Minimum_Values.Append (Cave_Map (I, J));
            end if;

            -- Other cells
         else
            if Cave_Map (I, J) < Cave_Map (I - 1, J) and
              Cave_Map (I, J) < Cave_Map (I + 1, J) and
              Cave_Map (I, J) < Cave_Map (I, J - 1) and
              Cave_Map (I, J) < Cave_Map (I, J + 1)
            then
               Minimum_Values.Append (Cave_Map (I, J));
            end if;

         end if;

      end loop;
   end loop;

   New_Line;
   for I of Minimum_Values loop
      Put (I'Image & " ");
      Result := Result + I + 1;
   end loop;

   New_Line;
   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
