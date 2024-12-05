with Ada.Text_IO; use Ada.Text_IO;

----------
-- Main --
----------

procedure Main is
   subtype Data_Range is Positive range 1 .. 140;

   type Input_Text is array (Data_Range) of String (Data_Range);
   type Word_Status is ('X', 'M', 'A', 'S');

   F : File_Type;
   In_Text : Input_Text := (others => (others => '.'));
   Str_Num : Natural := 1;

   --  Result : Natural  := 0;

   -----------------
   -- Print_Array --
   -----------------

   procedure Print_Array is
   begin
      for I in In_Text'First..In_Text'Last loop
         Put_Line (In_Text(I));

      end loop;
   end;

   ----------------------
   -- Check_Neighbours --
   ----------------------

   --  ------------------------------------------
   --   X (I-1)(J-1) | X (I-1)(J) | X (I-1)(J+1)
   --  ------------------------------------------
   --   X (I)(J-1)   | X (I)(J)   | X (I)(J+1)
   --  ------------------------------------------
   --   X (I+1)(J-1) | X (i+1)(J) | X (I+1)(J+1)
   --  ------------------------------------------

   function Check_Neighbours
     (Data : Input_Text; Ii, Jj : in Positive; Status : in out Word_Status)
      return Boolean
   is
      --  type Y is range Ii-1 .. Ii+1;
      --  type X is range Jj-1 .. Jj+1;

      Str_Status : String := Status'Image;
      Chr_Status : Character := Str_Status(1);
   begin

      for I in Ii-1 .. Ii+1 loop
         for J in Jj-1 .. Jj+1 loop
            --  if Data (I,J) = Chr_Status
            null;
         end loop;
      end loop;

      if Data (Ii)(Jj) = Chr_Status then
         return True;
      end if;

      return False;
   end;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (Data : in Input_Text) is
      Result : Natural := 0;
   begin
      --  start check
      for I in Data_Range loop
         for J in Data_Range loop
            if Data(I)(J) = 'X' then
               -- check front
               if (J+3 in Data_Range) then
                  if Data(I)(J..J+3) = "XMAS" then
                     Result := Result + 1;
                  end if;
               end if;

               -- check back
               if (J-3 in Data_Range) then
                  if Data(I)(J-3..J) = "SAMX" then
                     Result := Result + 1;
                  end if;
               end if;

               -- check up
               if (I-3 in Data_Range) then
                  if In_Text(I)(J) = 'X' and
                    Data(I-1)(J) = 'M' and
                    Data(I-2)(J) = 'A' and
                    Data(I-3)(J) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

               -- check down
               if (I+3 in Data_Range) then
                  if In_Text(I)(J) = 'X' and
                    Data(I+1)(J) = 'M' and
                    Data(I+2)(J) = 'A' and
                    Data(I+3)(J) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

               -- check diagonals
               -- left up
               if I-3 in Data_Range and J+3 in Data_Range then
                  if Data(I)(J) = 'X' and
                    Data(I-1)(J+1) = 'M' and
                    Data(I-2)(J+2) = 'A' and
                    Data(I-3)(J+3) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

               -- left down
               if I+3 in Data_Range and J+3 in Data_Range then
                  if Data(I)(J) = 'X' and
                    Data(I+1)(J+1) = 'M' and
                    Data(I+2)(J+2) = 'A' and
                    Data(I+3)(J+3) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

               -- right up
               if I-3 in Data_Range and J-3 in Data_Range then
                  if Data(I)(J) = 'X' and
                    Data(I-1)(J-1) = 'M' and
                    Data(I-2)(J-2) = 'A' and
                    Data(I-3)(J-3) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

               -- right down
               if I+3 in Data_Range and J-3 in Data_Range then
                  if Data(I)(J) = 'X' and
                    Data(I+1)(J-1) = 'M' and
                    Data(I+2)(J-2) = 'A' and
                    Data(I+3)(J-3) = 'S'
                  then
                     Result := Result + 1;
                  end if;
               end if;

            end if;
         end loop;
      end loop;

      Put_Line ("Part1 result: " & Result'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------

   procedure Part2 (Data : Input_Text) is
      Result : Natural := 0;
   begin
      for I in Data_Range loop
         for J in Data_Range loop
            if Data(I)(J) = 'A' then
               if I-1 in Data_Range and
                 I+1 in Data_Range and
                 J-1 in Data_Range and
                 J+1 in Data_Range
               then
                  if ((Data(I-1)(J-1) = 'M' and Data(I+1)(J+1) = 'S') or
                        (Data(I-1)(J-1) = 'S' and Data(I+1)(J+1) = 'M')) and
                    ((Data(I-1)(J+1) = 'M' and Data(I+1)(J-1) = 'S') or
                       (Data(I-1)(J+1) = 'S' and Data(I+1)(J-1) = 'M'))
                  then
                     Result := Result + 1;
                  end if;
               end if;
            end if;
         end loop;
      end loop;

      Put_Line ("Part2 result: " & Result'Image);
   end Part2;

begin
   Open (F, In_File, "input.txt");

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         In_Text (Str_Num) := Line;
         Str_Num := Str_Num + 1;
      end;
   end loop;

   Print_Array;

   New_Line;
   Part1 (In_Text);
   Part2 (In_Text);
end Main;
