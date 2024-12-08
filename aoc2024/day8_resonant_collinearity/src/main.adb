with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

procedure Main is
   -- Constants

   --  Map_Size : constant Natural := 10;
   --  Filename : constant String := "test.txt";

   Map_Size : constant Natural := 10;
   Filename : constant String := "test_3freq.txt";

   --  Map_Size : constant Natural := 12;
   --  Filename : constant String := "test2.txt";

   --  Map_Size : constant Natural := 50;
   --  Filename : constant String := "input.txt";


   -- Types definitions

   type Antenna_Type is record
      Tag : Character;
      X : Integer;
      Y : Integer;
   end record;

   type Antenna_Coords is record
      X : Integer;
      Y : Integer;
   end record;

   type Antenna_Array is array (Positive range <>) of Antenna_Type;
   type Map_Type is array (Positive range <>) of String (1 .. Map_Size);

   type Check_Status_Type is (Nop, Yep, Through);

   ----------------------------
   -- Antenna_Coords_Vectors --
   ----------------------------

   package Antenna_Coords_Vectors is new
     Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Antenna_Coords);
   use Antenna_Coords_Vectors;

   ----------
   -- Hash --
   ----------

   function Hash(Key : Character) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Character'Pos(Key));
   end Hash;

   -------------------------
   -- Antenna_Hashed_Maps --
   -------------------------

   package Antenna_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => Character,
        Element_Type => Antenna_Coords_Vectors.Vector,
        Hash => Hash,
        Equivalent_Keys => "=");
   use Antenna_Hashed_Maps;

   -- Variables

   F : File_Type;

   Area_Map : Map_Type (1 .. Map_Size);
   --  Antennas : Antenna_Array (1 .. Map_Size*Map_Size);
   --  Antennas_Num : Natural := 0;

   Antennas : Antenna_Hashed_Maps.Map;

   Curr_Line_Num : Positive := 1;

   --  Check_Status : Boolean := False;
   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (Map : Map_Type) is
   begin
      for I in Map'Range loop
         Put_Line (Map (I));
      end loop;
   end Print_Map;

   --------------------
   -- Print_Antennas --
   --------------------

   procedure Print_Antennas (A : Antenna_Hashed_Maps.Map) is
   begin
      for C in A.Iterate loop
         for I of A (C) loop
            Put_Line (Key (C) & ": [x=" & I.X'Image & ", y=" & I.Y'Image & "]");
         end loop;
      end loop;
   end Print_Antennas;

   ---------------------------
   -- Find_Antennas_In_Line --
   ---------------------------

   procedure Find_Antennas_In_Line (Line : String; Curr_Line_Num : Positive) is
      Coords : Antenna_Coords := (0, 0);
      Vec : Vector;
      Tag : Character;
   begin
      for I in Line'Range loop
         if Line (I) /= '.' then
            --  Antennas_Num := Antennas_Num + 1;
            Coords.X := I;
            Coords.Y := Curr_Line_Num;
            Tag      := Line (I);

            if Antennas.Contains (Tag) then
               Antennas (Tag).Append (Coords);
            else
               Vec.Clear;
               Vec.Append (Coords);
               Antennas.Include (Tag, Vec);
            end if;
         end if;
      end loop;
   end Find_Antennas_In_Line;

   ----------------------
   -- Check_Map_Coords --
   ----------------------

   function Check_Map_Coords (Tag : Character; X : Integer; Y : Integer )
                              return Check_Status_Type
   is
   begin
      if X in Area_Map'Range and Y in Area_Map'Range then
         if Area_Map (Y)(X) /= Tag then
            if Area_Map (Y)(X) = '.' then
               Area_Map (Y)(X) := '#';
               return Yep;
            elsif Area_Map (Y)(X) = '#' then
               return Through;
            end if;
            return Through;
         end if;
      end if;
      return Nop;
   end Check_Map_Coords;

   --  FIXME Part1 & Part2 pls
   --  holy sh.. man what's wrong with you?

   -----------
   -- Part1 --
   -----------

   procedure Part1 (A : Antenna_Hashed_Maps.Map) is
      Result : Natural := 0;
      X : Integer := 0;
      Y : Integer := 0;

      Vec : Vector;

      Coords : Antenna_Coords := (0, 0);
      Check_Status : Check_Status_Type := Nop;

   begin
      for C in A.Iterate loop
         Vec := A (C);
         for J in Vec.First_Index .. Vec.Last_Index-1 loop
            Coords := Vec(J);

            for I in J+1 .. Vec.Last_Index loop
               X := Coords.X - Vec(I).X;
               Y := Coords.Y - Vec(I).Y;
               Put_Line ("dX =" & X'Image & " dY =" & Y'Image);

               Put_Line ("Current pair: "
                         & Key (C) & ": ["
                         & Coords.X'Image & "," & Coords.Y'Image & "] and ["
                         & Vec(I).X'Image & "," & Vec(I).Y'Image & "]");
               --  -------------------------------------------------------------
               Check_Status := Check_Map_Coords (Key (C),
                                                 Coords.X - X, Coords.Y - Y);
               Print_Map (Area_Map);
               New_Line;

               if Check_Status = Yep then
                  Result := Result + 1;
               end if;
               --  -------------------------------------------------------------
               Check_Status := Check_Map_Coords (Key (C),
                                                 Coords.X + X, Coords.Y + Y);
               Print_Map (Area_Map);
               New_Line;

               if Check_Status = Yep then
                  Result := Result + 1;
               end if;
               --  -------------------------------------------------------------
               Check_Status := Check_Map_Coords (Key (C),
                                                 Vec(I).X - X, Vec(I).Y - Y);
               Print_Map (Area_Map);
               New_Line;

               if Check_Status = Yep then
                  Result := Result + 1;
               end if;
               --  -------------------------------------------------------------
               Check_Status := Check_Map_Coords (Key (C),
                                                 Vec(I).X + X, Vec(I).Y + Y);
               Print_Map (Area_Map);
               New_Line;

               if Check_Status = Yep then
                  Result := Result + 1;
               end if;
               --  -------------------------------------------------------------

            end loop;
         end loop;

      end loop;
      Print_Map (Area_Map);

      Put_Line ("Part1 : " & Result'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------

   procedure Part2 (A : Antenna_Hashed_Maps.Map) is
      Result : Natural := 0;
      X : Integer := 0;
      Y : Integer := 0;

      Vec : Vector;

      Coords : Antenna_Coords := (0, 0);
      New_Coords : Antenna_Coords := (0, 0);
      Check_Status : Check_Status_Type := Yep;

   begin
      for C in A.Iterate loop
         Vec := A (C);
         for J in Vec.First_Index .. Vec.Last_Index-1 loop
            Coords := Vec(J);

            for I in J+1 .. Vec.Last_Index loop
               X := Coords.X - Vec(I).X;
               Y := Coords.Y - Vec(I).Y;
               Put_Line ("dX =" & X'Image & " dY =" & Y'Image);

               Put_Line ("Current pair: "
                         & Key (C) & ": ["
                         & Coords.X'Image & "," & Coords.Y'Image & "] and ["
                         & Vec(I).X'Image & "," & Vec(I).Y'Image & "]");
               --  -------------------------------------------------------------
               New_Coords := Coords;
               while Check_Status /= Nop loop
                  Check_Status := Check_Map_Coords (Key (C),
                                                    New_Coords.X - X, New_Coords.Y - Y);
                  Print_Map (Area_Map);
                  New_Line;

                  if Check_Status = Yep or Check_Status = Through then
                     New_Coords.X := New_Coords.X - X;
                     New_Coords.Y := New_Coords.Y - Y;
                  end if;

                  if Check_Status = Yep then
                     Result := Result + 1;
                  end if;

               end loop;

               Check_Status := Yep;

               --  -------------------------------------------------------------
               New_Coords := Coords;
               while Check_Status /= Nop loop
                  Check_Status := Check_Map_Coords (Key (C),
                                                    New_Coords.X + X, New_Coords.Y + Y);
                  Print_Map (Area_Map);
                  New_Line;

                  if Check_Status = Yep or Check_Status = Through then
                     New_Coords.X := New_Coords.X + X;
                     New_Coords.Y := New_Coords.Y + Y;
                  end if;

                  if Check_Status = Yep then
                     Result := Result + 1;
                  end if;
               end loop;

               Check_Status := Yep;

               --  -------------------------------------------------------------
               New_Coords := Vec(I);
               while Check_Status /= Nop loop
                  Check_Status := Check_Map_Coords (Key (C),
                                                    New_Coords.X - X, New_Coords.Y - Y);
                  Print_Map (Area_Map);
                  New_Line;

                  if Check_Status = Yep or Check_Status = Through then
                     New_Coords.X := New_Coords.X - X;
                     New_Coords.Y := New_Coords.Y - Y;
                  end if;

                  if Check_Status = Yep then
                     Result := Result + 1;
                  end if;
               end loop;

               Check_Status := Yep;

               --  -------------------------------------------------------------
               New_Coords := Vec(I);
               while Check_Status /= Nop loop
                  Check_Status := Check_Map_Coords (Key (C),
                                                    Vec(I).X + X, Vec(I).Y + Y);
                  Print_Map (Area_Map);
                  New_Line;

                  if Check_Status = Yep or Check_Status = Through then
                     New_Coords.X := New_Coords.X + X;
                     New_Coords.Y := New_Coords.Y + Y;
                  end if;

                  if Check_Status = Yep then
                     Result := Result + 1;
                  end if;
               end loop;

               Check_Status := Yep;
               --  -------------------------------------------------------------

            end loop;
         end loop;
         if Vec.Last_Index /= 1 then
            Result := Result + Vec.Last_Index;
         end if;
      end loop;
      Print_Map (Area_Map);

      Put_Line ("Part2 : " & Result'Image);
   end Part2;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Find_Antennas_In_Line (Line, Curr_Line_Num);
         Area_Map (Curr_Line_Num) := Line;
         Curr_Line_Num := Curr_Line_Num + 1;
      end;
   end loop;

   Print_Map (Area_Map);
   Print_Antennas (Antennas);
   Put_Line ("============");

   --  Part1 (Antennas);
   Part2 (Antennas);


end Main;
