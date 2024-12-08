with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Main is
   -- Constants

   Map_Size : constant Natural := 10;
   Filename : constant String := "test.txt";
   --
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

   type Antenna_Array is array (Positive range <>) of Antenna_Type;
   type Map_Type is array (Positive range <>) of String (1 .. Map_Size);

   package Antenna_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String,
        Element_Type => Natural,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");

   -- Variables

   F : File_Type;

   Map : Map_Type (1 .. Map_Size);
   Antennas : Antenna_Array (1 .. Map_Size*Map_Size);
   Antennas_Num : Natural := 0;

   Curr_Line_Num : Positive := 1;

   Check_Status : Boolean := False;
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

   procedure Print_Antennas (A : Antenna_Array) is
   begin
      for I in A'First .. Antennas_Num loop
         Put_Line (A(I).Tag & ": [x=" & A(I).X'Image & ", y=" & A(I).Y'Image & "]");
      end loop;
   end Print_Antennas;

   ---------------------------
   -- Find_Antennas_In_Line --
   ---------------------------

   procedure Find_Antennas_In_Line (Line : String; Curr_Line_Num : Positive) is
   begin
      for I in Line'Range loop
         if Line (I) /= '.' then
            Antennas_Num := Antennas_Num + 1;
            Antennas (Antennas_Num) := (Tag => Line (I),
                                        X   => I,
                                        Y   => Curr_Line_Num);
         end if;
      end loop;
   end Find_Antennas_In_Line;

   ----------------------
   -- Check_Map_Coords --
   ----------------------

   function Check_Map_Coords (Tag : Character; Y : Integer; X : Integer )
                              return Boolean
   is
   begin
      if X in Map'Range and Y in Map'Range then
         if Map (Y)(X) /= Tag and Map (Y)(X) /= 'X' then
            if Map (Y)(X) = '.' then
               Map (Y)(X) := 'X';
            end if;
            return True;
         end if;
      end if;
      return False;
   end Check_Map_Coords;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Find_Antennas_In_Line (Line, Curr_Line_Num);
         Map (Curr_Line_Num) := Line;
         Curr_Line_Num := Curr_Line_Num + 1;
      end;
   end loop;

   Print_Map (Map);
   Print_Antennas (Antennas);

   Check_Status := Check_Map_Coords ('a', 5, 9);
   Check_Status := Check_Map_Coords ('a', 3, 1);
   Check_Status := Check_Map_Coords ('a', 6, 13);
   Check_Status := Check_Map_Coords ('a', 2, 4);
   Check_Status := Check_Map_Coords ('a', 8, 7);
   Check_Status := Check_Map_Coords ('a', 7, 3);

   Print_Map (Map);


end Main;
