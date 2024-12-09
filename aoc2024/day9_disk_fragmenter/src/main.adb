with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; with Ada.Long_Long_Integer_Text_IO;


procedure Main is
   --  Filename : constant String := "test1.txt";

   --  Filename : constant String := "test2.txt";

   Filename : constant String := "input.txt";


   type Cell_Type is (Block, Empty);

   type Cell is record
      Data : Natural;
      Size : Natural;
      T : Cell_Type;
   end record;

   type Cells_Array is array (Natural range <>) of Cell;

   F : File_Type;

   procedure Part1 (Disk : Cells_Array);
   procedure Part2 (Disk : Cells_Array; Files_Num : Natural);

   ----------------
   -- Print_Disk --
   ----------------

   procedure Print_Disk (Disk : Cells_Array) is
   begin
      for I in Disk'Range loop
         if Disk (I).T = Block then
            Put (Disk (I).Data'Image);
         else
            Put (".");
         end if;
      end loop;
      New_Line;
   end Print_Disk;

   --------------
   -- Checksum --
   --------------

   function Checksum (Disk : Cells_Array) return Long_Long_Integer is
      Checksum : Long_Long_Integer := 0;
   begin
      for I in Disk'Range loop
         --  exit when Disk_Map (I).T = Empty;

         Checksum := Checksum + Long_Long_Integer(Disk (I).Data * I);
      end loop;

      return Checksum;
   end Checksum;

   -------------
   -- Process --
   -------------

   procedure Process (Line : String; New_Str_Size : Natural) is
      Disk_Map : Cells_Array (0 .. New_Str_Size - 1) := (others => (0, 0, Empty));

      Write_Idx : Natural := 0;
      Write_End : Natural := 0;

      Number : Natural := 0;
      Block_Size : Natural := 0;

      Cell_Idx : Natural := Disk_Map'Last;

      Result : Long_Long_Integer := 0;
   begin
      -- rewrite str
      for I in Line'Range loop
         Write_End := Write_Idx + Natural'Value (Line (I..I)) - 1;
         Block_Size := Natural'Value (Line (I..I));

         if (I-1) rem 2 = 0 then

            for J in Write_Idx .. Write_End loop
               Disk_Map (J) := (Number, Block_Size, Block);
               --  Print_Disk (Disk_Map);
            end loop;

            Number := Number + 1;

         else

            for J in Write_Idx .. Write_End loop
               Disk_Map (J) := (0, Block_Size, Empty);
               --  Print_Disk (Disk_Map);
            end loop;

         end if;
         Write_Idx := Write_End + 1;
      end loop;

      New_Line;

      --  Part1 (Disk_Map);
      Part2 (Disk_Map, Line'Length);
   end Process;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (Disk : Cells_Array) is
      Cell_Idx : Natural := Disk'Last;

      Disk_Map : Cells_Array := Disk;

      Result : Long_Long_Integer := 0;
   begin
      for I in Disk_Map'Range loop
         if Disk_Map (I).T = Empty then

            -- Skip empty
            while Disk_Map (Cell_Idx).T /= Block loop
               Cell_Idx := Cell_Idx - 1;
            end loop;

            exit when I = Cell_Idx + 1;

            Disk_Map(I) := Disk_Map (Cell_Idx);
            Disk_Map (Cell_Idx) := (0, 0, Empty);
            Cell_Idx := Cell_Idx - 1;
         end if;

      end loop;

      Print_Disk (Disk_Map);

      Result := Checksum (Disk_Map);

      Put_Line ("Part1 : " & Result'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------

   procedure Part2 (Disk : Cells_Array; Files_Num : Natural) is

      type File is record
         First : Natural;
         Last : Natural;

         Data : Natural;

         Size : Natural;
         Cells_Type : Cell_Type;
      end record;

      type Files is array (0 .. Files_Num-1) of File;

      Result : Long_Long_Integer := 0;

      Disk_Files : Files := (others => (0, 0, 0, 0, Empty));
      Disk_Files_Num : Natural := 0;
      Disk_Map : Cells_Array := Disk;
      Disk_Idx : Natural := 0;

      New_Disk_Files : Files := (others => (0, 0, 0, 0, Empty));
      New_Disk_Files_Idx : Natural := 0;

   begin
      Print_Disk (Disk_Map);

      --  Read the file abstraction
      while Disk_Files_Num < Files_Num-1 loop
         Disk_Files (Disk_Files_Num) := (First =>
                                           Disk_Idx,
                                         Last =>
                                           Disk_Idx + Disk_Map (Disk_Idx).Size - 1,
                                         Data =>
                                           Disk_Map (Disk_Idx).Data,
                                         Size =>
                                           Disk_Map (Disk_Idx).Size,
                                         Cells_Type =>
                                           Disk_Map (Disk_Idx).T);

         --  Put_Line (Disk_Files (Disk_Files_Num).First'Image & ".."
         --            & Disk_Files (Disk_Files_Num).Last'Image & " = "
         --            & Disk_Files (Disk_Files_Num).Data'Image & " size= "
         --            & Disk_Files (Disk_Files_Num).Size'Image & " of "
         --            & Disk_Files (Disk_Files_Num).Cells_Type'Image);

         if Disk_Files (Disk_Files_Num).Data = 9999 then
            -- crutch :>>>
            exit;
         end if;

         Disk_Idx := Disk_Idx + Disk_Map (Disk_Idx).Size;
         Disk_Files_Num := Disk_Files_Num + 1;
      end loop;

      for J in reverse Disk_Files'Range loop
         if Disk_Files (J).Cells_Type = Block then

            for I in Disk_Files'Range loop
               exit when Disk_Files (I).First > Disk_Files (J).First;
               if Disk_Files (I).Cells_Type = Empty and Disk_Files (I).Size /= 0 then
                  if Disk_Files (I).Size >= Disk_Files (J).Size then
                     Disk_Files (I).Size := Disk_Files (I).Size - Disk_Files (J).Size;
                     --  Disk_Files (I).Last := Disk_Files (I).First + Disk_Files (J).Size-1;
                     Disk_Map (Disk_Files (I).First..Disk_Files (I).First + Disk_Files (J).Size-1) := (others => (Disk_Files (J).Data, Disk_Files (J).Size, Disk_Files (J).Cells_Type));
                     Disk_Map (Disk_Files (J).First..Disk_Files (J).Last) := (others => (0, 0, Empty));
                     Disk_Files (I).First := Disk_Files (I).First + Disk_Files (J).Size;
                     --  Print_Disk (Disk_Map);
                     exit;
                  end if;
               end if;
            end loop;

         end if;
      end loop;

      Print_Disk (Disk_Map);

      Result := Checksum (Disk_Map);

      Put_Line ("Part2 : " & Result'Image);
   end Part2;

begin
   Open (F, In_File, Filename);

   declare
      -- the input file contains the only one line
      Line : String := Get_Line (F);

      New_Str_Size : Natural := 0;
   begin

      for I in Line'Range loop
         New_Str_Size := New_Str_Size + Natural'Value (Line (I..I));
      end loop;

      Put_Line ("Input string size : " & Line'Length'Image);
      Put_Line ("New string size   : " & New_Str_Size'Image);

      Process (Line, New_Str_Size);
   end;

   Close (F);

end Main;
