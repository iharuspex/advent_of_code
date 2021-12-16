with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;


procedure Main is
   type Grid is array (Natural range <>, Natural range <>) of Natural;

   subtype Tile_Side is Natural range 0 .. 99;


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


   function Increment_Tile (T: Grid) return Grid is
   begin
      return New_Tile : Grid (T'Range(1), T'Range(2))
      do
         for I in T'Range(1) loop
            for J in T'Range(2) loop
               if T (I, J) = 9 then
                  New_Tile (I, J) := 1;
               else
                  New_Tile (I, J) := T (I, J) + 1;
               end if;
            end loop;
         end loop;
      end return;
   end Increment_Tile;


   procedure Set_Tile (M: in out Grid; T: Grid; X, Y: Natural) is
   begin
      for I in T'Range(1) loop
         for J in T'Range(2) loop
            M (Y * T'Length(1) + I, X * T'Length(2) + J) := T (I, J);
         end loop;
      end loop;
   end Set_Tile;


   function Get_Best_Path (Map: in out Grid) return Natural is
   begin
      for I in 1 .. Map'Last(1) loop
         Map (0, I) := Map (0, I) + Map (0, I - 1);
      end loop;

      for I in 1 .. Map'Last(1) loop
         Map (I, 0) := Map (I, 0) + Map (I - 1, 0);
      end loop;

      for I in 1 .. Map'Last(1) loop
         for J in 1 .. Map'Last(2) loop
            Map (I, J) := Map (I, J) + Natural'Min (Map (I - 1, J), Map (I, J - 1));
         end loop;
      end loop;

      return Map (Map'Last(1), Map'Last(2)) - Map (0, 0);
   end Get_Best_Path;


   Input_File : File_Type;
   Tile       : Grid (Tile_Side, Tile_Side);
   Risk_Map   : Grid (0 .. 499,
                      0 .. 499) := (others => (others => 0));

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      for I in Tile'Range(1) loop
         for J in Tile'Range(2) loop
            Get (Input_File, Tile (I, J), 1);

            if End_Of_Line (Input_File) then
               Skip_Line (Input_File);
            end if;
         end loop;
      end loop;
   end loop;

   Print_Grid (Tile);

   -- Заполним всю карту на основе тайла
   declare
      V_Tile : Grid := Tile;
      H_Tile : Grid := V_Tile;
   begin
      for I in 0 .. Risk_Map'Length(1) / Tile'Length(1) - 1 loop
         for J in 0 .. Risk_Map'Length(2) / Tile'Length(2) - 1 loop
            Set_Tile
              (M => Risk_Map,
               T => H_Tile,
               X => J,
               Y => I);
            H_Tile := Increment_Tile (H_Tile);
         end loop;
         V_Tile := Increment_Tile (V_Tile);
         H_Tile := V_Tile;
      end loop;
   end;

   --Print_Grid (Risk_Map);

   New_Line;
   Put_Line (Natural'Image (Get_Best_Path(Tile)));

   Close (Input_File);

end Main;
