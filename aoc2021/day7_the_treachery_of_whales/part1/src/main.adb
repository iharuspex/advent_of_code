with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

procedure Main is
   package Int_Vector is new Ada.Containers.Vectors (Positive, Natural);
   package Sorter is new Int_Vector.Generic_Sorting;
   use Int_Vector;

   Input_File  : File_Type;

   H_Positions : Vector;

   Fuel_raw    : Natural;
   Result      : Natural;


   function Calc (To_Pos: Natural) return Natural
   is
      RV : Natural := 0;
   begin
      for I of H_Positions loop
         RV := RV + abs (To_Pos - I);
      end loop;

      return RV;
   end;


begin
   Open (Input_File, In_File, "input.txt");

   declare
      Line  : constant String := Get_Line (Input_File);
      Comma : constant Character_Set := To_Set (",");
      From  : Natural := 0;
      To    : Natural := 0;
   begin
      loop
         exit when From + 1 > Line'Last;

         Find_Token (Line, Comma, From + 1, Ada.Strings.Outside, From, To);
         H_Positions.Append (Natural'Value (Line (From .. To)));
         From := To;
      end loop;
   end;

   Sorter.Sort (H_Positions);

   --  for I of H_Positions loop
   --     Put_Line (I'Image);
   --  end loop;
   --  New_Line;

   Result := Calc (0);
   for I in 0 .. H_Positions.Last_Element loop
      Fuel_raw := Calc (I);

      if Result > Fuel_raw then
         Result := Fuel_raw;
      end if;
   end loop;

   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
