with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Main is
   type Point is record
      X : Natural;
      Y : Natural;
   end record;

   type Line is record
      A : Point;
      B : Point;
   end record;

   package Int_Vector is new Ada.Containers.Vectors (Natural, Line);
   use Int_Vector;

   type Field is array (Natural range <>, Natural range <>) of Natural;


   Input_File : File_Type;
   Lines      : Vector;
   Diagram    : Field (0 .. 999, 0 .. 999) := (others => (others => 0));

   procedure Print_Line (L: in Line) is
   begin
      Put (L.A.X, 1);
      Put (",");
      Put (L.A.Y, 1);
      Put (" to ");
      Put (L.B.X, 1);
      Put (",");
      Put (L.B.Y, 1);
      New_Line;
   end Print_Line;


   procedure Print_Diagram (Dia: in Field) is
   begin
      for I in Dia'Range(1) loop
         for J in Dia'Range(2) loop
            if Dia (J, I) = 0 then
               Put (". ");
            else
               Put (Dia (J, I), 1);
               Put (" ");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Diagram;


   function Decode_Point (Str: String) return Point
   is
      P : Point;
   begin
      P.X := Natural'Value (Head (Str, Index (Str, ",", Str'First) - 1));
      P.Y := Natural'Value (Tail (Str, Str'Last - Index (Str, ",", Str'First)));

      return P;
   end Decode_Point;


   function Decode_Line (Str: String) return Line
   is
      L : Line;
   begin
      L.A := Decode_Point (Head (Str, Index (Str, " ", Str'First) - 1));
      L.B := Decode_Point (Tail (Str, Str'Last - Index (Str, " ", Str'Last, Backward)));

      return L;
   end Decode_Line;


   procedure Get_Result (Dia: in Field) is
      Result : Natural := 0;
   begin
      for I in Dia'Range(1) loop
         for J in Dia'Range(2) loop
            if Dia (J, I) >= 2 then
               Result := Result + 1;
            end if;
         end loop;
      end loop;

      Put_Line ("Result: " & Result'Image);
   end Get_Result;


   procedure Swap_Points (P0: in out Point; P1: in out Point) is
      Tmp : Point;
   begin
      Tmp := P0;
      P0 := P1;
      P1 := Tmp;
   end Swap_Points;


   procedure Draw_Line (Dia: in out Field; L: in Line) is
      Line_Range : Natural;
      DV : Natural;
      DL : Line := L;
      XI, YI : Integer := 0;

   begin
      if L.A.X = L.B.X then
         -- vertical line
         Line_Range := abs (L.A.Y - L.B.Y);
         DV := Natural'Min (L.A.Y, L.B.Y);

         for I in DV .. Line_Range + DV loop
            Dia (L.A.X, I) := Dia (L.A.X, I) + 1;
         end loop;

      elsif L.A.Y = L.B.Y then
         -- horizontal line
         Line_Range := abs (L.A.X - L.B.X);
         DV := Natural'Min (L.A.X, L.B.X);

         for I in DV .. Line_Range + DV loop
            Dia (I, L.A.Y) := Dia (I, L.A.Y) + 1;
         end loop;
      else
         -- diagonal line at 45 degrees
         Line_Range := abs (DL.A.X - DL.B.X);

         if DL.B.Y < DL.A.Y then
            Swap_Points (DL.A, DL.B);
         end if;

         XI := DL.A.X;
         YI := DL.A.Y;

         if DL.A.X < DL.B.X then
            for I in 0 .. Line_Range loop
               Dia (XI + I, YI + I) := Dia (XI + I, YI + I) + 1;
            end loop;
         else
            for I in reverse 0 .. Line_Range loop
               Dia (XI - I, YI + I) := Dia (XI - I, YI + I) + 1;
            end loop;
         end if;

      end if;
   end Draw_Line;

   --TL : Line;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      declare
         Str : String := Get_Line (Input_File);
         L : Line;
      begin
         L := Decode_Line (Str);
         Lines.Append (L);
      end;
   end loop;

   for I of Lines loop
      Print_Line (I);
      Draw_Line (Diagram, I);
   end loop;

   --  TL := Decode_Line ("3,3 -> 5,1");
   --  Draw_Line (Diagram, TL);
   --
   Print_Diagram (Diagram);
   Get_Result (Diagram);

   Close (Input_File);

end Main;
