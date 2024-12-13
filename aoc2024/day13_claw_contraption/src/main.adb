with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Main is
   --  Filename : constant String := "test.txt";
   Filename : constant String := "input.txt";

   type In_Data_Type is record
      Xa, Xb : Long_Long_Integer;
      Ya, Yb : Long_Long_Integer;
      Prize_X, Prize_Y : Long_Long_Integer;
   end record;


   package In_Data_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => In_Data_Type);
   use In_Data_Vectors;

   ----------------
   -- Print_Data --
   ----------------

   procedure Print_Data (D : In_Data_Type) is
   begin
      Put_Line ("Xa=" & D.Xa'Image & " Ya=" & D.Ya'Image
                & " Xb=" & D.Xb'Image & " Yb=" & D.Yb'Image
                & " Prize_X=" & D.Prize_X'Image & " Prize_Y=" & D.Prize_Y'Image);
   end Print_Data;

   ----------------
   -- Parse_Line --
   ----------------

   function Parse_Line (Line : String; D : in out In_Data_Type) return Boolean is
      type Button_Type is (A, B);

      Colon_Pos : Natural := Index (Line, ":");
      Type_Str : String := Line (Line'First .. Colon_Pos-1);
      Data_Str : String := Line (Colon_Pos+1 .. Line'Last);

      procedure Parse_Button_Str (Btn_Type : Button_Type) is
         Comma_Pos : Natural := Index (Data_Str, ",");

         X_Str : String := Data_Str (Data_Str'First+3 .. Comma_Pos-1);
         Y_Str : String := Data_Str (Comma_Pos+4 .. Data_Str'Last);
      begin
         if Btn_Type = A then
            D.Xa := Long_Long_Integer'Value (X_Str);
            D.Ya := Long_Long_Integer'Value (Y_Str);
         else
            D.Xb := Long_Long_Integer'Value (X_Str);
            D.Yb := Long_Long_Integer'Value (Y_Str);
         end if;
      end Parse_Button_Str;

      procedure Parse_Prize_Str is
         Comma_Pos : Natural := Index (Data_Str, ",");

         X_Str : String := Data_Str (Data_Str'First+3 .. Comma_Pos-1);
         Y_Str : String := Data_Str (Comma_Pos+4 .. Data_Str'Last);
      begin
         D.Prize_X := Long_Long_Integer'Value (X_Str);
         D.Prize_Y := Long_Long_Integer'Value (Y_Str);
      end Parse_Prize_Str;

   begin
      if Type_Str /= "Prize" then
         if Type_Str = "Button A" then
            Parse_Button_Str (A);
         else
            Parse_Button_Str (B);
         end if;
      else
         Parse_Prize_Str;
         return True;
      end if;

      return False;
   end;

   -----------
   -- Solve --
   -----------

   function Solve (D : in out In_Data_Type; P2 : Boolean := False) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
      Det : Long_Long_Float := 0.0;

      A, B : Long_Long_Float;
   begin
      if P2 = True then
         D.Prize_X := D.Prize_X + 10_000_000_000_000;
         D.Prize_Y := D.Prize_Y + 10_000_000_000_000;
      end if;

      Det := Long_Long_Float((D.Xa * D.Yb) - (D.Xb * D.Ya));

      if Det /= 0.0 then
         A := Long_Long_Float(D.Prize_X * D.Yb - D.Prize_Y * D.Xb) / Det;
         B := Long_Long_Float(D.Prize_Y * D.Xa - D.Prize_X * D.Ya) / Det;
      end if;

      if (A = Long_Long_Float'Floor(A)) and (B = Long_Long_Float'Floor(B)) then
         Result := Long_Long_Integer((A * 3.0) + B);
      end if;

      return Result;
   end Solve;

   F : File_Type;
   Data : In_Data_Type := (others => (0));
   Data_Vec : Vector;

   Result_P1 : Long_Long_Integer := 0;
   Result_P2 : Long_Long_Integer := 0;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
         Res : Boolean := False;

         Temp : Natural := 0;
      begin
         if Line /= "" then
            Res := Parse_Line (Line, Data);

            if Res = True then
               Res := False;
               --  Print_Data (Data);
               Data_Vec.Append (Data);
            end if;
         end if;
      end;
   end loop;

   for D of Data_Vec loop
      Result_P1 := Result_P1 + Solve (D);
   end loop;

   for D of Data_Vec loop
      Result_P2 := Result_P2 + Solve (D, True);
   end loop;

   Put_Line ("Part1: " & Result_P1'Image);
   Put_Line ("Part2: " & Result_P2'Image);

end Main;
