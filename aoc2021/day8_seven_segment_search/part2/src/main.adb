with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Vectors;

procedure Main is
   -- Type definition
   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 7);
   use B_Str;

   procedure String_Sort is
     new Ada.Containers.Generic_Array_Sort (Positive, Character, String);

   type SSI_Numbers is array (Natural range 0 .. 9) of Bounded_String;

   type Raw_Pattern is record
      Code : Bounded_String;
      Mark : Boolean := False;
   end record;

   package Raw_Pattern_Vec is new
     Ada.Containers.Vectors (Natural, Raw_Pattern);

   package B_Str_Vec is new
     Ada.Containers.Vectors (Natural, Bounded_String);


   -- Variables
   Input_File    : File_Type;
   Result        : Natural := 0;


   -- Subprograms
   function Str_Sort (Str : in Bounded_String) return Bounded_String is
      S : String := To_String (Str);
   begin
      String_Sort (S);

      return To_Bounded_String (S);
   end;

   -- Find Substr segments in Str
   function Diff_Segments (Str, Substr : in Bounded_String) return Natural is
      Cnt : Natural := 0;
   begin
      for I in 1 .. Length (Substr) loop
         for J in 1 .. Length (Str) loop
            if Element (Str, J) = Element (Substr, I) then
               Cnt := Cnt + 1;
               exit;
            end if;
         end loop;
      end loop;

      return Length (Substr) - Cnt;
   end Diff_Segments;


   function Decode_Output (Out_Val: B_Str_Vec.Vector; Patterns: SSI_Numbers) return Natural is
      Result_Str : Bounded_String;
   begin
      for I of Out_Val loop
         for J in 0 .. Patterns'Last loop
            if I = Patterns (J) then
               Result_Str := Result_Str & Trim(To_Bounded_String(J'Image), Ada.Strings.Left);
               exit;
            end if;
         end loop;
      end loop;
      Put_Line (To_String(Result_Str));
      return Natural'Value (To_String (Result_Str));
   end Decode_Output;


   function "-" (left, right : in Bounded_String) return Bounded_String is
      ret : Bounded_String;
      fl : Boolean;
   begin
      for I in 1 .. Length (left) loop
         for J in 1 .. Length (right) loop
            if Element (left, I) = Element (right, J) then
               fl := True;
               exit;
            end if;
         end loop;

         if fl /= True then
            Append (ret, Element (left, I));
         end if;

         fl := False;
      end loop;

      return ret;
   end "-";


   function "+" (left, right : in Bounded_String) return Bounded_String is
      ret : Bounded_String;
   begin
      ret := left & right;
      ret := Str_Sort (ret);

      return ret;
   end "+";


   procedure Display_String_Info (S : Bounded_String) is
   begin
      Put_Line ("String: " & To_String (S));
      Put_Line ("String Length: " & Integer'Image (Length (S)));
      Put_Line ("Max.   Length: " & Integer'Image (Max_Length));
   end Display_String_Info;
   -----------------------------------------------------------------

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      declare
         Line : String := Get_Line (Input_File);
         Pattern_Values_Str : String := Head (Line, Index (Line, " |", Line'First) - 1);
         Out_Values_Str : String := Tail (Line, Line'Last - Index (Line, "|", Line'Last, Ada.Strings.Backward) - 1);
         Whitespace : constant Character_Set := To_Set (' ');
         From : Natural := 0;
         To   : Natural := 0;

         D_Patterns    : SSI_Numbers := (others => To_Bounded_String(""));
         Output_Values : B_Str_Vec.Vector;
         Raw_Patterns  : Raw_Pattern_Vec.Vector;

         function To_Raw_Pattern (C: Bounded_String) return Raw_Pattern is
            Ret : Raw_Pattern;
         begin
            Ret.Code := C;
            Ret.Mark := False;

            return Ret;
         end To_Raw_Pattern;

      begin
         Put_Line ("Patterns: " & Pattern_Values_Str);
         Put_Line ("Output: " & Out_Values_Str);
         loop
            exit when From + 1 > Pattern_Values_Str'Last;
            Find_Token (Pattern_Values_Str, Whitespace, From + 1, Ada.Strings.Outside, From, To);

            Raw_Patterns.Append (To_Raw_Pattern (Str_Sort (To_Bounded_String (Pattern_Values_Str (From .. To)))));
            From := To;
         end loop;

         From := 0;
         To := 0;

         loop
            exit when From + 1 > Out_Values_Str'Last;
            Find_Token (Out_Values_Str, Whitespace, From + 1, Ada.Strings.Outside, From, To);

            Output_Values.Append (Str_Sort (To_Bounded_String (Out_Values_Str (From .. To))));
            From := To;
         end loop;

         -- Put known values on their places
         for I of Raw_Patterns loop
            case Length (I.Code) is
            when 2 =>
               D_Patterns (1) := I.Code;
               I.Mark := True;
            when 3 =>
               D_Patterns (7) := I.Code;
               I.Mark := True;
            when 4 =>
               D_Patterns (4) := I.Code;
               I.Mark := True;
            when 7 =>
               D_Patterns (8) := I.Code;
               I.Mark := True;
            when others =>
               null;
            end case;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Calc needed patterns
         -- Find "6"
         for I of Raw_Patterns loop
            if Length (I.Code) = 6 then
               if Diff_Segments (I.Code, D_Patterns(8) - D_Patterns (1)) = 0 then
                  D_Patterns (6) := I.Code;
                  I.Mark := True;
               end if;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Find "5"
         for I of Raw_Patterns loop
            if Length (I.Code) = 5 then
               if Diff_Segments (I.Code, D_Patterns(6) - I.Code) = 1 then
                  D_Patterns (5) := I.Code;
                  I.Mark := True;
               end if;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Find "9"
         D_Patterns (9) := D_Patterns (8) - (D_Patterns (6) - D_Patterns (5));
         for I of Raw_Patterns loop
            if D_Patterns(9) = I.Code then
               I.Mark := True;
               exit;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Find "0"
         for I of Raw_Patterns loop
            if Length (I.Code) = 6 and I.Mark /= True then
               D_Patterns (0) := I.Code;
               I.Mark := True;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Find "3"
         D_Patterns (3) := D_Patterns (8) - D_Patterns (0) + D_Patterns (7);

         for I of Raw_Patterns loop
            if Diff_Segments (I.Code, D_Patterns(3)) = 0 and I.Mark /= True then
               D_Patterns (3) := I.Code;
               I.Mark := True;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");

         -- Find "2" (last unmarked)
         for I of Raw_Patterns loop
            if I.Mark /= True then
               D_Patterns (2) := I.Code;
            end if;
         end loop;

         --  for I in D_Patterns'First .. D_Patterns'Last loop
         --     Put_Line (I'Image & ":" & To_String (D_Patterns(I)));
         --  end loop;
         --  Put_Line ("===================================================");


         -- NOW WE DECODE THE OUTPUT VALUES, FINALLY! =)
         Result := Result + Decode_Output (Output_Values, D_Patterns);
      end;
   end loop;

   Put_Line ("===================================================");
   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
