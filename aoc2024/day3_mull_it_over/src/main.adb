with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;

procedure Main is
   F      : File_Type;
   Result : Natural := 0;

   --------------------
   -- Search_Pattern --
   --------------------

   function Search_Pattern
     (Pattern : String; Search_In : String) return Match_Array
   is
      Re      : constant Pattern_Matcher := Compile (Pattern);
      Matches : Match_Array (0 .. Paren_Count (Re));

   begin
      Match (Re, Search_In, Matches);

      return Matches;
   end Search_Pattern;

   -------------------
   -- Print_Matches --
   -------------------

   procedure Print_Matches (S : String; M : Match_Array) is
   begin
      for I in 0 .. M'Last loop
         Put_Line (S (M (I).First .. M (I).Last));
      end loop;
      Put_Line ("===");
   end Print_Matches;

   ---------
   -- Mul --
   ---------

   function Mul (S : String; M : Match_Array) return Natural is
   begin
      return
        (Natural'Value (S (M (1).First .. M (1).Last)) *
         Natural'Value (S (M (2).First .. M (2).Last)));
   end Mul;

   -----------
   -- Part1 --
   -----------

   function Part1 (F : File_Type) return Natural is
      Ret : Natural := 0;

      Re_Mul  : constant Pattern_Matcher :=
        Compile ("mul\(([0-9]*?)\,([0-9]*?)\)");
      Matches : Match_Array (0 .. Paren_Count (Re_Mul));
   begin
      while not End_Of_File (F) loop

         declare
            Line : String := Get_Line (F);
         begin
            loop
               Match
                 (Re_Mul, Line (Matches (0).Last + 1 .. Line'Last), Matches);
               exit when Matches (0) = No_Match;

               Ret := Ret + Mul (Line, Matches);
            end loop;
         end;

      end loop;

      return Ret;
   end Part1;

   -----------
   -- Part2 --
   -----------

   function Part2 (F : File_Type) return Natural is
      Mul_Allowed : Boolean := True;

      Ret : Natural := 0;

      Re_Mul  : constant Pattern_Matcher :=
        Compile ("mul\(([0-9]*?)\,([0-9]*?)\)");
      Re_Do   : constant Pattern_Matcher := Compile ("do\(\)");
      Re_Dont : constant Pattern_Matcher := Compile ("don\'t\(\)");

      Matches : Match_Array (0 .. Paren_Count (Re_Mul));

   begin

      while not End_Of_File (F) loop

         declare
            Line : String   := Get_Line (F);
            L    : Positive := Line'First;
            R    : Positive := Line'Last;

            Dont_Pos : Match_Location := (First => L, Last => R);
         begin

            loop
               if Mul_Allowed = True then
                  --  find don't() pattern first
                  Match (Re_Dont, Line (L .. R), Matches);
                  if Matches (0) /= No_Match then
                     Dont_Pos := Matches (0);
                  else
                     Dont_Pos.First := R;
                  end if;

                  --  find mul(x,y) pattern
                  Match (Re_Mul, Line (L .. R), Matches);
                  -- pattern not found in this line, go next
                  exit when Matches (0) = No_Match;

                  --  check if mul(x,y) pattern before don't() pattern
                  if Matches (0).First < Dont_Pos.First then
                     --  calc all mul from current pos to don't() first
                     Ret := Ret + Mul (Line, Matches);
                     L   := Matches (0).Last + 1;
                     null;
                  else
                     Mul_Allowed := False;
                  end if;

               else
                  -- find next do() pattern
                  Match (Re_Do, Line (L .. R), Matches);
                  exit when Matches (0) = No_Match;

                  Mul_Allowed := True;
                  L           := Matches (0).Last + 1;
               end if;

            end loop;
         end;
      end loop;

      return Ret;

   end Part2;

begin
   Open (File => F, Mode => In_File, Name => "input.txt");

   Result := Part1 (F);
   Put_Line ("Result: " & Result'Image);

   Reset (F, In_File);

   Result := Part2 (F);
   Put_Line ("Result: " & Result'Image);

end Main;
