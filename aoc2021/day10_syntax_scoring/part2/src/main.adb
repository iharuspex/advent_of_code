with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with Stack;


procedure Main is
   -- Type definitions
   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 100);
   use B_Str;

   package N_Vec is new
     Ada.Containers.Vectors (Positive, Long_Integer);
   use N_Vec;

   package Vector_Sorter is new N_Vec.Generic_Sorting;


   -- Variables
   Input_File : File_Type;
   Result     : Natural := 0;

   Scores_Vec : Vector;


   -- Subprograms
   function Wrong_Pair (Left, Right : Character) return Boolean is
   begin
      if Left = '(' and Right /= ')' then
         return True;
      elsif Left = '[' and Right /= ']' then
         return True;
      elsif Left = '{' and Right /= '}' then
         return True;
      elsif Left = '<' and Right /= '>' then
         return True;
      else
         return False;
      end if;
   end Wrong_Pair;


   function Invert_Bracket (B: in Character) return Character is
   begin
      case B is
         when '(' =>
            return ')';
         when '[' =>
            return ']';
         when '{' =>
            return '}';
         when '<' =>
            return '>';
         when others =>
            return '!';
      end case;
   end Invert_Bracket;


   function Get_Bracket_Points (B: in Character) return Long_Integer is
   begin
      case B is
         when ')' =>
            return 1;
         when ']' =>
            return 2;
         when '}' =>
            return 3;
         when '>' =>
            return 4;
         when others =>
            return 0;
      end case;
   end Get_Bracket_Points;


   function Check_String (Str : String) return Long_Integer is
      Ch  : Character;
      Incomplete_Parentheses : Bounded_String;
      Score : Long_Integer := 0;
   begin
      Stack.Clear;

      for I in Str'Range loop
         if Str (I) = '(' or Str (I) = '[' or Str (I) = '{' or Str (I) = '<' then
            Stack.Push (Str (I));
         else
            Ch := Stack.Pop;

            if Wrong_Pair (Ch, Str (I)) then
               return 0;
            end if;
         end if;
      end loop;

      -- If input line is incomplete
      if Stack.Size /= 0 then
         for I in 1 .. Stack.Size loop
            Append (Incomplete_Parentheses, Invert_Bracket (Stack.Pop));
         end loop;

         for I in 1 .. Length (Incomplete_Parentheses) loop
            Score := 5 * Score + Get_Bracket_Points (Element (Incomplete_Parentheses, I));
         end loop;

         Put_Line (To_String (Incomplete_Parentheses) & " - " & Score'Image);

         return Score;
      end if;

      return 0;

   end Check_String;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      declare
         Line : String := Get_Line (Input_File);

         Line_Score : Long_Integer;
      begin
         Line_Score := Check_String (Line);

         if Line_Score /= 0 then
            Scores_Vec.Append (Line_Score);
         end if;
      end;
   end loop;

   Vector_Sorter.Sort (Scores_Vec);

   Put_Line ("Result: " &
               Long_Integer'Image (Scores_Vec.Element (Integer (Scores_Vec.Length) / 2 + 1)));

   Close (Input_File);

end Main;
