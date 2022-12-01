with Ada.Text_IO; use Ada.Text_IO;
with Stack;


procedure Main is
   Input_File : File_Type;
   Result     : Natural := 0;


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


   function Check_String (Str : String) return Natural is
      Ch : Character;
   begin
      for I in Str'Range loop
         if Str (I) = '(' or Str (I) = '[' or Str (I) = '{' or Str (I) = '<' then
            Stack.Push (Str (I));
         else
            Ch := Stack.Pop;

            if Wrong_Pair (Ch, Str (I)) then
               case Str (I) is
                  when ')' =>
                     return 3;
                  when ']' =>
                     return 57;
                  when '}' =>
                     return 1197;
                  when '>' =>
                     return 25137;
                  when others =>
                     null;
               end case;
            end if;
         end if;
      end loop;

      return 0;

   end Check_String;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      declare
         Line : String := Get_Line (Input_File);
      begin
         Result := Result + Check_String (Line);
      end;
   end loop;

   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
