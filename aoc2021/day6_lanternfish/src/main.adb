with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps; use Ada.Strings.Maps;

procedure Main is

   subtype Internal_Timer is Natural range 0 .. 8;
   type Tim_Array is array (Internal_Timer) of Long_Long_Integer;

   Generations   : constant Positive := 256;
   Input_File    : File_Type;
   Lanternfishes : Tim_Array := (others => 0);
   Curr_Idx      : Internal_Timer;


   function Get_Result (Arr: Tim_Array) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
   begin
      for I in Internal_Timer loop
         Result := Result + Arr (I);
      end loop;

      return Result;
   end Get_Result;


begin
   Open (Input_File, In_File, "input.txt");

   declare
      Line  : constant String := Get_Line (Input_File);
      Comma : constant Character_Set := To_Set (",");
      From  : Natural := 0;
      To    : Natural := 0;
      Idx   : Internal_Timer;
   begin
      loop
         exit when From + 1 > Line'Last;

         Find_Token (Line, Comma, From + 1, Ada.Strings.Outside, From, To);

         Idx := Internal_Timer'Value (Line (From .. To));
         Lanternfishes (Idx) := Lanternfishes (Idx) + 1;
      end loop;
   end;

   for I in 0 .. Generations - 1 loop
      Curr_Idx := (I + 7) rem 9;
      Lanternfishes (Curr_Idx) := Lanternfishes (Curr_Idx) + Lanternfishes (I rem 9);
   end loop;

   Put_Line ("Result : " & Get_Result(Lanternfishes)'Image);

   Close (Input_File);

end Main;
