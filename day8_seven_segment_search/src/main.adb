with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Main is
   Input_File : File_Type;

   Result : Natural := 0;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      declare
         Line : String := Get_Line (Input_File);
         Output_Values : String := Tail (Line, Line'Last - Index (Line, "|", Line'Last, Ada.Strings.Backward) - 1);
         Whitespace : constant Character_Set := To_Set (' ');
         From : Natural := 0;
         To   : Natural := 0;
         I    : Natural := 1;
         Word_Len : Natural;
      begin
         Put_Line (Output_Values);
         loop
            exit when From + 1 > Output_Values'Last;
            Find_Token (Output_Values, Whitespace, From + 1, Ada.Strings.Outside, From, To);

            Word_Len := To - From + 1;

            Put (Output_Values (From .. To) & ", letters = " & Word_Len'Image);

            if Word_Len /= 5 and Word_Len /= 6 then
               Result := Result + 1;
            end if;

            New_Line;
            From := To;
         end loop;
      end;

   end loop;

   Put_Line ("Result: " & Result'Image);

   Close (Input_File);

end Main;
