with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


procedure Main is
   type Paper is array (Natural range <>, Natural range <>) of Boolean;

   Input_File : File_Type;

   Transp_Paper : Paper (0 .. 20, 0 .. 20) := (others => (others => False));

   procedure String_Split
     (In_Str: in String;
      Split_By: in String;
      Part1: out Natural;
      Part2: out Natural)
   is
      Delimiter_Idx : Natural := 0;
   begin
      Delimiter_Idx := Index
        (Source => In_Str,
         Pattern => Split_By,
         From => In_Str'First);

      Part1 := Natural'Value (In_Str (In_Str'First .. Delimiter_Idx - 1));
      Part2 := Natural'Value (In_Str (Delimiter_Idx + 1 .. In_Str'Last));
   end String_Split;

   procedure Print_Paper (P : Paper) is
   begin
      for I in P'Range (1) loop
         for J in P'Range (2) loop
            if P (I, J) then
               Put ("#");
            else
               Put (".");
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Paper;

begin
   Open (Input_File, In_File, "input_test.txt");

   while not End_Of_File (Input_File) loop

      declare
         Line : String := Get_Line (Input_File);
         X, Y : Natural;
      begin
         exit when Line = "";
         String_Split (Line, ",", X, Y);
         Transp_Paper (Y, X) := True;
      end;

   end loop;

   Print_Paper (Transp_Paper);

   Close (Input_File);

end Main;
