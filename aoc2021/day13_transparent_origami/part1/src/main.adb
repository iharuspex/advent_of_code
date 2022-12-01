with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   type Paper is array (Natural range <>, Natural range <>) of Boolean;

   type Folding_Type is (horizontal, vertical);

   type Folding is record
      F_Type : Folding_Type;
      Val    : Natural;
   end record;

   Input_File : File_Type;
   Transp_Paper : Paper (0 .. 2000, 0 .. 2000) := (others => (others => False));

   Fld : Folding;

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


   function V_Fold (P: Paper; FV: Natural) return Paper is
      Tmp : Integer;
   begin
      return Ret : Paper (P'Range(1),
                          0 .. Natural'Min (FV - 1, P'Last (2) - FV))
      do
         for Y in P'Range (1) loop
            for K in Ret'Range (2) loop
               Tmp := FV - 1 - (Ret'Last (2) - K);

               Ret (Y, K) :=
                 (Tmp in P'Range(2) and then P (Y, Tmp));

               Tmp := FV + 1 + (Ret'Last (2) - K);

               Ret (Y, K) := Ret (Y, K) or
                 (Tmp in P'Range(2) and then P (Y, Tmp));
            end loop;
         end loop;
      end return;
   end V_Fold;


   function H_Fold (P: Paper; FV: Natural) return Paper is
      Tmp : Integer;
   begin
      return Ret : Paper (0 .. Natural'Min (FV - 1, P'Last (1) - FV),
                          P'Range(2))
      do
         for Y in Ret'Range (1) loop
            for X in P'Range (2) loop
               Tmp := FV - 1 - (Ret'Last (1) - Y);

               Ret (Y, X) :=
                 (Tmp in P'Range(1) and then P (Tmp, X));

               Tmp := FV + 1 + (Ret'Last (1) - Y);

               Ret (Y, X) := Ret (Y, X) or
                 (Tmp in P'Range(1) and then P (Tmp, X));
            end loop;
         end loop;
      end return;
   end H_Fold;


   procedure Do_Fold (P: Paper; F: Folding) is
   begin
      case F.F_Type is
         when horizontal =>
            declare
               Result : constant Paper := H_Fold (P, F.Val);
               Count : Natural := 0;
            begin
               for Dot of Result loop
                  if Dot then
                     Count := Count + 1;
                  end if;
               end loop;
               --Print_Paper (Result);
               Put_Line (Count'Image);
            end;
         when vertical =>
            declare
               Result : constant Paper := V_Fold (P, F.Val);
               Count : Natural := 0;
            begin
               for Dot of Result loop
                  if Dot then
                     Count := Count + 1;
                  end if;
               end loop;
               --Print_Paper (Result);
               Put_Line (Count'Image);
            end;
      end case;
   end Do_Fold;


   procedure Read_Folding (F: out Folding) is
      Prefix : constant String := "fold along x=";
      Line   : constant String := Get_Line (Input_File);
      Number : Natural;
      Ignore : Natural;

   begin
      Get (Line (Prefix'Last + 1 .. Line'Last), Number, Ignore);

      if Line (Prefix'Range) = Prefix then
         F := (vertical, Number);
      else
         F := (horizontal, Number);
      end if;
   end Read_Folding;


begin
   Open (Input_File, In_File, "input.txt");

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

   Read_Folding (Fld);

   --Print_Paper (Transp_Paper);
   --New_Line;

   Do_Fold (Transp_Paper, Fld);

   Close (Input_File);

end Main;
