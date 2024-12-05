with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

----------
-- Main --
----------

procedure Main is
   Input_File : File_Type;

   type Seq_Dir is (Undef, Increase, Decrease);

   Result : Integer := 0;

   function Part1 return Integer is
   begin
      return 0;
   end Part1;

   ----------------
   -- Check_Pair --
   ----------------

   function Check_Pair (A, B : in Integer; Dir : in out Seq_Dir) return Boolean
   is
      Diff     : Integer;
      Pair_Dir : Seq_Dir;
   begin
      -- check direction
      if A < B then
         Pair_Dir := Increase;
      else
         Pair_Dir := Decrease;
      end if;

      if Dir = Undef then
         Dir := Pair_Dir;
      else
         if Dir /= Pair_Dir then
            return False;
         end if;
      end if;

      -- check diff
      Diff := abs (A - B);
      if Diff < 1 or Diff > 3 then
         return False;
      end if;

      return True;
   end Check_Pair;

begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      declare
         A : Natural;
         B : Natural;

         Dir         : Seq_Dir := Undef;
         Line_Status : Boolean := True;

      begin
         -- read first pair
         Get (Input_File, A);
         Get (Input_File, B);

         Line_Status := Check_Pair (A, B, Dir);

         while not End_Of_Line (Input_File) loop
            exit when Line_Status = False;

            A := B;
            Get (Input_File, B);
            Line_Status := Check_Pair (A, B, Dir);
         end loop;

         Put_Line (Line_Status'Image);

         if Line_Status = True then
            Result := Result + 1;
         end if;
      end;

      Skip_Line (Input_File);
      Put_Line ("===");
   end loop;

   Put_Line ("Answer: " & Result'Image);

   Close (Input_File);
end Main;
