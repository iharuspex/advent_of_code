with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

----------
-- Main --
----------

-- 1 check full sequence
-- 2 find the problem element
-- 2 check his neighbours
-- if removing this element fix seq - PROFIT

procedure Main is
   Input_File : File_Type;

   type Seq_Dir is (Undef, Increase, Decrease);

   Result : Integer := 0;

   package Seq_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => Natural);

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
   Open (Input_File, In_File, "test.txt");

   while not End_Of_File (Input_File) loop

      declare
         Num : Natural;

         Dir         : Seq_Dir := Undef;
         Line_Status : Boolean := True;

         Seq : Seq_Vectors.Vector;

      begin
         -- read the whole sequense
         while not End_Of_Line (Input_File) loop
            Get (Input_File, Num);
            Seq.Append (Num);
         end loop;

         for I in Seq.First_Index .. Seq.Last_Index loop
            Put (Seq.Element (I)'Image & " ");
         end loop;
         New_Line;
      end;

      Skip_Line (Input_File);
      Put_Line ("===");
   end loop;

   Put_Line ("Answer: " & Result'Image);

   Close (Input_File);
end Main;
