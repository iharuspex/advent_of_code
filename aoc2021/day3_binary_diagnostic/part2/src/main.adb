with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   subtype Val_Range is Natural range 1 .. 12;
   type Values is array (Positive range 1 .. 1000) of String (Val_Range);
   type Find_Mode is (less, great);
   type Data_Type is (scrubber, generator);
   type Bit_Check_Result is (zeros, ones, equal);

   Input_File   : File_Type;

   Arr          : Values;
   Scrubber_Val : Integer;
   Generator_Val : Integer;

   procedure Print_Arr (Arr: in Values; Size: in Natural) is
   begin
      Put ("[ ");
      for I in 1 .. Size loop
         Put (Arr(I));
         Put (" ");
      end loop;
      Put ("]");
   end;

   function Bit_Check (Arr: in Values;
                       Size: in Natural;
                       Pos : in Natural;
                       FM: Find_Mode) return Bit_Check_Result is
      Z : Natural := 0;
   begin
      for I in 1 .. Size loop
         if Arr (I)(Pos) = '0' then
            Z := Z + 1;
         end if;
      end loop;

      if FM = great then
         if Z > (Size - Z) then
            return zeros;
         elsif Z < (Size - Z) then
            return ones;
         else
            return equal;
         end if;
      else
         if Z < (Size - Z) then
            return zeros;
         elsif Z > (Size - Z) then
            return ones;
         else
            return equal;
         end if;
      end if;

   end Bit_Check;

   procedure Rewrite_Arr (Arr: in Values;
                          Size: in Natural;
                          Pos: in Natural;
                          Bit: in Character;
                          Out_Arr : out Values;
                          Out_Size : out Natural) is
   begin
      Out_Size := 0;

      for I in 1 .. Size loop
         if Arr (I)(Pos) = Bit then
            Out_Arr (Out_Size + 1) := Arr (I);
            Out_Size := Out_Size + 1;
         end if;
      end loop;
   end Rewrite_Arr;

   procedure Find_Value (Arr: in Values;
                         Size: in Natural;
                         DT: in Data_Type;
                         Output: out Integer) is
      Check_Res : Bit_Check_Result;

      New_Arr : Values := Arr;
      New_Size : Natural := Size;

      FM : Find_Mode;

      Fmt_Str : String (Val_Range'First .. Val_Range'Last + 3);

   begin
      if DT = generator then
         FM := great;
      else
         FM := less;
      end if;

      for I in Val_Range loop
         exit when New_Size = 1;

         Check_Res := Bit_Check (New_Arr, New_Size, I, FM);

         case Check_Res is
         when ones =>
            Rewrite_Arr (New_Arr, New_Size, I, '1', New_Arr, New_Size);
            Print_Arr (New_Arr, New_Size);
            New_Line;
         when zeros =>
            Rewrite_Arr (New_Arr, New_Size, I, '0', New_Arr, New_Size);
            Print_Arr (New_Arr, New_Size);
            New_Line;
         when equal =>
            case DT is
               when scrubber =>
                  Rewrite_Arr (New_Arr, New_Size, I, '0', New_Arr, New_Size);
                  Print_Arr (New_Arr, New_Size);
                  New_Line;
               when generator =>
                  Rewrite_Arr (New_Arr, New_Size, I, '1', New_Arr, New_Size);
                  Print_Arr (New_Arr, New_Size);
                  New_Line;
            end case;
         end case;
      end loop;

      Fmt_Str := "2#" & New_Arr(1) & "#";
      Output := Integer'Value (Fmt_Str);
   end Find_Value;

begin
   Open (Input_File, In_File, "input.txt");

   for I in Arr'Range loop
      exit when End_Of_File (Input_File);

      Arr (I) := Get_Line (Input_File);
   end loop;

   Find_Value (Arr, Arr'Last(1), generator, Generator_Val);
   Put_Line (Generator_Val'Image);
   New_Line;
   Find_Value (Arr, Arr'Last(1), scrubber, Scrubber_Val);
   Put_Line (Scrubber_Val'Image);

   Put ("Answer: ");
   Put (Generator_Val * Scrubber_Val);

   Close (Input_File);

end Main;
