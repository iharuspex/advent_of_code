with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   subtype Val_Range is Natural range 1 .. 12;
   type Common_Bits is array (Val_Range) of Integer;
   type Bit_Field is array (Val_Range) of Boolean with Pack;
   type Unsigned_12 is mod 2**12;

   Input_File   : File_Type;

   Zero_Counter : Common_Bits := (others => 0);

   Line         : String (Val_Range);
   Lines_Num    : Integer := 0;

   Gamma_Rate   : Unsigned_12 := 0;
   Epsilon_Rate : Unsigned_12 := 0;


   procedure Count_Zeros (Line : in String) is

   begin
      for I in Line'Range loop
         if Line (I) = '0' then
            Zero_Counter (I) := Zero_Counter (I) + 1;
         end if;
      end loop;
   end Count_Zeros;


   function Get_Gamma_Rates (Zeros_Num : in Common_Bits;
                       Lines_Num : in Integer) return Unsigned_12 is
      Ones_Num  : Integer := 0;
      Result    : Unsigned_12 := 0;
      Gamma_BF  : Bit_Field with Address => Result'Address, Import, Volatile;

   begin
      for I in Zeros_Num'Range loop
         Ones_Num := Lines_Num - Zeros_Num (I);

         if Ones_Num > Zeros_Num (I) then
            Gamma_BF (Val_Range'Last - I + 1) := True;
         else
            Gamma_BF (Val_Range'Last - I + 1) := False;
         end if;
      end loop;

      return Result;

   end Get_Gamma_Rates;


begin
   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop
      Line := Get_Line (Input_File);

      Count_Zeros (Line);
      Lines_Num := Lines_Num + 1;

   end loop;

   Close (Input_File);

   Gamma_Rate := Get_Gamma_Rates (Zero_Counter, Lines_Num);
   Epsilon_Rate := not Gamma_Rate;

   New_Line;
   Put ("Gamma: ");
   Put (Integer(Gamma_Rate), 1);
   New_Line;
   Put ("Epsilon: ");
   Put (Integer(Epsilon_Rate), 1);
   New_Line;
   Put ("Result: ");
   Put (Integer(Gamma_Rate) * Integer(Epsilon_Rate), 1);

end Main;
