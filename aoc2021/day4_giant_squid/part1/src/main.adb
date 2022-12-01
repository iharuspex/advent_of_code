with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   type Arr is array (Positive range <>) of Natural;

   type Cell is record
      Value : Integer;
      Mark  : Boolean := False;
   end record;

   subtype Width is Positive range 1 .. 5;
   type Table_Type is array (Width, Width) of Cell;

   type Cards is array (1 .. 100) of Table_Type;

   Input_File       : File_Type;

   Players_Cards    : Cards;
   Turn_Numbers     : Arr (1 .. 200) := (others => 0);
   Num_Count        : Positive := 1;


   procedure Print_Arr (A: in Arr; Size: in Natural) is
   begin
      Put ("[ ");
      for I in 1 .. Size loop
         Put (A(I), 1);
         Put (" ");
      end loop;
      Put ("]");
   end;


   procedure Print_Card (Card: in Table_Type) is
   begin
      for I in Card'Range(1) loop
         for J in Card'Range(2) loop
            Put (Card (I, J).Value, 4);
            if Card (I, J).Mark = True then
               Put ("*");
            end if;
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Card;


   function Get_Token (Str: String;
                       S_Pos: in out Integer;
                       Delim: Character) return String is
      Pos : Integer := S_Pos;
   begin
      for I in S_Pos .. Str'Last loop
         if Str (I) = Delim then
            S_Pos := I + 1;
            return Str (Pos .. I - 1);
         end if;
      end loop;
      S_Pos := S_Pos + 1;
      return Str (Pos .. Pos);
   end Get_Token;


   --  Check if win
   function Check_Card (Card: Table_Type) return Boolean is
      Counter : Natural := 0;
   begin
      --  check rows
      for I in Card'Range(1) loop
         for J in Card'Range(2) loop
            if Card (I, J).Mark = True then
               Counter := Counter + 1;
            end if;
         end loop;

         if Counter = Card'Last(2) then
            return True;
         end if;

         Counter := 0;
      end loop;

      Counter := 0;

      --  check columns
      for I in Card'Range(1) loop
         for J in Card'Range(2) loop
            if Card (J, I).Mark = True then
               Counter := Counter + 1;
            end if;
         end loop;

         if Counter = Card'Last(1) then
            return True;
         end if;

         Counter := 0;
      end loop;

      return False;
   end Check_Card;


   function Calc_Score (Card: Table_Type; Winning_Num: Natural) return Natural is
      Sum_Unmarked : Natural := 0;
   begin
      for I in Card'Range(1) loop
         for J in Card'Range(2) loop
            if Card(I, J).Mark = False then
               Sum_Unmarked := Sum_Unmarked + Card(I, J).Value;
            end if;
         end loop;
      end loop;

      return (Sum_Unmarked * Winning_Num);
   end Calc_Score;


   function Game (Numbers: Arr;
                  Num_Size: Positive;
                  Bingo_Cards: in out Cards) return Natural is
   begin
      for N in 1 .. Num_Size loop
         Put ("Iteration: ");
         Put (N, 1);
         Put (", Num: ");
         Put (Numbers (N), 1);

         for Card of Bingo_Cards loop
            for I in Card'Range(1) loop
               for J in Card'Range(2) loop
                  if Card(I, J).Value = Numbers (N) then
                     Card(I, J).Mark := True;

                     if (Check_Card (Card) = True) then
                        New_Line;
                        Put_Line ("Winner:");
                        Print_Card (Card);

                        return Calc_Score (Card, Numbers (N));
                     end if;
                  end if;
               end loop;
            end loop;
            New_Line;
            Print_Card (Card);
            New_Line;
         end loop;
      end loop;
      return 0;
   end Game;

begin
   Open (Input_File, In_File, "input.txt");

   -- Read turn numbers from first string
   Read_Turn_Numbers:
   declare
      TN_Str : String := Get_Line (Input_File);
      S_Pos  : Integer := TN_Str'First;
   begin

      while S_Pos <= TN_Str'Last loop
         -- FIXME: Tokenizer works pretty poorly
         Turn_Numbers (Num_Count) := Natural'Value (Get_Token (TN_Str, S_Pos, ','));
         Num_Count := Num_Count + 1;
      end loop;
   end Read_Turn_Numbers;


   -- Get players cards
   while not End_Of_File (Input_File) loop
      for Card of Players_Cards loop
         for I in Card'Range(1) loop
            for J in Card'Range(2) loop
               Get (Input_File, Card(I, J).Value);
            end loop;
         end loop;
      end loop;
   end loop;

   Print_Arr (Turn_Numbers, Num_Count);
   New_Line;

   Put (Game (Turn_Numbers, Num_Count, Players_Cards));

   Close (Input_File);

end Main;
