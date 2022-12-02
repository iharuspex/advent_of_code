with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   type Items is (Rock, Paper, Scissors);

   for Items use (
      Rock => 1,
      Paper => 2,
      Scissors => 3
   );

   Input_File : File_Type;
   My_Item_Ch : Character;
   Opponent_Item_Ch : Character;

   Total_Score_Part1, Total_Score_Part2 : Natural := 0;

   --------------------
   -- Get_Item_By_Ch --
   --------------------

   function Get_Item_By_Ch (Item_Ch : Character) return Items is
   begin
      case Item_Ch is
         when 'A' | 'X' => return Rock;
         when 'B' | 'Y' => return Paper;
         when 'C' | 'Z' => return Scissors;
         when others => return Rock;
      end case;
   end Get_Item_By_Ch;

   ------------------
   -- Get_Win_Item --
   ------------------

   function Get_Win_Item (Opponent_Item : Items) return Items is
   begin
      case Opponent_Item is
         when Rock => return Paper;
         when Paper => return Scissors;
         when Scissors => return Rock;
      end case;
   end Get_Win_Item;

   --------------------
   -- Get_Loose_Item --
   --------------------

   function Get_Loose_Item (Opponent_Item : Items) return Items is
   begin
      case Opponent_Item is
         when Rock => return Scissors;
         when Paper => return Rock;
         when Scissors => return Paper;
      end case;
   end Get_Loose_Item;

   ------------------------------
   -- Calc_Current_Score_Part1 --
   ------------------------------

   function Calc_Current_Score_Part1 (Opponent_Item_Ch, My_Item_Ch : Character) return Natural is
      Opponent_Item, My_Item : Items;

      Round_Score : Natural := 0;
   begin
      Opponent_Item := Get_Item_By_Ch (Opponent_Item_Ch);
      My_Item := Get_Item_By_Ch (My_Item_Ch);

      if My_Item = Opponent_Item then
         Round_Score := 3 + Items'Enum_Rep(My_Item);
         return Round_Score;
      end if;

      if My_Item = Rock and Opponent_Item = Scissors then
         Round_Score := 6 + Items'Enum_Rep(My_Item);
      elsif My_Item = Paper and Opponent_Item = Rock then
         Round_Score := 6 + Items'Enum_Rep(My_Item);
      elsif My_Item = Scissors and Opponent_Item = Paper then
         Round_Score := 6 + Items'Enum_Rep(My_Item);
      else 
         Round_Score := Items'Enum_Rep(My_Item);
      end if;

      return Round_Score;
   end Calc_Current_Score_Part1;

   ------------------------------
   -- Calc_Current_Score_Part2 --
   ------------------------------

   function Calc_Current_Score_Part2 (Opponent_Item_Ch, Round_Result : Character) return Natural is
      Opponent_Item : Items := Get_Item_By_Ch (Opponent_Item_Ch);

      --  X - Loose
      --  Y - Draw
      --  Z - Win
   begin
      case Round_Result is
         when 'X' => return Items'Enum_Rep (Get_Loose_Item (Opponent_Item));
         when 'Y' => return 3 + Items'Enum_Rep (Opponent_Item);
         when 'Z' => return 6 + Items'Enum_Rep (Get_Win_Item (Opponent_Item));
         when others => return 0;
      end case;
   end Calc_Current_Score_Part2;

begin
   Open (Input_File, In_File, "input.txt");
      
   while not End_Of_File (Input_File) loop
      Get (Input_File, Opponent_Item_Ch);
      Get (Input_File, My_Item_Ch);
      Get (Input_File, My_Item_Ch);

      Total_Score_Part1 := Total_Score_Part1 + Calc_Current_Score_Part1 (Opponent_Item_Ch, My_Item_Ch);
      Total_Score_Part2 := Total_Score_Part2 + Calc_Current_Score_Part2 (Opponent_Item_Ch, My_Item_Ch);
   end loop;

   Put_Line ("[Part 1] Total score: " & Total_Score_Part1'Image);
   Put_Line ("[Part 2] Total score: " & Total_Score_Part2'Image);

   Close (Input_File);

end Main;
