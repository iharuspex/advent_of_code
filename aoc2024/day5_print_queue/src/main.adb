with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

----------
-- Main --
----------

procedure Main is
   package Natural_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Natural);

   type Rule is record
      First_Page : Natural;
      Second_Page : Natural;
   end record;

   type Rules is array (Positive range <>) of Rule;

   type Pages_List is array (Positive range <>) of Natural_Vectors.Vector;

   F : File_Type;

   Rules_List : Rules (1 .. 1200);
   Rules_Num : Natural := 0;

   Pages_Data : Pages_List (1 .. 200);
   Pages_Data_Num : Natural := 0;

   Wrong_Pages_Data : Pages_List (1 .. 200);
   Wrong_Pages_Data_Num : Natural := 0;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule (Line : String) return Rule is
      Ret : Rule;
      Sep_Index : Natural := Index (Line, "|");
   begin
      Ret.First_Page := Natural'Value (Line (Line'First .. Sep_Index-1));
      Ret.Second_Page := Natural'Value (Line (Sep_Index+1 .. Line'Last));

      return Ret;
   end Get_Rule;

   -------------------
   -- Get_Pages_Num --
   -------------------

   function Get_Pages_Numbers (Line : String) return Natural_Vectors.Vector is
      Ret : Natural_Vectors.Vector;

      Start_Idx : Natural := Line'First;
      End_Idx : Natural;
      Num : Natural;
      Last : Natural;
   begin
      loop
         End_Idx := Index (Line (Start_Idx .. Line'Last), ",");
         if End_Idx = 0 then
            End_Idx := Line'Last+1;
         end if;
         --  else
         --     End_Idx := Start_Idx + End_Idx - 2;
         --  end if;

         Get (Line (Start_Idx .. End_Idx-1), Num, Last);
         Ret.Append (Num);

         exit when End_Idx = Line'Last+1;
         Start_Idx := End_Idx + 1;
      end loop;

      return Ret;
   end Get_Pages_Numbers;

   -----------------
   -- Print_Rules --
   -----------------

   procedure Print_Rules is
   begin
      for I in Rules_List'First .. Rules_Num loop
         Put_Line (Rules_List(I).First_Page'Image
                   & " | "
                   & Rules_List(I).Second_Page'Image);
      end loop;
   end;

   ----------------------
   -- Print_Pages_Data --
   ----------------------

   procedure Print_Pages_Data is
   begin
      for I in Pages_Data'First .. Pages_Data_Num loop
         for J of Pages_Data (I) loop
            Put (J'Image & " ");
         end loop;
         New_Line;
      end loop;
   end;

   procedure Print_One_Page_Line (Line : Natural_Vectors.Vector) is
   begin
      for J of Line loop
         Put (J'Image & ", ");
      end loop;
      New_Line;
   end;

   ----------------
   -- Check_Rule --
   ----------------

   function Check_Rule (Rule_X : Rule; Pages : Natural_Vectors.Vector)
                           return Boolean
   is
      Ret : Boolean := False;

      Left_Idx : Natural := 0;
      Right_Idx : Natural := 0;
   begin
      for I in Pages.First_Index .. Pages.Last_Index loop
         if Pages (I) = Rule_X.First_Page then
            Left_Idx := I;
         elsif Pages (I) = Rule_X.Second_Page then
            Right_Idx := I;
         end if;
      end loop;

      -- if rule's page not found
      if Left_Idx = 0 or Right_Idx = 0 then
         Ret := True;
      else
         Ret := Left_Idx < Right_Idx;
      end if;

      return Ret;
   end Check_Rule;

   ----------------
   -- Check_Rule --
   ----------------

   function Check_Rule_And_Swap (Rule_X : Rule; Pages : in out Natural_Vectors.Vector)
                           return Boolean
   is
      Ret : Boolean := False;

      Left_Idx : Natural := 0;
      Right_Idx : Natural := 0;
      Tmp : Natural := 0;
   begin
      for I in Pages.First_Index .. Pages.Last_Index loop
         if Pages (I) = Rule_X.First_Page then
            Left_Idx := I;
         elsif Pages (I) = Rule_X.Second_Page then
            Right_Idx := I;
         end if;
      end loop;

      -- if rule's page not found
      if Left_Idx = 0 or Right_Idx = 0 then
         Ret := True;
      elsif Left_Idx > Right_Idx then
         --  Tmp := Pages (Left_Idx);
         --  Pages. (Left_Idx) := Pages (Right_Idx);
         --  Pages (Right_Idx) := Tmp;
         Pages.Swap(Left_Idx, Right_Idx);
         Ret := False;
      else
         Ret := Left_Idx < Right_Idx;
      end if;

      return Ret;
   end Check_Rule_And_Swap;

   -----------
   -- Part1 --
   -----------

   procedure Part1 is
      Check_Result : Boolean := False;
      Result : Natural := 0;

   begin
      for I in Pages_Data'First .. Pages_Data_Num loop
         for J in Rules_List'First .. Rules_Num loop
            Check_Result := Check_Rule (Rules_List(J), Pages_Data (I));
            exit when Check_Result = False;
         end loop;

         if Check_Result = True then
            Put_Line (I'Image);
            Result := Result + Pages_Data(I).Element ((Pages_Data (I).Last_Index / 2) + 1);
         else
            Wrong_Pages_Data_Num := Wrong_Pages_Data_Num + 1;
            Wrong_Pages_Data (Wrong_Pages_Data_Num) := Pages_Data (I);
         end if;
      end loop;

      Put_Line ("Part1 result: " & Result'Image);
   end Part1;

   -----------
   -- Part2 --
   -----------

   procedure Part2 is
      Check_Result : Boolean := False;
      Result : Natural := 0;

      Ret : Boolean := False;

      Left_Idx : Natural := 0;
      Right_Idx : Natural := 0;

   begin
      for I in Wrong_Pages_Data'First .. Wrong_Pages_Data_Num loop
         Check_Result := False;

         while Check_Result = False loop
            for J in Rules_List'First .. Rules_Num loop
               Check_Result := Check_Rule_And_Swap (Rules_List(J), Wrong_Pages_Data (I));
               exit when Check_Result = False;
            end loop;
         end loop;
         Print_One_Page_Line (Wrong_Pages_Data (I));
         Result := Result + Wrong_Pages_Data(I).Element ((Wrong_Pages_Data (I).Last_Index / 2) + 1);
      end loop;

      Put_Line ("Part2 result: " & Result'Image);
   end Part2;

begin
   Open (F, In_File, "input.txt");

   -- read the rules
   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         exit when Line = "";

         Rules_Num := Rules_Num + 1;
         Rules_List (Rules_Num) := Get_Rule (Line);
      end;
   end loop;

   -- read the pages
   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Pages_Data_Num := Pages_Data_Num + 1;
         Pages_Data (Pages_Data_Num) := Get_Pages_Numbers (Line);
      end;
   end loop;

   Print_Rules;
   New_Line;
   Print_Pages_Data;

   -- now check the pages sequencies
   Part1;
   Part2;

end Main;
