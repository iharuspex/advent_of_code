with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   --  Filename : constant String := "test.txt";
   --  Map_Size : constant Positive := 8;

   Filename : constant String := "input.txt";
   Map_Size : constant Positive := 55;

   -- types definitions
   type Position_Type is record
      Height : Natural;
      Mark   : Boolean;
   end record;

   type Topo_Map is array (1 .. Map_Size, 1 .. Map_Size) of Position_Type;

   --  variables
   F : File_Type;
   Map : Topo_Map := (others => (others => (0, False)));
   Map_Line : Positive := 1;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Topo_Map) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            Put (Item => M (I, J).Height, Width => 1);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   -----------------
   -- Print_Marks --
   -----------------

   procedure Print_Marks (M : Topo_Map) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            if M (I, J).Mark = True then
               Put ("x ");
            else
               Put ("o ");
            end if;

         end loop;
         New_Line;
      end loop;
   end Print_Marks;

   --------------------------
   -- Get_Height_By_Coords --
   --------------------------

   function Get_Height_By_Coords (Map : Topo_Map; Y, X : Natural) return Natural
   is
      Ret : Natural := 255; -- invalid val
   begin
      if X in Map'Range(2) and Y in Map'Range(1) then
         Ret := Map (Y,X).Height;
      end if;

      return Ret;
   end Get_Height_By_Coords;

   --------------------------
   -- Check_Mark_By_Coords --
   --------------------------

   function Check_Mark_By_Coords (Map : Topo_Map; Y, X : Natural; P2 : Boolean := False) return Boolean
   is
      Ret : Boolean := False;
   begin
      if P2 = False then
         if (X in Map'Range(2)) and (Y in Map'Range(1)) then
            Ret := Map (Y,X).Mark;
         end if;
      end if;

      return Ret;
   end Check_Mark_By_Coords;

   ---------------
   -- Find_Path --
   ---------------

   procedure Find_Path (Map : in out Topo_Map; Y, X : Natural; Score : in out Natural; P2 : Boolean := False) is
      Current_Height : Natural := Map (Y, X).Height;
   begin
      -- look around and go to the position with the needed height
      -- check coords:
      --  - Map (Y-1, X)
      --  - Map (Y, X+1)
      --  - Map (Y+1, X)
      --  - Map (Y, X-1)
      --
      --  | ------------ | Map (Y-1, X) | ------------ |
      --  | Map (Y, X-1) | Map (Y, X)   | Map (Y, X+1) |
      --  | ------------ | Map (Y+1, X) | ------------ |
      --

      if Current_Height /= 9 then
         --  Put_Line ("[" & Y'Image & "," & X'Image & "]" & " height =" & Current_Height'Image);

         if Get_Height_By_Coords (Map, Y-1, X) = Current_Height + 1 and Check_Mark_By_Coords (Map, Y-1, X, P2) = False then
            Find_Path (Map, Y-1, X, Score, P2);
         end if;

         if Get_Height_By_Coords (Map, Y, X+1) = Current_Height + 1 and Check_Mark_By_Coords (Map, Y, X+1, P2) = False then
            Find_Path (Map, Y, X+1, Score, P2);
         end if;

         if Get_Height_By_Coords (Map, Y+1, X) = Current_Height + 1 and Check_Mark_By_Coords (Map, Y+1, X, P2) = False then
            Find_Path (Map, Y+1, X, Score, P2);
         end if;

         if Get_Height_By_Coords (Map, Y, X-1) = Current_Height + 1 and Check_Mark_By_Coords (Map, Y, X-1, P2) = False then
            Find_Path (Map, Y, X-1, Score, P2);
         end if;

      else
         if P2 = True then
            Score := Score + 1;
         else
            if Map (Y, X).Mark = False then
               Score := Score + 1;
               Map (Y, X).Mark := True;
            end if;
         end if;

      end if;

   end Find_Path;

   -------------
   -- Process --
   -------------

   procedure Process (Map : in out Topo_Map) is
      Result_P1 : Natural := 0;
      Result_P2 : Natural := 0;

   begin
      for I in Map'Range(1) loop
         for J in Map'Range(2) loop
            if Map (I, J).Height = 0 then
               declare
                  M : Topo_Map := Map;
                  Score : Natural := 0;
                  Rating : Natural := 0;
               begin
                  Put_Line ("Finding path from " & I'Image & "," & J'Image);
                  Find_Path (M, I, J, Score);
                  Find_Path (M, I, J, Rating, True);
                  Put_Line ("Score: " & Score'Image);
                  Put_Line ("Rating: " & Rating'Image);
                  Result_P1 := Result_P1 + Score;
                  Result_P2 := Result_P2 + Rating;
                  --  Print_Marks(M);
               end;
               --  Print_Map (Map);
            end if;
         end loop;
      end loop;

      Put_Line ("=============");
      Put_Line ("Part1: " & Result_P1'Image);
      Put_Line ("Part2: " & Result_P2'Image);
   end Process;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         for I in Line'Range loop
            Map (Map_Line, I).Height := Natural'Value (Line (I .. I));
         end loop;
         Map_Line := Map_Line + 1;
      end;
   end loop;

   Print_Map (Map);

   Process (Map);

   Close (F);

end Main;
