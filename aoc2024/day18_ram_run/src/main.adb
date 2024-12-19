with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Main is

   --  Filename : constant String := "test1.txt";
   --  Map_Size : constant Natural := 6+1;
   --  Bytes    : constant Natural := 12;

   Filename : constant String := "input.txt";
   Map_Size : constant Natural := 70+1;
   Bytes    : constant Natural := 1024;

   type Point_Type is record
      Y, X : Integer;
   end record;

   function "=" (Left, Right: Point_Type) return Boolean is
   begin
      return (Left.X = Right.X and Left.Y = Right.Y);
   end "=";

   type Dir_Type is (Up, Down, Left, Right);

   type Cell_Dir_Type is record
      Cell : Point_Type;
      Dir : Dir_Type;
   end record;

   type Cost_Type is record
      Cost : Integer;
      Dir  : Dir_Type;
   end record;

   type Queue_Element is record
      Priority : Natural;
      Cell_With_Dir : Cell_Dir_Type;
   end record;

   function "<" (Left, Right: Queue_Element) return Boolean is
   begin
      return Left.Priority < Right.Priority;
   end "<";

   type Map_Type is array (Positive range <>, Positive range <>) of Character;

   package Cell_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Point_Type);

   package Cell_Dir_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Cell_Dir_Type);

   ----------
   -- Hash --
   ----------

   function Hash (Key : Point_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Key.Y * 1000 + Key.X);
   end Hash;

   package Cell_Dir_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Point_Type,
      Element_Type    => Cell_Dir_Type,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package Cost_Dir_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Point_Type,
      Element_Type    => Cost_Type,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   package Priority_Queues is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Queue_Element);

   package Priority_Queue_Sorting is new
     Priority_Queues.Generic_Sorting;
   use Priority_Queue_Sorting;

   subtype Priority_Queue is Priority_Queues.Vector;

   ----------
   -- Push --
   ----------

   procedure Push(Q: in out Priority_Queue; Value: Cell_Dir_Type; Prio: Natural) is
      New_Node : Queue_Element := (Priority => Prio, Cell_With_Dir => Value);
   begin
      Q.Append(New_Node);
      Sort (Q);
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop(Q: in out Priority_Queue) return Queue_Element is
      Result : Queue_Element;
   begin
      if Q.Last_Index > 0 then
         Result := Q(1);
         Q.Delete(1);
         return Result;
      else
         raise Constraint_Error with "Queue is empty";
      end if;
   end Pop;

   F : File_Type;

   Map : Map_Type (1 .. Map_Size, 1 .. Map_Size) := (others => (others => '.'));
   Bytes_Num : Natural := 0;

   Start_Point : Point_Type;
   End_Point : Point_Type;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Map_Type) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            Put (M (I, J));
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   ------------------------
   -- Manhattan_Distance --
   ------------------------

   function Manhattan_Distance (Current, Goal : Point_Type) return Integer is
   begin
      return abs(Current.X - Goal.X) + abs(Current.Y - Goal.Y);
   end Manhattan_Distance;

   -------------
   -- Get_Dir --
   -------------

   function Get_Dir(Current, Next : Point_Type) return Dir_Type is
   begin
      if Next.Y > Current.Y then
         return Down;
      elsif Next.Y < Current.Y then
         return Up;
      elsif Next.X > Current.X then
         return Right;
      elsif Next.X < Current.X then
         return Left;
      end if;

      return Up;
   end Get_Dir;

   ------------
   -- A_Star --
   ------------

   procedure A_Star (M : Map_Type;
                    Start_P, End_P : Point_Type;
                    Path : out Cell_Vectors.Vector;
                    Dirs : out Cell_Dir_Vectors.Vector)
   is
      Frontier : Priority_Queue;
      Came_From : Cell_Dir_Maps.Map;
      Cost_So_Far : Cost_Dir_Maps.Map;
      Current : Point_Type;
      Dir : Dir_Type := Right;
      Priority : Integer;

      Element : Queue_Element;

      type Dirs_Array_Type is array (1 .. 4) of Point_Type;

      Dir_Array : constant Dirs_Array_Type := ((0, 1), (1, 0), (0, -1), (-1, 0));
   begin
      Push (Frontier, (Start_P, Dir), 0);
      Came_From.Include (Start_P, (Start_P, Dir));
      Cost_So_Far.Include (Start_P, (0, Dir));

      while not Frontier.Is_Empty loop
         Element := Pop (Frontier);
         Current := Element.Cell_With_Dir.Cell;
         Dir := Element.Cell_With_Dir.Dir;

         exit when Element.Cell_With_Dir.Cell = End_P;

         for D of Dir_Array loop
            declare
               Next_Cell : Point_Type := (Current.Y + D.Y, Current.X + D.X);
               New_Dir : Dir_Type := Get_Dir (Current, Next_Cell);
               --  Turn_Cost : Integer := (if New_Dir /= Dir then 10 else 1);
               Turn_Cost : Integer := 1;
               New_Cost : Integer := Cost_So_Far.Element (Current).Cost + Turn_Cost;
            begin
               if (Next_Cell.Y in M'Range(1) and Next_Cell.X in M'Range(2)) and then
                 M (Next_Cell.Y, Next_Cell.X) /= '#' then
                  if not Cost_So_Far.Contains (Next_Cell) or else New_Cost < Cost_So_Far.Element (Next_Cell).Cost then
                     Cost_So_Far.Include (Next_Cell, (New_Cost, New_Dir));
                     Priority := New_Cost + Manhattan_Distance (End_P, Next_Cell);
                     Push (Frontier, (Next_Cell, New_Dir), Priority);
                     Came_From.Include (Next_Cell, (Current, New_Dir));
                  end if;
               end if;
            end;
         end loop;
      end loop;

      while Current /= Start_P loop
         Path.Append (Current);
         Dirs.Append((Current, Dir));
         Current := Came_From.Element(Current).Cell;
         Dir := Came_From.Element(Current).Dir;
      end loop;

      Path.Append (Start_P);
      Dirs.Append ((Start_P, Dir));

      Path.Reverse_Elements;
      Dirs.Reverse_Elements;
   end A_Star;

   ----------------
   -- Print_Path --
   ----------------

   procedure Print_Path (M : Map_Type; Path : Cell_Vectors.Vector; Dirs : Cell_Dir_Vectors.Vector) is
      --  Direction_Map : constant array (Dir_Type) of Character := ('^', 'v', '<', '>');
      Direction_Map : constant array (Dir_Type) of Character := (others => 'O');
   begin
      for I in M'Range (1) loop
         for J in M'Range(2) loop
            if (I, J) = Path.First_Element then
               Put ('S');
            elsif (I, J) = Path.Last_Element then
               Put ('E');
            elsif Path.Contains ((I, J)) then
               declare
                  Idx : constant Natural := Path.Find_Index((I, J));
               begin
                  Put (Direction_Map (Dirs.Element (Idx).Dir));
               end;
            else
               Put (M (I, J));
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Path;

   -----------------
   -- Calc_Result --
   -----------------

   procedure Calc_Result (Dirs : Cell_Dir_Vectors.Vector) is
      Steps : Integer := Dirs.Last_Index;
      Turns : Integer := 0;

      Result : Integer := 0;
   begin
      for I in 2 .. Dirs.Last_Index loop
         if Dirs (I).Dir /= Dirs (I - 1).Dir then
            Turns := Turns + 1;
         end if;
      end loop;

      Put_Line ("Steps:" & Steps'Image);
      Put_Line ("Turns:" & Turns'Image);

      if Dirs (1).Dir = Up then
         Turns := Turns + 1;
      end if;

      Result := (Turns * 1000) + Steps;

      Put_Line ("Result:" & Result'Image);
   end Calc_Result;

   ----------------
   -- Place_Wall --
   ----------------

   procedure Place_Wall (M : in out Map_Type; Line : String) is
      Comma_Idx : Natural := Index (Line, ",");

      Wall_Coords : Point_Type;
   begin
      Wall_Coords.X := Integer'Value (Line (Line'First .. Comma_Idx-1));
      Wall_Coords.Y := Integer'Value (Line (Comma_Idx+1 .. Line'Last));

      M (Wall_Coords.Y+1, Wall_Coords.X+1) := '#';
   end Place_Wall;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Place_Wall (Map, Line);

         Bytes_Num := Bytes_Num + 1;

         if Bytes_Num >= Bytes then

            declare
               Path : Cell_Vectors.Vector;
               Dirs : Cell_Dir_Vectors.Vector;
            begin
               A_Star (Map, (1,1), (Map_Size,Map_Size), Path, Dirs);

               if Bytes_Num = Bytes then
                  Put_Line ("Part1: " & Line);
                  Calc_Result (Dirs);
               end if;

               if Path.Last_Element /= (Map_Size,Map_Size) then
                  New_Line;
                  --  Print_Path (Map, Path, Dirs);
                  Put_Line ("Part2: " & Line);
                  exit;
               end if;
            end;

         end if;
      end;

   end loop;

   Close (F);
end Main;
