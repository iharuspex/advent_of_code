with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Main is
   -- Types definitions
   type Map_Type is array (Natural range <>) of String (1..130);

   type View_Type is (Up, Right, Down, Left);

   type Guard_Type is record
      X : Natural;
      Y : Natural;
      View_Dir : View_Type;
   end record;

   ---------
   -- "+" --
   ---------

   function "+" (Left : View_Type; Right : Natural) return View_Type is
      Ret : View_Type;
      N : Natural :=
        (View_Type'Pos (Left) + Right) mod (View_Type'Pos (View_Type'Last) + 1);
   begin
      Ret := View_Type'Val (N);
      return Ret;
   end "+";

   -- Variables
   F : File_Type;

   Map      : Map_Type (1 .. 130);
   Map_Size : Natural := 0;

   Guard : Guard_Type;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map is
   begin
      for I in 1 .. Map_Size loop
         Put_Line (Map (I) (1 .. Map_Size));
      end loop;
   end Print_Map;

   -----------------
   -- Print_Guard --
   -----------------

   procedure Print_Guard (G : Guard_Type) is
   begin
      Put_Line ("===============");
      Put_Line ("X =" & Integer'Image (G.X) & "; Y =" & Integer'Image (G.Y));
      Put_Line ("View dir : " & View_Type'Image (G.View_Dir));
      Put_Line ("===============");
   end Print_Guard;

   procedure Turn_Guard (G : in out Guard_Type) is
   begin
      G.View_Dir := G.View_Dir + 1;
   end Turn_Guard;

   function Move (G : Guard_Type) return Guard_Type is
      New_G : Guard_Type := G;
   begin
      case G.View_Dir is
         when Up =>
            New_G.Y := G.Y - 1;
         when Down =>
            New_G.Y := G.Y + 1;
         when Left =>
            New_G.X := G.X - 1;
         when Right =>
            New_G.X := G.X + 1;
      end case;
      return New_G;
   end Move;

   function Simulate_Guard_Path (Map : Map_Type; Map_Size : Natural; Guard : Guard_Type) return Boolean is
      Position : Guard_Type := Guard;
      Visited_States : array (1 .. 130, 1 .. 130, View_Type) of Boolean := (others => (others => (others => False)));
      Max_Steps : constant Natural := 10000;
      Steps : Natural := 0;
   begin
      Visited_States (Guard.X, Guard.Y, Guard.View_Dir) := True;

      while Steps < Max_Steps loop
         declare
            Next_Position : Guard_Type := Move (Position);
         begin
            if Next_Position.Y = 0 or Next_Position.Y > Map_Size or
               Next_Position.X = 0 or Next_Position.X > Map_Size then
               return False; -- Guard leaves the map
            elsif Map (Next_Position.Y) (Next_Position.X) = '#' then
               Turn_Guard (Position);
            else
               Position := Next_Position;
               if Visited_States (Position.X, Position.Y, Position.View_Dir) then
                  return True;
               end if;
               Visited_States (Position.X, Position.Y, Position.View_Dir) := True;
            end if;
         end;
         Steps := Steps + 1;
      end loop;

      return False;
   end Simulate_Guard_Path;

   function Find_Loop_Positions (Map : in out Map_Type; Map_Size : Natural; Guard : Guard_Type) return Natural is
      Original_Map : Map_Type (1 .. Map_Size) := (others => (others => ' '));
      Loop_Positions : array (1 .. 130, 1 .. 130) of Boolean := (others => (others => False));
      Count : Natural := 0;
   begin
      for I in 1 .. Map_Size loop
         Original_Map (I) := Map (I);
      end loop;

      for Y in 1 .. Map_Size loop
         for X in 1 .. Map_Size loop
            if Map (Y) (X) /= '#' and then (X /= Guard.X or Y /= Guard.Y) then
               Map (Y) (X) := '#';
               if Simulate_Guard_Path (Map, Map_Size, Guard) then
                  Loop_Positions (X, Y) := True;
               end if;
               Map (Y) (X) := Original_Map (Y) (X);
            end if;
         end loop;
      end loop;

      for Y in 1 .. Map_Size loop
         for X in 1 .. Map_Size loop
            if Loop_Positions (X, Y) then
               Count := Count + 1;
            end if;
         end loop;
      end loop;

      return Count;
   end Find_Loop_Positions;

begin
   Open (F, In_File, "input.txt");

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
         View_Idx : Natural := Index (Line, "^");
      begin
         Map_Size := Map_Size + 1;
         Map (Map_Size) (1 .. Line'Length) := Line;

         if View_Idx /= 0 then
            Guard.X := View_Idx;
            Guard.Y := Map_Size;
            Guard.View_Dir := Up;
         end if;
      end;
   end loop;

   Close (F);

   Put_Line ("Number of different positions for the obstruction:" &
               Integer'Image (Find_Loop_Positions (Map, Map_Size, Guard)));
end Main;
