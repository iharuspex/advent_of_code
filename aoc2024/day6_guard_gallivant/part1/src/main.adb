with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

----------
-- Main --
----------

procedure Main is
   -- Types definitions
   type Map_Type is array (1 .. 130) of String (1 .. 130);

   type View_Type is (Up, Right, Down, Left);

   type Guard_Type is record
      X : Natural;
      Y : Natural;
      View_Dir : View_Type;
   end record;

   ---------
   -- "+" --
   ---------

   function "+" (Left : View_Type; Right : Natural) return View_Type
   is
      Ret : View_Type;
      N : Natural :=
        (View_Type'Pos (Left) + Right) mod (View_Type'Pos (View_Type'Last) + 1);
   begin
      Ret := View_Type'Val (N);
      return Ret;
   end "+";

   -- Variables
   F : File_Type;

   Map      : Map_Type := (others => (others => 'W'));
   Map_Size : Natural  := 0;

   Guard : Guard_Type;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map is
   begin
      for I in Map'First .. Map_Size loop
         Put_Line (Map (I) (Map (I)'First .. Map_Size));
      end loop;
   end Print_Map;

   -----------------
   -- Print_Guard --
   -----------------

   procedure Print_Guard (G : Guard_Type) is
   begin
      Put_Line ("===============");
      Put_Line ("X =" & G.X'Image & "; Y =" & G.Y'Image);
      Put_Line ("View dir : " & G.View_Dir'Image);
      Put_Line ("===============");
   end Print_Guard;

   procedure Turn_Guard (G : in out Guard_Type) is
   begin
      G.View_Dir := G.View_Dir + 1;
   end Turn_Guard;

   ----------
   -- Step --
   ----------

   function Step (M : in out Map_Type; G : in out Guard_Type; Res : in out Natural)
                  return Boolean
   is
   begin

      if G.View_Dir = Up then
         if G.Y - 1 in 1..Map_Size then
            if M (G.Y - 1) (G.X) = '.' or M (G.Y - 1) (G.X) = 'X' then
               if M (G.Y - 1) (G.X) = '.' then
                  Res := Res + 1;
               end if;
               -- update map
               M (G.Y) (G.X) := 'X';
               M (G.Y - 1) (G.X) := '^';
               -- update guard
               G.Y := G.Y - 1;
            else
               Turn_Guard (G);
            end if;
         else
            Res := Res + 1;
            return True;
         end if;

      end if;

      if G.View_Dir = Right then
         if G.X + 1 in 1..Map_Size then
           if M (G.Y) (G.X + 1) = '.' or M (G.Y) (G.X + 1) = 'X' then
               if M (G.Y) (G.X + 1) = '.' then
                  Res := Res + 1;
               end if;
               -- update map
               M (G.Y) (G.X) := 'X';
               M (G.Y) (G.X + 1) := '>';
               -- update guard
               G.X := G.X + 1;
            else
               Turn_Guard (G);
            end if;
         else
            Res := Res + 1;
            return True;
         end if;

      end if;

      if G.View_Dir = Down then
         if G.Y + 1 in 1..Map_Size then
            if M (G.Y + 1) (G.X) = '.' or M (G.Y + 1) (G.X) = 'X' then
               if M (G.Y + 1) (G.X) = '.' then
                  Res := Res + 1;
               end if;
               -- update map
               M (G.Y) (G.X) := 'X';
               M (G.Y + 1) (G.X) := 'v';
               -- update guard
               G.Y := G.Y + 1;
            else
               Turn_Guard (G);
            end if;
         else
            Res := Res + 1;
            return True;
         end if;
      end if;

      if G.View_Dir = Left then
         if G.X - 1 in 1..Map_Size then
            if M (G.Y) (G.X - 1) = '.' or M (G.Y) (G.X - 1) = 'X' then
               if M (G.Y) (G.X - 1) = '.' then
                  Res := Res + 1;
               end if;
               -- update map
               M (G.Y) (G.X) := 'X';
               M (G.Y) (G.X - 1) := '<';
               -- update guard
               G.X := G.X - 1;
            else
               Turn_Guard (G);
            end if;
         else
            Res := Res + 1;
            return True;
         end if;
      end if;

      return False;
   end Step;

   ------------
   -- Patrol --
   ------------

   procedure Patrol (M : in out Map_Type; G : in out Guard_Type) is
      Step_Res : Boolean := False;
      Result   : Natural := 0;
   begin
      while Step_Res /= True loop
         Step_Res := Step (Map, G, Result);
         Print_Map;
         Print_Guard (G);
      end loop;

      Put_Line ("Result: " & Result'Image);
   end Patrol;

begin
   Open (F, In_File, "input.txt");

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
         View_Idx : Natural := Index (Line, "^");
      begin
         Map_Size                        := Map_Size + 1;
         Map (Map_Size) (1 .. Line'Last) := Line;

         if View_Idx /= 0 then
            Guard.X := View_Idx;
            Guard.Y := Map_Size;
            Guard.View_Dir := Up;
         end if;
      end;
   end loop;

   Print_Map;
   Print_Guard (Guard);

   Patrol (Map, Guard);

   Close (F);
end Main;
