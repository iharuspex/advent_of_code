with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

procedure Main is
   Filename : constant String := "test_mini.txt";
   Map_Size : constant Natural := 7;

   type Point_Type is record
      X, Y : Natural;
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

   type Queue_Element is record
      Prio : Natural;
      Cell_With_Dir : Cell_Dir_Type;
   end record;

   type Map_Type is array (Positive range <>, Positive range <>) of Character;



   package Cell_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Point_Type);

   package Cell_Dir_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => Cell_Dir_Type);

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

   function Get_Priority (Element : Queue_Element) return Natural is
   begin
      return Element.Prio;
   end Get_Priority;
   function Before (Left, Right : Natural) return Boolean is
   begin
      return Left > Right;
   end Before;
   package Cell_Dir_Queues is new Ada.Containers.Synchronized_Queue_Interfaces
     (Element_Type => Queue_Element);
   package Cell_Dir_Priority_Queue is new Ada.Containers.Unbounded_Priority_Queues
     (Queue_Interfaces => Cell_Dir_Queues,
      Queue_Priority => Natural);

   F : File_Type;

   Map : Map_Type (1 .. Map_Size, 1 .. Map_Size);
   Map_Y_Idx : Natural := 0;

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

   ------------
   -- A_Star --
   ------------

   procedure A_Star (M : Map_Type;
                    Start_P, End_P : Point_Type;
                    Path : out Cell_Vectors.Vector;
                    Dirs : out Cell_Dir_Vectors.Vector)
   is
      Frontier : Cell_Dir_Priority_Queue.Queue;
      Came_From : Cell_Dir_Maps.Map;
      Cost_So_Far : Cell_Dir_Maps.Map;
      Current : Point_Type;
      Dir : Dir_Type;
      Priority : Integer;
   begin
      null;
   end A_Star;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Map_Y_Idx := Map_Y_Idx + 1;
         for I in Line'Range loop
            Map (Map_Y_Idx, I) := Line (I);
         end loop;
      end;
   end loop;

   Print_Map (Map);

   Close (F);
end Main;
