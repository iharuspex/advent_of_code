with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Main is
   --  Filename : constant String := "test_mini.txt";
   --  Map_Size : constant Natural := 4;

   --  Filename : constant String := "test1.txt";
   --  Map_Size : constant Natural := 5;

   --  Filename : constant String := "test2.txt";
   --  Map_Size : constant Natural := 10;

   Filename : constant String := "input.txt";
   Map_Size : constant Natural := 140;

   type Dir_Type is (Up, Down, Left, Right);
   type Side_Type is record
      C1  : Natural;
      C2  : Natural;
      Dir : Dir_Type;
   end record;

   --  package Side_Type_Hash is new Ada.Containers.Hash_Type(Side_Type);

   ----------
   -- Hash --
   ----------

   function Hash(Element : Side_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Natural(Element.C1) + Natural(Element.C2) + Dir_Type'Pos(Element.Dir));
   end Hash;

   -------------------------
   -- Equivalent_Elements --
   -------------------------

   function Equivalent_Elements(Left, Right : Side_Type) return Boolean is
   begin
      return Left.C1 = Right.C1 and Left.C2 = Right.C2 and Left.Dir = Right.Dir;
   end Equivalent_Elements;

   package Side_Type_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type => Side_Type,
      Hash => Hash,
      Equivalent_Elements => Equivalent_Elements,
      "=" => Equivalent_Elements);
   use Side_Type_Sets;

   type Cell is record
      Tag : Character;
      Mark : Boolean;
   end record;

   type Group is record
      Area : Natural := 0;
      Perimeter : Natural := 0;
      Sides : Side_Type_Sets.Set;
   end record;

   package Group_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Group);
   use Group_Vectors;

   type Map_Type is array (1 .. Map_Size, 1 .. Map_Size) of Cell;

   F : File_Type;

   Map : Map_Type := (others => (others => ('.', False)));
   Lines_Num : Positive := 1;

   Groups : Vector;

   Result : Natural := 0;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Map_Type) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            Put (M (I, J).Tag);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   ------------------
   -- Print_Groups --
   ------------------

   procedure Print_Groups (Gr : Vector) is
   begin
      for G of Gr loop
         Put_Line ("Area:" & G.Area'Image & ", Perimeter:" & G.Perimeter'Image &
                  ", Sides:" & G.Sides.Length'Image);
      end loop;
   end Print_Groups;

   ------------------
   -- Check_Coords --
   ------------------

   function Check_Coords (M : Map_Type; Tag : Character; Y, X : Natural)
                          return Boolean
   is
      Ret : Boolean := False;
   begin
      if (X in M'Range(2)) and (Y in M'Range(1)) then
         if M (Y,X).Tag = Tag then
            Ret := True;
         end if;
      end if;

      return Ret;
   end Check_Coords;

   ---------------
   -- Check_Mark --
   ---------------

   function Check_Mark (M : Map_Type; Y, X : Natural)
                       return Boolean
   is
      Ret : Boolean := False;
   begin
      if (X in M'Range(2)) and (Y in M'Range(1)) then
         if M (Y,X).Mark = False then
            Ret := True;
         end if;
      end if;

      return Ret;
   end Check_Mark;

   ---------
   -- DFS --
   ---------

   procedure DFS (M: in out Map_Type; Y, X : Natural; Gr : in out Group) is
      Current_Tag : Character := M (Y,X).Tag;
      Side : Side_Type;
   begin
      -- look around and go to the next sector
      -- check coords:
      --  - Map (Y-1, X)
      --  - Map (Y, X+1)
      --  - Map (Y+1, X)
      --  - Map (Y, X-1)
      --
      --  --------------------------------------------------
      --  | -------------- | Map (Y-1, X) | -------------- |
      --  --------------------------------------------------
      --  | Map (Y, X-1)   | Map (Y, X)   | Map (Y, X+1)   |
      --  --------------------------------------------------
      --  | -------------- | Map (Y+1, X) | -------------- |
      --  --------------------------------------------------
      --

      -- If visited
      if M (Y,X).Mark = True then
         return;
      end if;


      if X in M'Range(1) and Y in M'Range(2) then
         M (Y,X).Mark := True;
      end if;

      Gr.Area := Gr.Area + 1;

      if Check_Coords (M, Current_Tag, Y-1, X) = True then
         if Check_Mark (M, Y-1, X) = True then
            DFS (M, Y-1, X, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
         Side.C1 := Y;
         Side.C2 := Y - 1;
         Side.Dir := Up;
         Gr.Sides.Include (Side);
      end if;

      if Check_Coords (M, Current_Tag, Y, X+1) = True then
         if Check_Mark (M, Y, X+1) = True then
            DFS (M, Y, X+1, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
         Side.C1 := X;
         Side.C2 := X + 1;
         Side.Dir := Right;
         Gr.Sides.Include (Side);
      end if;

      if Check_Coords (M, Current_Tag, Y+1, X) = True then
         if Check_Mark (M, Y+1, X) = True then
            DFS (M, Y+1, X, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
         Side.C1 := Y;
         Side.C2 := Y + 1;
         Side.Dir := Down;
         Gr.Sides.Include (Side);
      end if;

      if Check_Coords (M, Current_Tag, Y, X-1) = True then
         if Check_Mark (M, Y, X-1) = True then
            DFS (M, Y, X-1, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
         Side.C1 := X;
         Side.C2 := X - 1;
         Side.Dir := Left;
         Gr.Sides.Include (Side);
      end if;

   end DFS;

   -------------
   -- Process --
   -------------

   procedure Process (M : in out Map_Type) is
      Result_P1 : Natural := 0;
      Result_P2 : Natural := 0;

   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            declare
               Gr : Group;
            begin
               DFS (M, I, J, Gr);
               if Gr.Area /= 0 and Gr.Perimeter /= 0 then
                  Groups.Append (Gr);
               end if;
            end;
         end loop;
      end loop;

      Print_Groups (Groups);

      for G of Groups loop
         Result_P1 := Result_P1 + (G.Area * G.Perimeter);
         Result_P2 := Result_P2 + (G.Area * Natural(G.Sides.Length));
      end loop;

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
            Map (Lines_Num, I).Tag := Line (I);
         end loop;
         Lines_Num := Lines_Num + 1;
      end;
   end loop;

   Print_Map (Map);
   New_Line;

   Process (Map);


end Main;
