with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   Filename : constant String := "test1.txt";
   Map_Size : constant Natural := 5;

   --  Filename : constant String := "test2.txt";
   --  Map_Size : constant Natural := 10;

   --  Filename : constant String := "input.txt";
   --  Map_Size : constant Natural := 140;

   type Cell is record
      Tag : Character;
      Mark : Boolean;
   end record;

   type Group is record
      Area : Natural;
      Perimeter : Natural;
   end record;

   type Map_Type is array (1 .. Map_Size, 1 .. Map_Size) of Cell;

   package Group_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Group);
   use Group_Vectors;

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
         Put_Line ("Area:" & G.Area'Image & ", Perimeter:" & G.Perimeter'Image);
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
      end if;

      if Check_Coords (M, Current_Tag, Y, X+1) = True then
         if Check_Mark (M, Y, X+1) = True then
            DFS (M, Y, X+1, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
      end if;

      if Check_Coords (M, Current_Tag, Y+1, X) = True then
         if Check_Mark (M, Y+1, X) = True then
            DFS (M, Y+1, X, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
      end if;

      if Check_Coords (M, Current_Tag, Y, X-1) = True then
         if Check_Mark (M, Y, X-1) = True then
            DFS (M, Y, X-1, Gr);
         end if;
      else
         Gr.Perimeter := Gr.Perimeter + 1;
      end if;

   end DFS;

   -------------
   -- Process --
   -------------

   procedure Process (M : in out Map_Type) is
      Result : Natural := 0;

   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            declare
               Gr : Group := (0, 0);
            begin
               DFS (M, I, J, Gr);
               if Gr /= (0, 0) then
                  Groups.Append (Gr);
               end if;
            end;
         end loop;
      end loop;

      Print_Groups (Groups);

      for G of Groups loop
         Result := Result + (G.Area * G.Perimeter);
      end loop;

      Put_Line ("Part1: " & Result'Image);
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
