-- CLEANUP NEEDED! >:(

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;

procedure Main is
   package B_Str is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 5);
   use B_Str;

   function Hash is new Ada.Strings.Bounded.Hash (B_Str);

   package Str_Vector is new
     Ada.Containers.Vectors (Positive, Bounded_String);
   use Str_Vector;

   function Vec_Hash (V : Vector) return Ada.Containers.Hash_Type is
      use type Ada.Containers.Hash_Type;
      Ret : Ada.Containers.Hash_Type := 0;
   begin
      for I of V loop
         Ret := Ret xor Hash (I);
      end loop;

      return Ret;
   end Vec_Hash;

   package Cave_Map is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Bounded_String,
        Element_Type    => Vector,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => Str_Vector."=");
   use Cave_Map;

   -- Store Visited caves
   package Cave_Set is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Bounded_String,
        Hash                => Hash,
        Equivalent_Elements => "=");

   -- Store founded paths
   package Path_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Vector,
        Hash                => Vec_Hash,
        Equivalent_Elements => "=",
        "="                 => "=");

   type Visited_Caves is record
      Once: Cave_Set.Set;
      Twice: Bounded_String;
   end record;


   Input_File : File_Type;

   Caves_HM : Map := Empty_Map;

   Result : Path_Sets.Set;

   Start : Bounded_String := To_Bounded_String ("start");


   procedure String_Split (In_Str: in String;
                           Split_By: in String;
                           Part1: out Bounded_String;
                           Part2: out Bounded_String) is
      Delimiter_Idx : Natural := 0;
   begin
      Delimiter_Idx := Index
        (Source => In_Str,
         Pattern => Split_By,
         From => In_Str'First);

      Part1 := To_Bounded_String (In_Str (In_Str'First .. Delimiter_Idx - 1));
      Part2 := To_Bounded_String (In_Str (Delimiter_Idx + 1 .. In_Str'Last));
   end String_Split;


   procedure Print_Map (M: Map) is
   begin
      for C in M.Iterate loop
         Put (To_String (Key (C)) & ": ");
         for I of Element (C) loop
            Put (To_String (I) & ", ");
         end loop;
         New_Line;
      end loop;
   end;


   function Is_Small (Cave: Bounded_String) return Boolean is
     (Element (Cave, 1) in 'a' .. 'z');


   procedure Find_Paths
     (Graph: Map;
      Curr_Cave: Bounded_String;
      Visited: Visited_Caves;
      Path : Vector;
      Result: in out Path_Sets.Set)
   is
      use type Cave_Set.Set;

      Copy : Str_Vector.Vector := Str_Vector.Copy (Path);
   begin
      if Curr_Cave = "end" then
         Result.Include (Path);

         for I of Path loop
            Put (To_String (I));
            Put (",");
         end loop;
         New_Line;

         return;
      end if;
      Copy.Append (Curr_Cave);

      for C of Graph (Curr_Cave) loop
         if not Is_Small (C) then
            Find_Paths (Graph, C, Visited, Copy, Result);
         elsif not Visited.Once.Contains (C) then
            if Visited.Twice = "" then
               Find_Paths (Graph, C, (Visited.Once, C), Copy, Result);
            end if;

            Find_Paths
              (Graph,
               C,
               (Visited.Once or Cave_Set.To_Set (C), Visited.Twice),
               Copy,
               Result);

         end if;
      end loop;
   end Find_Paths;

begin

   Open (Input_File, In_File, "input.txt");

   while not End_Of_File (Input_File) loop

      declare
         Line : String := Get_Line (Input_File);
         A, B : Bounded_String;
      begin
         String_Split (Line, "-", A, B);

         --Put_Line (To_String (A) & " <-> " & To_String (B));

         -- TODO: To separate procedure
         if not Caves_HM.Contains (A) then
            Caves_HM.Include (A, Str_Vector.Empty_Vector);
         end if;

         if not Caves_HM.Contains (B) then
            Caves_HM.Include (B, Str_Vector.Empty_Vector);
         end if;

         Append(Caves_HM (A), B);
         Append(Caves_HM (B), A);
      end;

   end loop;

   Print_Map (Caves_HM);
   New_Line;

   Find_Paths
     (Caves_HM,
      Start,
      (Once => Cave_Set.To_Set (Start), Twice => <>),
      Empty_Vector,
      Result);

   Put_Line ("Result: " & Result.Length'Image);

   Close (Input_File);

end Main;
