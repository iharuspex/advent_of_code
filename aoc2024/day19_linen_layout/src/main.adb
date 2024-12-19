with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Hashed_Maps;

procedure Main is
   --  Filename : constant String := "test.txt";
   Filename : constant String := "input.txt";

   package Pattern_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);

   ----------
   -- Hash --
   ----------

   function Hash(Key : Character) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Character'Pos(Key));
   end Hash;

   package Pattern_Hash_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => Character,
      Element_Type => Pattern_Vectors.Vector,
      Hash => Hash,
      Equivalent_Keys => "=",
      "=" => Pattern_Vectors."=");

   use Pattern_Vectors;
   use Pattern_Hash_Maps;

   F : File_Type;

   Patterns : Map;

   --------------------
   -- Parse_Patterns --
   --------------------

   -- r, wr, b, g, bwu, rb, gb, br

   procedure Parse_Patterns (P : in out Map; Line : String) is
      Comma_Idx1 : Natural := 1;
      Comma_Idx2 : Natural := Index (Line, ",");

      L : Positive := 1;
      R : Positive;

      Cursor : Pattern_Hash_Maps.Cursor;
      Found : Boolean;
   begin
      while Comma_Idx1 /= 0 loop
         --  L := Comma_Idx1;
         R := (if Comma_Idx2 = 0 then Line'Last else Comma_Idx2-1);

         --  Put_Line (Line (L) & ": " & Line (L .. R));
         Cursor := P.Find(Line (L));
         Found := Has_Element(Cursor);

         if Found then
            P.Replace_Element(Cursor, P (Cursor) & Line (L .. R));
         else
            P.Include(Line (L), To_Vector (Line (L .. R), 1));
         end if;

         L := Comma_Idx2+2;
         Comma_Idx1 := Comma_Idx2;
         Comma_Idx2 := Index (Line, ",", Comma_Idx1+1);
      end loop;
   end Parse_Patterns;

   ------------------
   -- Check_Design --
   ------------------

   function Check_Design (Line : String; P : Map) return Boolean is
      Ret : Boolean := False;

      Letter : Character := Line (Line'First);

      L : Positive := Line'First;
      R : Positive;

      Cursor : Pattern_Hash_Maps.Cursor := P.Find(Letter);
      Found : Boolean := Has_Element(Cursor);
   begin
      --  Put_Line ("=======");
      Put_Line ("Current line: " & Line);

      if Found = False then
         return False;
      end if;

      for E of P.Element (Letter) loop
         R := L + E'Length - 1;
         --  Put_Line ("Check " & Line (L .. R));
         --  Put_Line ("Trying " & E);
         if R in Line'Range then
            if Line (L .. R) = E then
               Put_Line (E & " is valid");
               if R+1 in Line'Range then
                  Ret := Check_Design (Line (R+1 .. Line'Last), P);
               else
                  Ret := True;
                  exit;
               end if;
            end if;
         end if;
      end loop;

      return Ret;
   end Check_Design;

   Result : Natural := 0;

begin
   Open (F, In_File, Filename);

   declare
      Line : String := Get_Line (F);
   begin
      Parse_Patterns (Patterns, Line);
   end;

   for C in Patterns.Iterate loop
      Put (Key (C) & ": ");

      for E of Patterns (C) loop
         Put (E & " ");
      end loop;
      New_Line;
   end loop;

   Skip_Line (F);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);

         Check_Res : Boolean := False;
      begin
         Check_Res := Check_Design (Line, Patterns);
         Put (Line & " - ");
         if Check_Res = True then
            Put ("true");
            Result := Result + 1;
         else
            Put ("false");
         end if;
      end;
      New_Line;
   end loop;

   Put_Line ("Part1:" & Result'Image);

   Close (F);
end Main;
