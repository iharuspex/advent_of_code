with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Main is
   --  Filename : constant String := "test.txt";
   --  Map_Width : constant Natural := 11;
   --  Map_Height : constant Natural := 7;

   Filename : constant String := "input.txt";
   Map_Width : constant Natural := 101;
   Map_Height : constant Natural := 103;

   type Point_Type is record
      X : Integer;
      Y : Integer;
   end record;

   type Robot_Type is record
      P : Point_Type;
      V : Point_Type;
   end record;

   package Robot_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Robot_Type);

   type Map_Type is array (Natural range <>, Natural range <>) of Natural;

   F : File_Type;

   Map : Map_Type (0 .. Map_Height-1, 0 .. Map_Width-1) :=
     (others => (others => 0));
   Robots : Robot_Vectors.Vector;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Map_Type) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            if M (I, J) = 0 then
               Put (".");
            elsif M (I, J) = 7999 then
               Put (" ");
            else
               Put (Item => M (I, J), Width => 1);
            end if;
         end loop;
         New_Line;
      end loop;
   end Print_Map;

   ------------------
   -- Print_Robots --
   ------------------

   procedure Print_Robots (Robots : Robot_Vectors.Vector) is
   begin
      for R of Robots loop
         Put ("p = ");
         Put (Item => R.P.X, Width => 1);
         Put (",");
         Put (Item => R.P.Y, Width => 1);
         Put (" v = ");
         Put (Item => R.V.X, Width => 1);
         Put (",");
         Put (Item => R.V.Y, Width => 1);
         New_Line;
      end loop;
   end Print_Robots;

   ------------------
   -- Place_To_Map --
   ------------------

   procedure Place_To_Map (M : in out Map_Type; Robots : Robot_Vectors.Vector) is
   begin
      M := (others => (others => 0));

      for R of Robots loop
         if M (R.P.Y, R.P.X) = 0 then
            M (R.P.Y, R.P.X) := 1;
         else
            M (R.P.Y, R.P.X) := M (R.P.Y, R.P.X) + 1;
         end if;
      end loop;
   end Place_To_Map;

   ------------------------
   -- Parse_Robot_Params --
   ------------------------

   function Parse_Robot_Params (Line : String) return Robot_Type
   is
      Ret : Robot_Type := (others => (others => 0));

      Space_Idx : Natural := Index (Line, " ");

      P_Str : String := Line (Line'First+2 .. Space_Idx-1);
      V_Str : String := Line (Space_Idx+3 .. Line'Last);

      procedure Parse_Coords (Str : String; Target : in out Point_Type) is
         Comma_Idx : Natural := Index (Str, ",");
         X_Str : String := Str (Str'First .. Comma_Idx-1);
         Y_Str : String := Str (Comma_Idx+1 .. Str'Last);
      begin
         Target.X := Integer'Value (X_Str);
         Target.Y := Integer'Value (Y_Str);
      end Parse_Coords;

   begin
      --  Put_Line (P_Str);
      --  Put_Line (V_Str);

      Parse_Coords (P_Str, Ret.P);
      Parse_Coords (V_Str, Ret.V);

      return Ret;
   end Parse_Robot_Params;

   ----------
   -- Move --
   ----------

   procedure Move (M : Map_Type; Robot : in out Robot_Type) is
      Mov_X : Integer := Robot.P.X + Robot.V.X;
      Mov_Y : Integer := Robot.P.Y + Robot.V.Y;
   begin
      if Mov_X in M'Range (2) then
         Robot.P.X := Mov_X;
      else
         Robot.P.X := abs(M'Length(2) - abs(Mov_X));
      end if;

      if Mov_Y in M'Range (1) then
         Robot.P.Y := Mov_Y;
      else
         Robot.P.Y := abs(M'Length(1) - abs(Mov_Y));
      end if;
   end Move;

   -----------
   -- Part1 --
   -----------

   procedure Part1 (M: Map_Type) is
      P1_Map : Map_Type := M;

      Q1, Q2, Q3, Q4 : Natural := 0;
      Result_P1 : Natural := 0;
   begin
      Put_Line ("Quadrants:");
      for I in P1_Map'Range(1) loop
         P1_Map (I, Map_Width / 2) := 7999;
      end loop;

      for I in P1_Map'Range(2) loop
         P1_Map (Map_Height / 2, I) := 7999;
      end loop;

      -- calc result

      for I in P1_Map'First(1) .. (Map_Height / 2) - 1 loop
         -- Q1
         for J in P1_Map'First(2) .. (Map_Width / 2) - 1 loop
            Q1 := Q1 + P1_Map (I, J);
         end loop;

         -- Q2
         for J in (Map_Width / 2) + 1 .. P1_Map'Last(2) loop
            Q2 := Q2 + P1_Map (I, J);
         end loop;
      end loop;



      for I in (Map_Height / 2) + 1 .. P1_Map'Last(1) loop
         -- Q3
         for J in P1_Map'First(2) .. (Map_Width / 2) - 1 loop
            Q3 := Q3 + P1_Map (I, J);
         end loop;

         -- Q4
         for J in (Map_Width / 2) + 1 .. P1_Map'Last(2) loop
            Q4 := Q4 + P1_Map (I, J);
         end loop;
      end loop;

      Print_Map (P1_Map);

      Result_P1 := Q1 * Q2 * Q3 * Q4;
      Put_Line ("Part1: " & Result_P1'Image);
   end Part1;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);

         Robot : Robot_Type;
      begin
         Robot := Parse_Robot_Params (Line);
         Robots.Append (Robot);
      end;
   end loop;

   Print_Robots (Robots);
   New_Line;
   Put_Line ("Initial state:");
   Place_To_Map (Map, Robots);
   Print_Map (Map);
   New_Line;

   for I in 1 .. 10000 loop
      for R of Robots loop
         Move (Map, R);
      end loop;

      if I = 100 then
         Place_To_Map (Map, Robots);
         Part1 (Map);
      end if;

      if I = 6876 then
         Place_To_Map (Map, Robots);
         Print_Map (Map);
         Put_Line ("Part2: " & I'Image & " =))");
      end if;
   end loop;

   Close (F);
end Main;
