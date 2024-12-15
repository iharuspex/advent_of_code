with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Main is
   --  Filename : constant String := "test_mini.txt";
   --  Map_Size : constant Natural := 8;

   --  Filename : constant String := "test.txt";
   --  Map_Size : constant Natural := 10;

   Filename : constant String := "input.txt";
   Map_Size : constant Natural := 50;

   type Move_Dir_Type is (Up, Right, Down, Left);

   type Point_Type is record
      X, Y : Natural;
   end record;

   type Map_Type is array (Positive range <>) of String (1 .. Map_Size);

   package Move_Dir_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Move_Dir_Type);

   F : File_Type;

   Map : Map_Type (1 .. Map_Size);
   Curr_Line : Natural := 0;
   Move_Dirs : Move_Dir_Vectors.Vector;
   Robot_Pos : Point_Type := (0, 0);
   Result_P1 : Natural := 0;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Map_Type) is
   begin
      for I in M'Range loop
         Put_Line (M (I));
      end loop;
   end Print_Map;

   ---------------------
   -- Print_Move_Dirs --
   ---------------------

   procedure Print_Move_Dirs (MD : Move_Dir_Vectors.Vector) is
   begin
      for D of MD loop
         case D is
            when Up =>
               Put ("^");
            when Down =>
               Put ("v");
            when Left =>
               Put ("<");
            when Right =>
               Put (">");
            when others => null;
         end case;
      end loop;
      New_Line;
   end Print_Move_Dirs;

   ---------------------
   -- Print_Robot_Pos --
   ---------------------

   procedure Print_Robot_Pos (R : Point_Type) is
   begin
      Put_Line ("Robot Pos: x =" & R.X'Image & " y =" & R.Y'Image);
   end Print_Robot_Pos;

   ----------------
   -- Dir_To_Pos --
   ----------------

   function Dir_To_Pos (Curr_Pos : Point_Type; Dir : Move_Dir_Type)
                        return Point_Type
   is
      New_Pos : Point_Type;
   begin
      case Dir is
         when Up =>
            New_Pos := (Curr_Pos.X, Curr_Pos.Y-1);
         when Right =>
            New_Pos := (Curr_Pos.X+1, Curr_Pos.Y);
         when Down =>
            New_Pos := (Curr_Pos.X, Curr_Pos.Y+1);
         when Left =>
            New_Pos := (Curr_Pos.X-1, Curr_Pos.Y);
      end case;

      return New_Pos;
   end Dir_To_Pos;

   --------------
   -- Can_Move --
   --------------

   function Can_Move (Map : Map_Type;
                      R_Pos : Point_Type;
                      Dir : Move_Dir_Type)
                      return Boolean
   is
      New_Pos : Point_Type := Dir_To_Pos (R_Pos, Dir);
   begin
      if Map (New_Pos.Y)(New_Pos.X) = '.' then
         return True;
      elsif Map (New_Pos.Y)(New_Pos.X) = 'O' then
         return Can_Move (Map, New_Pos, Dir);
      else
         return False;
      end if;
   end Can_Move;

   ----------
   -- Step --
   ----------

   function Step (M : in out Map_Type;
                  R_Pos : in out Point_Type;
                  Dir : Move_Dir_Type)
                  return Boolean
   is
      Ret : Boolean := True;
      New_Pos : Point_Type := Dir_To_Pos (R_Pos, Dir);

   begin
      if Can_Move (Map, R_Pos, Dir) then
         if Map (New_Pos.Y)(New_Pos.X) = '.' then
            -- just update robot position
            Map (R_Pos.Y)(R_Pos.X) := '.';
            Map (New_Pos.Y)(New_Pos.X) := '@';
            R_Pos := New_Pos;
         elsif Map (New_Pos.Y)(New_Pos.X) = 'O' then

            declare
               Box_Pos : Point_Type := New_Pos;
               Next_Pos : Point_Type;
            begin
               while Map (New_Pos.Y)(New_Pos.X) /= '.' loop
                  Box_Pos := New_Pos;
                  loop
                     Next_Pos := Dir_To_Pos (Box_Pos, Dir);

                     if Map (Next_Pos.Y)(Next_Pos.X) = '.' then
                        Map (Box_Pos.Y)(Box_Pos.X) := '.';
                        Map (Next_Pos.Y)(Next_Pos.X) := 'O';
                        exit;
                     elsif Map (Next_Pos.Y)(Next_Pos.X) = 'O' then
                        Box_Pos := Next_Pos;
                     end if;
                  end loop;
               end loop;

               Map (R_Pos.Y)(R_Pos.X) := '.';
               Map (New_Pos.Y)(New_Pos.X) := '@';
               R_Pos := New_Pos;
            end;
         end if;
      else
         Ret := False;
      end if;

      return Ret;
   end Step;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
         Robot_X_Idx : Natural := Index (Line, "@");
      begin
         if Line /= "" then
            if Line (1) /= '>' and
              Line (1) /= '^' and
              Line (1) /= 'v' and
              Line (1) /= '<' then
               Curr_Line := Curr_Line + 1;
               if Robot_X_Idx /= 0 then
                  Robot_Pos := (Robot_X_Idx, Curr_Line);
               end if;
               Map (Curr_Line) := Line (1 .. Map_Size);
            else
               for I in Line'Range loop
                  case Line (I) is
                     when '^' =>
                        Move_Dirs.Append (Up);
                     when 'v' =>
                        Move_Dirs.Append (Down);
                     when '<' =>
                        Move_Dirs.Append (Left);
                     when '>' =>
                        Move_Dirs.Append (Right);
                     when others => null;
                  end case;
               end loop;
            end if;
         end if;
      end;
   end loop;

   Print_Map (Map);
   Print_Move_Dirs (Move_Dirs);
   Print_Robot_Pos (Robot_Pos);

   for Dir of Move_Dirs loop
      declare
         Status : Boolean := True;
      begin
         Status := Step (Map, Robot_Pos, Dir);
         --  Print_Map (Map);
      end;
   end loop;

   Print_Map (Map);

   for I in Map'Range loop
      for J in Map(I)'Range loop
         if Map (I)(J) = 'O' then
            Result_P1 := Result_P1 + ((100 * (I-1) + (J-1)));
         end if;
      end loop;
   end loop;

   Put_Line ("Part1: " & Result_P1'Image);

   Close (F);
end Main;
