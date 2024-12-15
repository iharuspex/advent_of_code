with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure Main is
   --  Filename : constant String := "test_mini.txt";
   --  Map_Size : constant Natural := 7;

   --  Filename : constant String := "test.txt";
   --  Map_Size : constant Natural := 10;

   Filename : constant String := "input.txt";
   Map_Size : constant Natural := 50;

   type Move_Dir_Type is (Up, Right, Down, Left);

   type Point_Type is record
      X, Y : Natural;
   end record;

   type Box_Pos_Type is record
      Left : Point_Type;
      Right : Point_Type;
   end record;

   type Map_Type is array (Positive range <>) of String (1 .. Map_Size*2);

   package Move_Dir_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Move_Dir_Type);

   F : File_Type;

   Map : Map_Type (1 .. Map_Size);
   Curr_Line : Natural := 0;
   Move_Dirs : Move_Dir_Vectors.Vector;
   Robot_Pos : Point_Type := (0, 0);
   Result_P2 : Natural := 0;

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

   -----------------
   -- Update_Line --
   -----------------

   function Update_Line (Line : String) return String is
      Ret : String (1 .. Line'Last * 2);
      Ret_Idx : Positive := 1;
   begin
      for I in Line'Range loop
         case Line (I) is
            when '#' =>
               Ret (Ret_Idx .. Ret_Idx+1) := "##";
            when '.' =>
               Ret (Ret_Idx .. Ret_Idx+1) := "..";
            when '@' =>
               Ret (Ret_Idx .. Ret_Idx+1) := "@.";
            when 'O' =>
               Ret (Ret_Idx .. Ret_Idx+1) := "[]";
            when others => null;
         end case;
         Ret_Idx := Ret_Idx + 2;
      end loop;

      return Ret;
   end Update_Line;

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

   --------------------
   -- Dir_To_Pos_Box --
   --------------------

   function Dir_To_Pos_Box (Curr_Pos : Box_Pos_Type; Dir : Move_Dir_Type)
                            return Box_Pos_Type
   is
      New_Pos : Box_Pos_Type;
   begin
      case Dir is
         when Up =>
            New_Pos := ((Curr_Pos.Left.X, Curr_Pos.Left.Y-1),
                        (Curr_Pos.Right.X, Curr_Pos.Right.Y-1));
         when Right =>
            New_Pos := ((Curr_Pos.Left.X+1, Curr_Pos.Left.Y),
                        (Curr_Pos.Right.X+1, Curr_Pos.Right.Y));
         when Down =>
            New_Pos := ((Curr_Pos.Left.X, Curr_Pos.Left.Y+1),
                        (Curr_Pos.Right.X, Curr_Pos.Right.Y+1));
         when Left =>
            New_Pos := ((Curr_Pos.Left.X-1, Curr_Pos.Left.Y),
                        (Curr_Pos.Right.X-1, Curr_Pos.Right.Y));
      end case;

      return New_Pos;
   end Dir_To_Pos_Box;

   --------------
   -- Can_Move --
   --------------

   function Can_Move (Map : Map_Type;
                      R_Pos : Point_Type;
                      Dir : Move_Dir_Type)
                      return Boolean
   is
      New_Pos : Point_Type := Dir_To_Pos (R_Pos, Dir);
      Second_Pos : Point_Type;
      Ret : Boolean := False;
   begin
      if Map (New_Pos.Y)(New_Pos.X) = '.' then
         return True;
      elsif Map (New_Pos.Y)(New_Pos.X) = '[' or Map (New_Pos.Y)(New_Pos.X) = ']' then
         if Dir = Up or Dir = Down then
            Second_Pos := New_Pos;

            if Map (New_Pos.Y)(New_Pos.X) = '[' then
               Second_Pos.X := Second_Pos.X + 1;
            else
               Second_Pos.X := Second_Pos.X - 1;
            end if;
            Ret := Can_Move (Map, New_Pos, Dir) and Can_Move (Map, Second_Pos, Dir);
         else
            Ret := Can_Move (Map, New_Pos, Dir);
         end if;
         return Ret;
      else
         return False;
      end if;
   end Can_Move;

   --------------
   -- Move_Box --
   --------------

   procedure Move_Box (M : in out Map_Type;
                       Dir : Move_Dir_Type;
                       Curr_Pos : Box_Pos_Type;
                       New_Pos : Box_Pos_Type) is
   begin
      if Dir = Left then
         M (New_Pos.Left.Y)(New_Pos.Left.X) := '[';
         M (Curr_Pos.Left.Y)(Curr_Pos.Left.X) := '.';
         M (New_Pos.Right.Y)(New_Pos.Right.X) := ']';
         M (Curr_Pos.Right.Y)(Curr_Pos.Right.X) := '.';
      elsif Dir = Right then
         M (New_Pos.Right.Y)(New_Pos.Right.X) := ']';
         M (Curr_Pos.Right.Y)(Curr_Pos.Right.X) := '.';
         M (New_Pos.Left.Y)(New_Pos.Left.X) := '[';
         M (Curr_Pos.Left.Y)(Curr_Pos.Left.X) := '.';
      else
         M (New_Pos.Left.Y)(New_Pos.Left.X) := '[';
         M (Curr_Pos.Left.Y)(Curr_Pos.Left.X) := '.';
         M (New_Pos.Right.Y)(New_Pos.Right.X) := ']';
         M (Curr_Pos.Right.Y)(Curr_Pos.Right.X) := '.';
      end if;

   end Move_Box;

   function Get_Box_Pos_From_Point (Point : Point_Type) return Box_Pos_Type is
      Box_Pos : Box_Pos_Type;
   begin
      if Map (Point.Y)(Point.X) = ']' then
         Box_Pos.Right := Point;
         Box_Pos.Left.Y := Point.Y;
         Box_Pos.Left.X := Point.X-1;
      else
         Box_Pos.Left := Point;
         Box_Pos.Right.Y := Point.Y;
         Box_Pos.Right.X := Point.X+1;
      end if;

      return Box_Pos;
   end Get_Box_Pos_From_Point;

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
         elsif Map (New_Pos.Y)(New_Pos.X) = '[' or Map (New_Pos.Y)(New_Pos.X) = ']' then

            declare
               Box_Pos : Box_Pos_Type;-- := New_Pos;
               Next_Pos : Box_Pos_Type;
               Rly_Can_Move : Boolean := False;
            begin
               while Map (New_Pos.Y)(New_Pos.X) /= '.' loop

                  Box_Pos := Get_Box_Pos_From_Point (New_Pos);

                  loop
                     Next_Pos := Dir_To_Pos_Box (Box_Pos, Dir);

                     if Dir = Left then
                        Rly_Can_Move := Map (Next_Pos.Left.Y)(Next_Pos.Left.X) = '.';
                     elsif Dir = Right then
                        Rly_Can_Move := Map (Next_Pos.Right.Y)(Next_Pos.Right.X) = '.';
                     elsif Dir = Up or Dir = Down then
                        Rly_Can_Move := Map (Next_Pos.Left.Y)(Next_Pos.Left.X) = '.' and Map (Next_Pos.Right.Y)(Next_Pos.Right.X) = '.';
                     end if;

                     if Rly_Can_Move = True then
                        Move_Box (Map, Dir, Box_Pos, Next_Pos);
                        exit;
                     else
                        if Dir = Left then
                           Box_Pos := Get_Box_Pos_From_Point (Next_Pos.Left);
                        elsif Dir = Right then
                           Box_Pos := Get_Box_Pos_From_Point (Next_Pos.Right);
                        else
                           if M (Next_Pos.Left.Y)(Next_Pos.Left.X) /= '.' then
                              Box_Pos := Get_Box_Pos_From_Point (Next_Pos.Left);
                           elsif M (Next_Pos.Right.Y)(Next_Pos.Right.X) /= '.' then
                              Box_Pos := Get_Box_Pos_From_Point (Next_Pos.Right);
                           end if;
                        end if;
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
              Line (1) /= '<'
            then
               Curr_Line := Curr_Line + 1;
               if Robot_X_Idx /= 0 then
                  Robot_Pos := ((Robot_X_Idx*2)-1, Curr_Line);
               end if;
               Map (Curr_Line) := Update_Line (Line);
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
         if Map (I)(J) = '[' then
            Result_P2 := Result_P2 + ((100 * (I-1) + (J-1)));
         end if;
      end loop;
   end loop;

   Put_Line ("Part2: " & Result_P2'Image);

   Close (F);
end Main;
