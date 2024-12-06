with Ada.Text_IO; use Ada.Text_IO;

----------
-- Main --
----------

procedure Main is
   F : File_Type;

   type Map_Type is array (1 .. 130) of String (1 .. 130);

   Map      : Map_Type := (others => (others => 'W'));
   Map_Size : Natural  := 0;

   Step_Res : Boolean := False;
   Result   : Natural := 0;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map is
   begin
      for I in Map'First .. Map_Size loop
         Put_Line (Map (I) (Map (I)'First .. Map_Size));
      end loop;
   end Print_Map;

   ----------
   -- Step --
   ----------

   function Step (M : in out Map_Type; Res : in out Natural) return Boolean is
   begin
      null;
      return True;
   end Step;

begin
   Open (F, In_File, "test.txt");

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         Map_Size                        := Map_Size + 1;
         Map (Map_Size) (1 .. Line'Last) := Line;
      end;
   end loop;

   Print_Map;

   while Step_Res /= True loop
      Step_Res := Step (Map, Result);
   end loop;

   Put_Line ("Result: " & Result'Image);

   Close (F);
end Main;
