with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   Filename : constant String := "test.txt";
   Map_Size : constant Positive := 8;

   --  Filename : constant String := "input.txt";
   --  Map_Size : constant Positive := 55;

   -- types definitions
   type Position_Type is record
      Height : Natural;
      Mark   : Boolean;
   end record;

   type Topo_Map is array (1 .. Map_Size, 1 .. Map_Size) of Position_Type;

   --  variables
   F : File_Type;
   Map : Topo_Map := (others => (others => (0, False)));
   Map_Line : Positive := 1;

   ---------------
   -- Print_Map --
   ---------------

   procedure Print_Map (M : Topo_Map) is
   begin
      for I in M'Range(1) loop
         for J in M'Range(2) loop
            Put (Item => M (I, J).Height, Width => 1);
            Put (" ");
         end loop;
         New_Line;
      end loop;
   end Print_Map;

begin
   Open (F, In_File, Filename);

   while not End_Of_File (F) loop
      declare
         Line : String := Get_Line (F);
      begin
         for I in Line'Range loop
            Map (Map_Line, I).Height := Natural'Value (Line (I .. I));
         end loop;
         Map_Line := Map_Line + 1;
      end;
   end loop;

   Print_Map (Map);

   Close (F);

end Main;
