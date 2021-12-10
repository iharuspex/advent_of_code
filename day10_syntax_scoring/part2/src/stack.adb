with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;

package body Stack is

   package Character_Container is new Doubly_Linked_Lists (Character);
   use Character_Container;
   
   The_Stack : List;
   
   procedure Push (Item: in Character) is
   begin
      The_Stack.Append (Item);
   end Push;
   
   function Pop return Character is
      Result : Character;
   begin
      if The_Stack.Is_Empty then
         raise Stack_Empty;
      end if;
      
      Result := The_Stack.Last_Element;
      The_Stack.Delete_Last;
      
      return Result;
   end Pop;
   
   function Size return Integer is
   begin
      return Integer (The_Stack.Length);
   end Size;
   
   procedure Clear is
   begin
      The_Stack.Clear;
   end Clear;
   

end Stack;
