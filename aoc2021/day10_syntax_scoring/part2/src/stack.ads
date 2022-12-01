package Stack is

   procedure Push (Item: in Character);
   
   function Pop return Character;
   
   function Size return Integer;
   
   procedure Clear;
   
   Stack_Empty : exception;

end Stack;
