with Ada.Unchecked_Deallocation;

package body List is
   
   procedure Insert (Head: in out Link; Element: T) is
      New_Node: Link := new Node'(Value => Element, Next => null);
   begin
      if Head = null then
         Head := New_Node;
         return;
      end if;

      New_Node.Next := Head;
      Head := New_Node;
   end Insert;

   procedure Remove (Head: in out Link) is
      Next: Link;

      procedure Free is new Ada.Unchecked_Deallocation (Object => Node, Name => Link);
   begin
      
      --  Empty List
      if Head = null then
         return;
      end if;

      Next := Head.Next;
      Free (Head);
      Head := Next;
   end Remove;

   function Length (Head: Link) return Integer is
      Node_Count : Integer := 0;
      Current_Node : Link := Head;
   begin
      
      while Current_Node /= null loop
         Node_Count := Node_Count + 1;
         Current_Node := Current_Node.Next;
      end loop;

      return Node_Count;
   end Length;
end List;
