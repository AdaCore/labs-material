package body Data_Lists is

   type Cell_Iterator is new Cell_Iterators_Interface.Forward_Iterator
   with record
      L : access List;
   end record;

   overriding
   function First (Object : Cell_Iterator) return Cell_Access;

   overriding
   function Next (Object : Cell_Iterator; Position : Cell_Access)
                  return Cell_Access;

   ------------
   -- Append --
   ------------

   procedure Append (V : in out List; Val : Data) is
      Tmp     : Cell_Access;
      Created : Cell_Access := new Cell'(Contents => Val, Next => null);
   begin
      if V.First = null then
         V.First := Created;
      else
         Tmp := V.First;

         while Tmp.Next /= null loop
           Tmp := Tmp.Next;
         end loop;

         Tmp.Next := Created;
      end if;
   end Append;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (L : List; C : Cell_Access) return Ref is
   begin
      return Ref'(R => C.Contents'Access);
   end Get_Ref;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (L : List; I : Integer) return Ref is
      C : Cell_Access := L.First;
   begin
      for J in 2 .. I loop
         C := C.Next;
      end loop;

      return Ref'(R => C.Contents'Access);
   end Get_Ref;

   ----------
   -- Tail --
   ----------

   function Tail (L : List) return Ref
   is
      Current : Cell_Access := L.First;
   begin
      while Current.Next /= null loop
         Current := Current.Next;
      end loop;

      return Ref'(R => Current.Contents'Access);
   end Tail;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (L : List; C : Cell_Access) return Data is
   begin
      return C.Contents;
   end Get_Element;

   -----------------
   -- Get_Element --
   -----------------

   function Get_Element (L : List; I : Integer) return Data is
      C : Cell_Access := L.First;
   begin
      for J in 2 .. I loop
         C := C.Next;
      end loop;

      return C.Contents;
   end Get_Element;

   -----------
   -- First --
   -----------

   overriding
   function First (Object : Cell_Iterator) return Cell_Access is
   begin
      return Object.L.First;
   end First;

   ----------
   -- Next --
   ----------

   overriding
   function Next (Object : Cell_Iterator; Position : Cell_Access)
                  return Cell_Access is
   begin
      return Position.Next;
   end Next;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (L : aliased in out List)
      return Cell_Iterators_Interface.Forward_Iterator'Class
   is
   begin
      return Cell_Iterator'(L => L'Unchecked_Access);
   end Iterate;

end Data_Lists;
