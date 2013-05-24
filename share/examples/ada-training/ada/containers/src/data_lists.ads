with Ada.Containers.Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;

package Data_Lists is

   type Data is record
      A, B : Integer;
   end record;

   type Cell_Access is private;

   type List is tagged private
   with Constant_Indexing => Get_Element,
     Variable_Indexing    => Get_Ref,
     Iterator_Element     => Data,
     Default_Iterator     => Iterate;

   function Get_Element (L : List; C : Cell_Access) return Data;

   function Get_Element (L : List; I : Integer) return Data;

   procedure Append (V : in out List; Val : Data);

   type Ref (R : access Data) is null record
   with Implicit_Dereference => R;

   function Get_Ref (L : List; C : Cell_Access) return Ref;

   function Get_Ref (L : List; I : Integer) return Ref;

   function Tail (L : List) return Ref;

   function Has_Element (Position : Cell_Access) return Boolean;

   package Cell_Iterators_Interface is new Ada.Iterator_Interfaces
     (Cell_Access, Has_Element);

   function Iterate
     (L : aliased in out List)
      return Cell_Iterators_Interface.Forward_Iterator'Class;

private

   type Cell is record
      Contents : aliased Data;
      Next     : access Cell;
   end record;

   type Cell_Access is access all Cell;

   type List is tagged record
      First : Cell_Access;
   end record;

   --  [NOTE] Interestingly, in Ada 2012, it is possible to complete a
   --  function declared in a public spec by an expression-function in the
   --  private part.
   function Has_Element (Position : Cell_Access) return Boolean
   is (Position /= null);

end Data_Lists;
