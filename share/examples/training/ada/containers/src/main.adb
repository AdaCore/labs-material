with Data_Lists;  use Data_Lists;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   L : List;
   I : Integer := 0;
begin
   Append (L, Data'(0, 1));
   Append (L, Data'(999, 666));
   Append (L, Data'(42, 36));

   --  [NOTE] Using variable indexing to modify values
   L (1).A := 99;
   L (1).B := 100;

   --  [NOTE] Using a simple function returning a user reference to modify
   --  values
   L.Tail.A := 0;
   L.Tail.B := 0;

   Put_Line ("FIRST LOOP");

   for E of L loop
      Put_Line (Integer'Image (E.A) & ", " & Integer'Image (E.B));

      --  [NOTE] Since L ha variable indexing defined, it's possible to change
      --  values of elements in the loop.
      E := (I, I + 1);

      I := I + 2;
   end loop;

   Put_Line ("SECOND LOOP");

   for E of L loop
      Put_Line (Integer'Image (E.A) & ", " & Integer'Image (E.B));
   end loop;

end Main;
