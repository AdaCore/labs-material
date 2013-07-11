with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   type Integral is record
      Slice : Float;
      Val   : Float;
      Step  : Float;
   end record;

   V : Integral := (1.0, 0.0, 0.1);
begin
   while V.Slice < 2.0 loop
      V.Val := V.Val + 1.0 / Float (V.Slice) * V.Step;
      V.Slice := V.Slice + V.Step;
   end loop;

   Put_Line ("FINAL RESULT = " & V.Val'Img);
end Main;
