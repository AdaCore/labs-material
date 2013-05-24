package body Objects is

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (V : in out Object)
   is
   begin
      V.Size := V.Size + 1.0;
   end Iterate;

end Objects;
