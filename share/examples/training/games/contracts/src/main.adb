with Display; use Display; 
with Display.Basic; use Display.Basic;
with Objects;
use Objects;


procedure Main is
   X : Objects.Object;
   
   Ball : Shape_Id := New_Circle 
     (X      => 0.0,
      Y      => 0.0,
      Radius => 10.0,
      Color  => Red);
   
   Text : Shape_Id := New_Text (-90.0, 90.0, "", White);
   
   type Shape_Array is array (Integer range <>) of Shape_Id;
   
   Lines : constant Shape_Array (1 .. 4) := 
     (New_Line (-100.0, -100.0, 100.0, -100.0, Blue),
      New_Line (-100.0, -100.0, -100.0, 100.0, Blue),
      New_Line (100.0, 100.0, 100.0, -100.0, Blue),
      New_Line (100.0, 100.0, -100.0, 100.0, Blue));
   
   Tail : Shape_Array (0 .. 99) := 
     (others => New_Circle (0.0, 0.0, 2.0, Magenta));
   Current_Tail : Integer := 0;
         
   Dx, Dy : Float := 0.0;
   
   Last_Key : Key_Type;
   
   Cycle : Long_Long_Integer := 0;
   
   type R is record
      A, B : Integer;
   end record
     with Dynamic_Predicate => R.A <= R.B;

   V : R := (A => 2, B => 1);

begin
   loop
      Iterate (X);
      Cycle := Cycle + 1;
      Last_Key := Current_Key_Press;
      
      
      
      Set_Text 
        (Text, "Dx = " & Float'Image (Dx) & ", Dy = " & Float'Image (Dy));
      
      if Get_X (Ball) + Dx in -90.0 .. 90.0 then      
         Set_X (Ball, Get_X (Ball) + Dx);
      else
         Dx := -Dx;
      end if;
      
      if Get_Y (Ball) + Dy in -90.0 .. 90.0 then      
         Set_Y (Ball, Get_Y (Ball) + Dy);
      else
         Dy := -Dy;
      end if;     

      if Cycle mod 5 = 0 then
         Current_Tail := (Current_Tail + 1) mod Tail'Length;
         
         Set_X (Tail (Current_Tail), Get_X (Ball));
         Set_Y (Tail (Current_Tail), Get_Y (Ball));
      end if;
      
      delay 0.01;
   end loop;   
end Main;

