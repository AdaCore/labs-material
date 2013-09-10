with Display; use Display;
with Display.Basic; use Display.Basic;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;

procedure Pong is
   package F_Numeri is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use F_Numeri;

   Ball_Size : constant Float := 5.0;

   S : Shape_Id := New_Circle (0.0, 0.0, Ball_Size, Blue);
   Angle : Float := 0.0;


   V : Float := 0.5;
   L1 : Shape_Id := New_Line (-90.0, 0.0, -90.0, 0.0, Red);
   L2 : Shape_Id := New_Line (90.0, 0.0, 90.0, 0.0, Red);

   L3 : Shape_Id := New_Line (-90.0, 90.0, 90.0, 90.0, Red);
   L4 : Shape_Id := New_Line (-90.0, -90.0, 90.0, -90.0, Red);


   P1 : Float := 0.0;
   P2 : Float := 0.0;

   Key : Key_Type;

   Player_Size : Float := 15.0;

   Dx, Dy : Float;
   W : Float := Player_Size + Ball_Size;
begin
   loop
      Dx := V * Cos (Angle);
      Dy := V * Sin (Angle);

      if Dx < 0.0 and then Get_X (S) + Dx < -80.0
        and then Get_Y (S) in P1 - W .. P1 + W
      then
         Angle := Angle + Pi - (Get_Y (S) - P1) / (Player_Size * 5.0) * Pi;
      elsif Dx > 0.0 and then Get_X (S) + Dx > 80.0
        and then Get_Y (S) in P2 - W .. P2 + W
      then
         Angle := Angle + Pi - (Get_Y (S) - P2) / (Player_Size * 5.0) * Pi;
      elsif (Dy < 0.0 and then Get_Y (S) - Dy < -80.0)
        or else (Dy > 0.0 and then Get_Y (S) + Dy > 80.0)
      then
         Dy := -Dy;
         Angle := Arctan (X => Dx, Y => Dy);
      elsif Get_X (S) not in -90.0 .. 90.0 then
         exit;
      else
         Set_X (S, Get_X (S) + Dx);
         Set_Y (S, Get_Y (S) + Dy);
      end if;

      Key := Current_Key_Press;

      case To_Character (Key) is
         when 'q' =>
            P1 := P1 + 3.0;
         when 'a' =>
            P1 := P1 - 3.0;
         when 'o' =>
            P2 := P2 + 3.0;
         when 'l' =>
            P2 := P2 - 3.0;
         when others =>
            null;
      end case;

      Set_Y (L1, P1 - Player_Size);
      Set_Y2 (L1, P1 + Player_Size);

      Set_Y (L2, P2 - Player_Size);
      Set_Y2 (L2, P2 + Player_Size);

      delay 0.01;
   end loop;

   Put_Line ("Game Over");
end Pong;
