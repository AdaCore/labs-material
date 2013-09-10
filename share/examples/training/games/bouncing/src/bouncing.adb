with Display; use Display;
with Display.Basic; use Display.Basic;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

procedure Bouncing is
   Seed : Generator;
   Buffer_Size : constant := 1_200;

   package F_Numeri is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use F_Numeri;

   Base_Immunity : constant := 500;

   type Ball is record
      X, Y     : Float := 0.0;
      Dx, Dy   : Float := 0.0;
      Size     : Float := 0.0;
      Mass     : Float := 1.0;
      S        : Shape_Id := Null_Shape_Id;
      Immunity : Integer := Base_Immunity;
   end record
   with Dynamic_Predicate => Ball.Mass > 0.0;

   type Shape_Array_Type is array (Integer range <>) of Shape_Id;
   type Ball_Array_Type is array (Integer range <>) of Ball;

   Null_Ball : constant Ball := Ball'(X      => 0.0,
                                      Y      => 0.0,
                                      Dx     => 0.0,
                                      Dy     => 0.0,
                                      Size   => 0.0,
                                      Mass   => 1.0,
                                      S      => Null_Shape_Id,
                                      Immunity => Base_Immunity);
   Total_Ball : Integer := 0;

   function Speed (B : Ball) return Float is
     (Sqrt (B.Dx * B.Dx + B.Dy * B.Dy));

   function Cynetic_Energy (B : Ball) return Float is
     (1.0 / 2.0 * B.Mass * Speed (B) * Speed (B));

   function Speed (Cy : Float; Mass : Float) return Float is
     (Sqrt (Cy * 2.0 / Mass));


   procedure Create_Ball
     (X : Float; Y : Float; Mass : Float; Velocity : Float; Balls : in out Ball_Array_Type; J : Integer);


   type Int_Array is array (Integer range <>) of Integer range 0 .. Buffer_Size;
   type Bool_Array is array (Integer range <>) of Boolean;

   protected Collision_Manager is
      procedure Reset;
      procedure Set_Collision (J, K : Integer);
      procedure Keep_Immunity (J : Integer);
      function Collision_With (J : Integer) return Integer;
      function Should_Keep_Immunity (J : Integer) return Boolean;
   private
      Collision_Vector : Int_Array (1 .. Buffer_Size);
      Immunity_Vector : Bool_Array (1 .. Buffer_Size);
   end Collision_Manager;

   protected body Collision_Manager is
      procedure Reset is
      begin
         Collision_Vector := (others => 0);
         Immunity_Vector := (others => False);
      end Reset;

      procedure Set_Collision (J, K : Integer) is
      begin
         Collision_Vector (J) := K;
      end Set_Collision;

      procedure Keep_Immunity (J : Integer) is
      begin
         Immunity_Vector (J) := True;
      end Keep_Immunity;

      function Collision_With (J : Integer) return Integer is
      begin
         return Collision_Vector (J);
      end Collision_With;

      function Should_Keep_Immunity (J : Integer) return Boolean is
      begin
         return Immunity_Vector (J);
      end Should_Keep_Immunity;

   end Collision_Manager;


   procedure Create_Graphic (B : in out Ball) is
   begin
      B.S := New_Circle
        (B.X, B.Y, B.Size,
         (if B.Size > 15.0 then Blue
          elsif B.Size > 10.0 then Green
          elsif B.Size > 5.0 then Yellow
          elsif B.Size > 2.0 then Magenta
          else Red));
   end Create_Graphic;

   function Collision (B1, B2 : Ball) return Boolean is
      Dx, Dy : Float;
      Size : Float;
   begin
      if B1 = Null_Ball or else B2 = Null_Ball then
         return False;
      end if;

      Dx := B1.X - B2.X;
      Dy := B1.Y - B2.Y;
      Size := B1.Size + B2.Size;

      return Dx * Dx + Dy * Dy <= Size * Size;
   end Collision;

   procedure Bounce (B1, B2 : in out Ball) is
      Dx : Float;
      Dy : Float;
      Length : Float;
      Dvx : Float;
      Dvy : Float;
      Impulse : Float;
   begin
      Dx := B1.X - B2.X;
      Dy := B1.Y - B2.Y;
      Length := Sqrt (Dx * Dx + Dy * Dy);

      if Length /= 0.0 then
         Dx := Dx / Length;
         Dy := Dy / Length;
         Dvx := B1.Dx - B2.Dx;
         Dvy := B1.Dy - B2.Dy;
         Impulse := -2.0 *  (Dx * Dvx + Dy * Dvy);
         Impulse := Impulse / (1.0 / B1.Mass + 1.0 / B2.Mass);

         B1.Dx := B1.Dx + Dx * (Impulse / B1.Mass);
         B1.Dy := B1.Dy + Dy * (Impulse / B1.Mass);

         B2.Dx := B2.Dx - Dx * (Impulse / B2.Mass);
         B2.Dy := B2.Dy - Dy * (Impulse / B2.Mass);
      end if;
   end Bounce;

   procedure Explode (Balls : in out Ball_Array_Type; Index : Integer) is
      B : Ball := Balls (Index);
      V : Float;
      Cy : Float;
      Sub_Particles : Integer;
   begin
      Sub_Particles := Integer (Log (X => B.Mass, Base => 2.0) + 1.0);

      Total_Ball := Total_Ball - 1;
      V := Sqrt (B.Dx * B.Dx + B.Dy * B.Dy);
      Cy := 1.0 / 2.0 * B.Mass * V * V;

      Delete (Balls (Index).S);
      Balls (Index) := Null_Ball;

      for J in 1 .. Sub_Particles loop
         for K in Balls'Range loop
            if Balls (K) = Null_Ball then
               Create_Ball
                 (X       => B.X + Random (Seed) * B.Size - B.Size / 2.0,
                  Y       => B.Y + Random (Seed) * B.Size - B.Size / 2.0,
                  Mass     => B.Mass / Float (Sub_Particles),
                  Velocity => Speed
                    (Cy / Float (Sub_Particles),
                     B.Mass / Float (Sub_Particles)),
                  Balls => Balls,
                  J => K);

               Create_Graphic (Balls (K));

               exit;
            end if;
         end loop;
      end loop;
   end Explode;

   procedure Combine (Balls : in out Ball_Array_Type; J, K : Integer) is
      B1 : Ball := Balls (J);
      B2 : Ball := Balls (K);
      Cy : Float := Cynetic_Energy (B1) + Cynetic_Energy (B2);
   begin
      Total_Ball := Total_Ball - 2;

      Delete (Balls (K).S);
      Balls (K) := Null_Ball;

      Delete (Balls (J).S);

      Create_Ball
        (B1.X + (B1.X - B2.X) / 2.0,
         B1.Y + (B1.Y - B2.Y) / 2.0,
         B1.Mass + B2.Mass,
         Speed (Cy, B1.Mass + B2.Mass),
         Balls,
         J);

      Create_Graphic (Balls (J));
   end Combine;

   procedure Create_Ball
     (X : Float; Y : Float; Mass : Float;
      Velocity : Float; Balls : in out Ball_Array_Type; J : Integer)
   is
      B : Ball renames Balls (J);
      Angle : Float := Random (Seed) * 2.0 * Pi;
   begin
      Total_Ball := Total_Ball + 1;
      B.X := X;
      B.Y := Y;


      B.Dx := Cos (Angle) * Velocity;
      B.Dy := Sin (Angle) * Velocity;

      B.Mass := Mass;
      B.Size := Sqrt (B.Mass);
      B.Immunity := Base_Immunity;
      Collision_Manager.Keep_Immunity (J);
   end Create_Ball;


    Lines : constant Shape_Array_Type (1 .. 4) :=
     (New_Line (-100.0, -100.0, 100.0, -100.0, Blue),
      New_Line (-100.0, -100.0, -100.0, 100.0, Blue),
      New_Line (100.0, 100.0, 100.0, -100.0, Blue),
      New_Line (100.0, 100.0, -100.0, 100.0, Blue));

   R : Float;

   Balls_Txt : Shape_Id := New_Text (110.0, 90.0, "0", White);
   Explode_Txt : Shape_Id := New_Text (110.0, 80.0, "0", White);
   Combine_Txt : Shape_Id := New_Text (110.0, 70.0, "0", White);

   Combine_Prob : Float := 0.04;
   Explode_Prob : Float := 0.02;

   Ball_Array : Ball_Array_Type (1 .. Buffer_Size) := (others => Null_Ball);


   task type Collision_Detection (Size, Modulus, Ind : Integer) is
      entry Compute;
      entry Finished;
      entry Stop;
   end Collision_Detection;

   task body Collision_Detection is
      Do_Work : Boolean := True;
      J : Integer;
   begin
      while Do_Work loop
         select
            accept Compute;

            J := Ind;

            if J = 0 then
               J := J + Modulus;
            end if;

            while J <= Size loop
               if Ball_Array (J) /= Null_Ball then
                  for K in J + 1 .. Ball_Array'Last loop
                     if Collision (Ball_Array (J), Ball_Array (K)) then
                        if Ball_Array (J).Immunity = 0
                          and then Ball_Array (K).Immunity = 0
                        then
                           Collision_Manager.Set_Collision (J, K);
                        end if;

                        Collision_Manager.Keep_Immunity (J);
                        Collision_Manager.Keep_Immunity (K);
                     end if;
                  end loop;
               end if;

               J := J + Modulus;
            end loop;

            accept Finished;
         or
            accept Stop;

            Do_Work := False;
         end select;
      end loop;
   end Collision_Detection;

   D1 : Collision_Detection (1200, 4, 0);
   D2 : Collision_Detection (1200, 4, 1);
   D3 : Collision_Detection (1200, 4, 2);
   D4 : Collision_Detection (1200, 4, 3);
begin
   for J in 1 .. 20 loop
      declare
         B : Ball renames Ball_Array (J);
      begin
         Create_Ball (0.0, 0.0, Random (Seed) * 75.0 + 4.0, 0.5, Ball_Array, J);
         Create_Graphic (B);
      end;
   end loop;

   loop
      Collision_Manager.Reset;

      D1.Compute;
      D2.Compute;
      D3.Compute;
      D4.Compute;

      D1.Finished;
      D2.Finished;
      D3.Finished;
      D4.Finished;

      for J in Ball_Array'Range loop
         if Ball_Array (J) /= Null_Ball then
            declare
               K : Integer;
            begin
               if not Collision_Manager.Should_Keep_Immunity (J) then
                  Ball_Array (J).Immunity := 0;
               end if;

               K := Collision_Manager.Collision_With (J);

               if K /= 0 and then Ball_Array (K) /= Null_Ball then
                  R := Random (Seed);

                  if R in 1.0 - Explode_Prob - Combine_Prob
                    .. 1.0 - Combine_Prob
                  then
                     if Ball_Array (J).Mass > Ball_Array (K).Mass then
                        Explode (Ball_Array, J);
                     else
                        Explode (Ball_Array, K);
                     end if;
                  elsif R in 1.0 - Combine_Prob .. 1.0 then
                     Combine (Ball_Array, J, K);
                  else
                     Bounce (Ball_Array (J), Ball_Array (K));
                  end if;
               end if;
            end;
         end if;
      end loop;

      for J in Ball_Array'Range loop
         declare
            B : Ball renames Ball_Array (J);
         begin
            if B /= Null_Ball then
               if (B.X - B.Size < -100.0 and then B.Dx < 0.0)
                 or else (B.X + B.Size > 100.0 and then B.Dx > 0.0)
               then
                  B.Dx := -B.Dx;
               end if;

               if (B.Y - B.Size< -100.0 and then B.Dy < 0.0)
                 or else (B.Y + B.Size> 100.0 and then B.Dy > 0.0)
               then
                  B.Dy := -B.Dy;
               end if;

               B.X := B.X + B.Dx;
               B.Y := B.Y + B.Dy;

               if B.Immunity > 0 then
                  B.Immunity := B.Immunity - 1;
               end if;

               Set_X (B.S, B.X);
               Set_Y (B.S, B.Y);
            end if;
         end;
      end loop;

      Set_Text (Balls_Txt, "Balls:" & Total_Ball'Img);
      Set_Text (Explode_Txt, "Explode Prob:" & Integer (Explode_Prob * 1000.0)'Img & " / 1000");
      Set_Text (Combine_Txt, "Combine Prob:" & Integer (Combine_Prob * 1000.0)'Img & " / 1000");

      declare
         Last_Key : Key_Type := Current_Key_Press;
      begin
         if To_Character (Last_Key) = 'q' then
            Explode_Prob := Explode_Prob - 0.001;
         elsif To_Character (Last_Key) = 'w' then
            Explode_Prob := Explode_Prob + 0.001;
         elsif To_Character (Last_Key) = 'a' then
            Combine_Prob := Combine_Prob - 0.001;
         elsif To_Character (Last_Key) = 's' then
            Combine_Prob := Combine_Prob + 0.001;
         end if;
      end;

      delay 0.01;
   end loop;
end Bouncing;
