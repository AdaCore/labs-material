with Display; use Display; 
with Display.Basic; use Display.Basic;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Calendar; use Ada.Calendar;

procedure Main is
   subtype Box_Coord is Integer range 1 .. 20;
   
   type Box is record
      X, Y       : Box_Coord;
      Mine       : Boolean := False;
      Uncovered  : Boolean := False;
      Shape_Box  : Shape_Id := Null_Shape_Id;
      Shape_Mine : Shape_Id := Null_Shape_Id;
      Shape_Text : Shape_Id := Null_Shape_Id;
   end record;
   
   type Board_Type is array (Box_Coord, Box_Coord) of Box;
   
   Board : Board_Type;
   
   P : Mouse_Position;
   Box_Space : constant Float := 100.0 / (Float (Box_Coord'Last) / 2.0);  
   
   Mines_To_Find : Integer := 0;
   Mines_Found   : Integer := 0;
   Total_Mines_Shape : Shape_Id;
   
   procedure Uncover (X, Y : Integer);
   procedure Pressed (X, Y : Integer; Button : Button_Type);
   procedure Reset;
      
   -------------
   -- Pressed --
   -------------
   
   procedure Pressed (X, Y : Integer; Button : Button_Type) is
      Cx, Cy : Float;
   begin
      if Button = Left then
         if not Board (X, Y).Uncovered 
           and then Board (X, Y).Shape_Mine = Null_Shape_Id
         then
            if Board (X, Y).Mine then
               Set_Color (Board (X, Y).Shape_Box, Red);
            else
               Uncover (X, Y);
            end if;
         end if;
      else
         if not Board (X, Y).Uncovered then
            if Board (X, Y).Shape_Mine = Null_Shape_Id then
               Cx := Get_X (Board (X, Y).Shape_Box) + (Box_Space - 1.0) / 2.0;
               Cy := Get_Y (Board (X, Y).Shape_Box) + (Box_Space - 1.0) / 2.0;
                               
               Board (X, Y).Shape_Mine := New_Circle (Cx, Cy, Box_Space * 0.3, Red);   
               Mines_Found := Mines_Found + 1;
            else
               Delete (Board (X, Y).Shape_Mine);
               Mines_Found := Mines_Found - 1;
            end if;
         end if;
      end if;
   end Pressed;
   
   -------------
   -- Uncover --
   -------------
   
   procedure Uncover (X, Y : Integer) is
      Mines : Integer := 0;
      
      procedure Compute (Xc, Yc : Integer) is
      begin
         if Xc in Board'Range (1) 
           and then Yc in Board'Range (2)
         then
            if Board (Xc, Yc).Mine then
               Mines := Mines + 1;            
            end if;
         end if;
      end Compute;
      
      procedure Sub_Uncover (Xc, Yc : Integer) is
      begin
         if Xc in Board'Range (1) 
           and then Yc in Board'Range (2)
         then
            if not Board (Xc, Yc).Uncovered then
               Uncover (Xc, Yc);
            end if;
         end if;
      end Sub_Uncover;
   begin
      Board (X, Y).Uncovered := True;
      Set_Color (Board (X, Y).Shape_Box, White);
      
      Compute (X - 1, Y);
      Compute (X + 1, Y);
      Compute (X, Y - 1);
      Compute (X, Y + 1);
      Compute (X - 1, Y - 1);
      Compute (X + 1, Y + 1);
      Compute (X - 1, Y + 1);
      Compute (X + 1, Y - 1);
      
      if Mines > 0 then
         Board (X, Y).Shape_Text := New_Text
           (Get_X (Board (X, Y).Shape_Box), 
            Get_Y (Board (X, Y).Shape_Box), 
            Trim (Integer'Image (Mines), Both),
            Red);
      else
         Sub_Uncover (X - 1, Y);
         Sub_Uncover (X + 1, Y);
         Sub_Uncover (X, Y - 1);
         Sub_Uncover (X, Y + 1);
         Sub_Uncover (X - 1, Y - 1);
         Sub_Uncover (X + 1, Y + 1);
         Sub_Uncover (X - 1, Y + 1);
         Sub_Uncover (X + 1, Y - 1);
      end if;
   end Uncover;
   
   Seed : Generator;
   
   -----------
   -- Reset --
   -----------
   
   procedure Reset is
   begin
      Mines_To_Find := 0;
      Mines_Found := 0;
      
      for X in Board'Range (1) loop
         for Y in Board'Range (2) loop
            Delete (Board (X, Y).Shape_Box);
            Delete (Board (X, Y).Shape_Mine);
            Delete (Board (X, Y).Shape_Text);
            
            Board (X, Y).X := X;
            Board (X, Y).Y := Y;
            Board (X, Y).Shape_Box := New_Box 
              (Float (X - 1) * Box_Space - 100.0,
               Float (Y - 1) * Box_Space - 100.0,
               Box_Space - 1.0, 
               Box_Space - 1.0,
               Blue);              
         
            Board (X, Y).Mine := Random (Seed) > 0.8;
            Board (X, Y).Uncovered := False;
            
            if Board (X, Y).Mine then
               Mines_To_Find := Mines_To_Find + 1;
            end if;
         end loop;
      end loop;
   end Reset;
      
   K : Key_Type;
begin
   Reset (Seed, Integer (Seconds (Ada.Calendar.Clock)));
   Reset;
   
   Total_Mines_Shape := New_Text (110.0, 80.0, "Mines : 0 / 0", Red);
           
   loop
      P := Read_Last_Mouse_Position;
      K := Read_Last_Key;
      
      if P /= No_Mouse_Position then
         if Integer (P.X) in -100 .. 100 
           and then Integer (P.Y) in -100 .. 100 
         then
            Pressed 
              (Integer 
                 (Float'Ceiling (P.X / 100.0 * Float (Box_Coord'Last) / 2.0)) 
               + Box_Coord'Last / 2, 
               Integer
                 (Float'Ceiling (P.Y / 100.0 * Float (Box_Coord'Last) / 2.0)) 
               + Box_Coord'Last / 2,
               P.Button);
         end if;
      end if;
      
      if To_Character (K) = 'r' then
         Reset;
      end if;
      
      Set_Text
        (Total_Mines_Shape,
         "Mines : " & Mines_Found'Img & " /" & Mines_To_Find'Img);
      
      delay 0.1;
   end loop;
end Main;
