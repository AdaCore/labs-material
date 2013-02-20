-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2009, AdaCore                  --
--                                                                   --
-- Labs is free  software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Display.Basic is

   -----------
   -- Set_X --
   -----------

   procedure Set_X (Shape : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_X, Natural (Shape), Value));
   end Set_X;

   -----------
   -- Get_X --
   -----------

   function Get_X (Shape : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (Shape)).X;
   end Get_X;

   -----------
   -- Set_Y --
   -----------

   procedure Set_Y (Shape : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_Y, Natural (Shape), Value));
   end Set_Y;

   -----------
   -- Get_Y --
   -----------

   function Get_Y (Shape : Shape_Id) return Float is
   begin
       return Data_Manager.Get_Data (Natural (Shape)).Y;
   end Get_Y;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Shape : in out Shape_Id; Color : Color_Type) is
   begin
     Data_Manager.Schedule_Command ((Set_Color, Natural (Shape), Color));
   end Set_Color;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Shape : Shape_Id) return Color_Type is
   begin
      return Data_Manager.Get_Data (Natural (Shape)).Color;
   end Get_Color;

   ------------
   -- Delete --
   ------------

   procedure Delete (Shape : in out Shape_Id) is
   begin
      if Shape /= Null_Shape_Id then
         Data_Manager.Schedule_Command ((Delete, Natural (Shape)));
         Shape := Null_Shape_Id;
      end if;
   end Delete;

   ----------------
   -- New_Circle --
   ----------------

   function New_Circle
     (X      : Float;
      Y      : Float;
      Radius : Float;
      Color  : Color_Type)
      return Shape_Id
   is
      The_Circle : Shape_Data (Circle);
   begin
      The_Circle.X := X;
      The_Circle.Y := Y;
      The_Circle.Radius := Radius;
      The_Circle.Color := Color;

      Data_Manager.Take_Id (The_Circle.Index);
      Data_Manager.Schedule_Command ((Store, The_Circle));

      return Shape_Id (The_Circle.Index);
   end New_Circle;

   --------------
   -- New_Line --
   --------------

   function New_Line
     (X      : Float;
      Y      : Float;
      X2     : Float;
      Y2     : Float;
      Color  : Color_Type)
      return Shape_Id
   is
      The_Line : Shape_Data (Line);
   begin
      The_Line.X := X;
      The_Line.Y := Y;
      The_Line.X2 := X2;
      The_Line.Y2 := Y2;
      The_Line.Color := Color;

      Data_Manager.Take_Id (The_Line.Index);
      Data_Manager.Schedule_Command ((Store, The_Line));

      return Shape_Id (The_Line.Index);
   end New_Line;

   -------------
   -- New_Box --
   -------------

   function New_Box
     (X,  Y          : Float;
      Width, Height : Float;
      Color  : Color_Type)
      return Shape_Id
   is
      The_Box : Shape_Data (Box);
   begin
      The_Box.X := X;
      The_Box.Y := Y;
      The_Box.Width := Width;
      The_Box.Height := Height;

      The_Box.Color := Color;

      Data_Manager.Take_Id (The_Box.Index);
      Data_Manager.Schedule_Command ((Store, The_Box));

      return Shape_Id (The_Box.Index);
   end New_Box;

   ----------------
   -- Set_Radius --
   ----------------

   procedure Set_Radius (Shape : in out Shape_Id; Value : Float) is
   begin
     Data_Manager.Schedule_Command ((Set_Radius, Natural (Shape), Value));
   end Set_Radius;

   ----------------
   -- Get_Radius --
   ----------------

   function Get_Radius (Shape : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (Shape)).Radius;
   end Get_Radius;

   ------------
   -- Get_X2 --
   ------------

   function Get_X2 (V : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (V)).X2;
   end Get_X2;

   ------------
   -- Get_Y2 --
   ------------

   function Get_Y2 (V : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (V)).Y2;
   end Get_Y2;

   ------------
   -- Set_X2 --
   ------------

   procedure Set_X2 (V : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_X2, Natural (V), Value));
   end Set_X2;

   ------------
   -- Set_Y2 --
   ------------

   procedure Set_Y2 (V : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_Y2, Natural (V), Value));
   end Set_Y2;

   ---------------
   -- New_Torus --
   ---------------

   function New_Torus
     (X            : Float;
      Y            : Float;
      Inner_Radius : Float;
      Outer_Radius : Float;
      Color        : Color_Type)
      return Shape_Id
   is
      The_Torus : Shape_Data (Torus);
   begin
      The_Torus.X := X;
      The_Torus.Y := Y;
      The_Torus.Inner_Radius := Inner_Radius;
      The_Torus.Outer_Radius := Outer_Radius;
      The_Torus.Color := Color;

      Data_Manager.Take_Id (The_Torus.Index);
      Data_Manager.Schedule_Command ((Store, The_Torus));

      return Shape_Id (The_Torus.Index);
   end New_Torus;

   ----------------------
   -- Set_Inner_Radius --
   ----------------------

   procedure Set_Inner_Radius (V : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_Inner_Radius, Natural (V), Value));
   end Set_Inner_Radius;

   ----------------------
   -- Set_Outer_Radius --
   ----------------------

   procedure Set_Outer_Radius (V : in out Shape_Id; Value : Float) is
   begin
      Data_Manager.Schedule_Command ((Set_Outer_Radius, Natural (V), Value));
   end Set_Outer_Radius;

   ----------------------
   -- Get_Inner_Radius --
   ----------------------

   function Get_Inner_Radius (V : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (V)).Inner_Radius;
   end Get_Inner_Radius;

   ----------------------
   -- Get_Outer_Radius --
   ----------------------

   function Get_Outer_Radius (V : Shape_Id) return Float is
   begin
      return Data_Manager.Get_Data (Natural (V)).Outer_Radius;
   end Get_Outer_Radius;

   --------------
   -- New_Text --
   --------------

   function New_Text
     (X     : Float;
      Y     : Float;
      Text  : String;
      Color : Color_Type)
      return Shape_Id
   is
      The_Text : Shape_Data (Display.Kernel.Text);
   begin
      The_Text.X := X;
      The_Text.Y := Y;
      The_Text.The_Text := new String'(Text);
      The_Text.Color := Color;

      Data_Manager.Take_Id (The_Text.Index);
      Data_Manager.Schedule_Command ((Store, The_Text));

      return Shape_Id (The_Text.Index);
   end New_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (V : in out Shape_Id; Text : String) is
   begin
      Data_Manager.Schedule_Command
        ((Set_Text, Natural (V), new String'(Text)));
   end Set_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (V : Shape_Id) return String is
   begin
      return Data_Manager.Get_Text (Natural (V));
   end Get_Text;

   -------------------
   -- Read_Last_Key --
   -------------------

   function Read_Last_Key return Key_Type is
      Val : Key_Type;
   begin
      Data_Manager.Read_Last_Key (Val);

      return Val;
   end Read_Last_Key;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Key : Key_Type) return Character is
   begin
      if Key <= 255 then
         return Character'Val (Integer (Key));
      else
         return ASCII.Nul;
      end if;
   end To_Character;

   ------------------
   -- To_Character --
   ------------------

   function To_Special(Key : Key_Type) return Special_Key is
   begin
      if Key > 255 then
         return Special_Key'Val (Key - 256);
      else
         return KEY_NONE;
      end if;
   end To_Special;

   function Read_Last_Mouse_Position return Mouse_Position is
      P : Mouse_Position;
   begin
      Data_Manager.Read_Last_Mouse_Position (P);

      return P;
   end Read_Last_Mouse_Position;

   ------------
   -- At_End --
   ------------

   function At_End return Boolean is
   begin
      return Display.Kernel.At_End;
   end At_End;

end Display.Basic;
