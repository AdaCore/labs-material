-----------------------------------------------------------------------
--                              Ada Labs                             --
--                                                                   --
--                 Copyright (C) 2008-2013, AdaCore                  --
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

package Display.Basic is

   type Shape_Id is private;
   --  Base type of a shape - can represent a circle, a torus, a line or a
   --  ball.

   Null_Shape_Id : constant Shape_Id;
   --  Represent an empty or non-existing shape.

   procedure Set_X (Shape : in out Shape_Id; Value : Float)
   with Pre => Value <= 100_000.0;
   --  Modify the X-coordinate of a shape

   function Get_X (Shape : Shape_Id) return Float;
   --  Return the X-coordinate of a shape

   procedure Set_Y (Shape : in out Shape_Id; Value : Float)
   with Pre => Value <= 100_000.0;
   --  Modify the Y-coordinate of a shape

   function Get_Y (Shape : Shape_Id) return Float;
   --  Return the Y-coordinate of a shape

   procedure Set_Color (Shape : in out Shape_Id; Color : Color_Type);
   --  Modify the color of a shape

   function Get_Color (Shape : Shape_Id) return Color_Type;
   --  Return the color of a shape

   procedure Delete (Shape : in out Shape_Id);
   --  Remove a shape from the system - no more accessors and modifiers are
   --  allowed passed that point.

   function New_Circle
     (X      : Float;
      Y      : Float;
      Radius : Float;
      Color  : Color_Type)
      return Shape_Id;
   --  Create a new circle on the screen. X and Y are the center of the circule

   procedure Set_Radius (Shape : in out Shape_Id; Value : Float);
   --  Modify the radius of a circle - shape. Exception will be raised if used
   --  on other kind of shapes.

   function Get_Radius (Shape : Shape_Id) return Float;
   --  Return the radius of a circle - shape. Exception will be raised if used
   --  on other kind of shapes.

   function New_Line
     (X      : Float;
      Y      : Float;
      X2     : Float;
      Y2     : Float;
      Color  : Color_Type)
      return Shape_Id;
   --  Create a new line on the screen. X and Y are the coordinates of the
   --  first point of the line.

   function Get_X2 (V : Shape_Id) return Float;
   --  Return the x-coordinate of the second point of a line. Raises an
   --  exception if the shape is not a line.

   function Get_Y2 (V : Shape_Id) return Float;
   --  Return the y-coordinate of the second point of a line. Raises an
   --  exception if the shape is not a line.

   procedure Set_X2 (V : in out Shape_Id; Value : Float)
   with Pre => Value <= 100_000.0;
   --  Modify the x-coordinate of the second point of a line. Raises an
   --  exception if the shape is not a line.

   procedure Set_Y2 (V : in out Shape_Id; Value : Float)
   with Pre => Value <= 100_000.0;
   --  Modify the y-coordinate of the second point of a line. Raises an
   --  exception if the shape is not a line.

   function New_Box
     (X,  Y          : Float;
      Width, Height : Float;
      Color  : Color_Type)
      return Shape_Id;

   function New_Torus
     (X            : Float;
      Y            : Float;
      Inner_Radius : Float;
      Outer_Radius : Float;
      Color        : Color_Type)
      return Shape_Id;
   --  Create a new torus on the screen. X and Y are the coordinate of the
   --  center.

   procedure Set_Inner_Radius (V : in out Shape_Id; Value : Float);
   --  Set the size of the inner radius of a torus. Exception will be raised
   --  if the shape is not a torus.

   procedure Set_Outer_Radius (V : in out Shape_Id; Value : Float);
   --  Set the size of the outer radius of a torus. Exception will be raised
   --  if the shape is not a torus.

   function Get_Inner_Radius (V : Shape_Id) return Float;
   --  Modify the size of the inner radius of a torus. Exception will be raised
   --  if the shape is not a torus.

   function Get_Outer_Radius (V : Shape_Id) return Float;
   --  Modify the size of the inner radius of a torus. Exception will be raised
   --  if the shape is not a torus.

   function New_Text
     (X     : Float;
      Y     : Float;
      Text  : String;
      Color : Color_Type)
      return Shape_Id;
   --  Create a new text on the screen. X and Y are the coordinate of the left
   --  character of the string.

   procedure Set_Text (V : in out Shape_Id; Text : String);
   --  Modify the text associated to a text shape. Exception will be raised if
   --  the shape is not a text.

   function Get_Text (V : Shape_Id) return String;
   --  Return the test for the shape given in parameter

   function Current_Key_Press return Key_Type;
   --  Returns the key currently pressed on the keyboard, if any. Result is 0
   --  if no key is pressed when the function is called. Key presses are not
   --  stored, therefore if this function is not called sufficiently frequently
   --  they may be missed.

   function To_Character (Key : Key_Type) return Character;
   --  Convert the key to an ASCII-character if the key actually represent such
   --  character. ASCII.NUL returned otherwise

   function To_Special(Key : Key_Type) return Special_Key;
   --  Convert the key to an special key

   function Read_Last_Mouse_Position return Mouse_Position;

   function At_End return Boolean;
   --  Return true if the graphical context is closed

private

   type Shape_Id is new Natural range 0 .. Max_Shapes;

   Null_Shape_Id : constant Shape_Id := 0;

end Display.Basic;
