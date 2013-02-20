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

package Display is

   type Color_Type is (Black, Blue, Green, Cyan, Red, Magenta, Yellow, White);

   type Key_Type is private;

   type Special_Key is
     (KEY_NONE,
      KEY_F1,
      KEY_F2,
      KEY_F3,
      KEY_F4,
      KEY_F5,
      KEY_F6,
      KEY_F7,
      KEY_F8,
      KEY_F9,
      KEY_F10,
      KEY_F11,
      KEY_F12,
      KEY_LEFT,
      KEY_UP,
      KEY_RIGHT,
      KEY_DOWN,
      KEY_PAGE_UP,
      KEY_PAGE_DOWN,
      KEY_HOME,
      KEY_END,
      KEY_INSERT);

   Display_Error : exception;

   type Button_Type is (Left, Right);

   type Mouse_Position is record
      X, Y : Float;
      Button : Button_Type;
   end record;

   No_Mouse_Position : constant Mouse_Position := (-10_000.0, -10_000.0, Right);

private

   Max_Shapes : constant := 4_096;

   Special_Key_Shift : constant := 1000;

   type Key_Type is new Integer;

end Display;
