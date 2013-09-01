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

with GNAT.Strings; use GNAT.Strings;
with GL_Gl_H;      use GL_Gl_H;
with Interfaces.C; use Interfaces.C;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;

private package Display.Kernel is

   --  This kernel is managing two sets of data. The protected object
   --  Data_Manager is responsible to manage a protected set of objects. It
   --  contains an internal representation of the graphical context, and
   --  buffers the sequence of commands that issued this representation since
   --  the last flush performed by the graphical loop. The graphical loops
   --  contains its own representation of the context (see Shapes in the body),
   --  which is only updated once at each iteration.

   type Shape_Kind is (None, Circle, Line, Box, Torus, Text);

   --  This record contains what is necessary to describe a shape on the
   --  board.
   type Shape_Data (Kind : Shape_Kind := Circle) is record
      Index    : Integer range 0 .. Max_Shapes := 0;
      X        : Float                         := 0.0;
      Y        : Float                         := 0.0;
      Color    : Color_Type                    := Black;

      case Kind is
         when Circle =>
            Radius : Float := 0.0;

         when Line =>
            X2, Y2 : Float;

         when Box =>
            Width, Height : Float := 0.0;

         when Torus =>
            Inner_Radius : Float := 0.0;
            Outer_Radius : Float := 0.0;

         when Text =>
            The_Text : GNAT.Strings.String_Access;

         when others =>
            null;
      end case;
   end record;

   Null_Data : constant Shape_Data := (others => <>);

   type Command_Kind is
     (Set_X, Set_Y, Set_X2, Set_Y2,
      Set_Color,
      Set_Radius, Set_Inner_Radius, Set_Outer_Radius,
      Set_Text,
      Delete,
      Store);

   --  This type is used to store a command modifying the graphical
   --  environment.
   type Command (Kind : Command_Kind := Set_X) is record
      case Kind is
         when Store =>
            Object : Shape_Data;
         when others =>
            Index : Natural range 0 .. Max_Shapes;

            case Kind is
               when Set_X | Set_Y | Set_X2 | Set_Y2 |
                    Set_Radius | Set_Inner_Radius | Set_Outer_Radius =>

                  Value_F : Float;

               when Set_Color =>
                  Value_C : Color_Type;

               when Set_Text =>
                  Value_T : String_Access;

               when Delete =>
                  null;

               when Store =>
                  null;
            end case;
      end case;
   end record;

   procedure Read_Current_Key (Key : out Key_Type);
   --  Key gets the value of the current key pressed on the keyboard, or 0 if no
   --  key is currently pressed

   package Command_Lists is new Ada.Containers.Doubly_Linked_Lists (Command);

   type Id_Stack is array (Integer range <>) of Natural;
   type Shape_Data_Array is array (Integer range <>) of Shape_Data;

   --  This type offers a thread-safe interface to the graphical data. Commands
   --  are taken into account immediately by its internal state. However,
   --  the graphical loop will only read those commands once per cycle.
   protected Data_Manager is
      procedure Initialize;

      procedure Schedule_Command (C : Command);

      procedure Flush (Commands : out Command_Lists.List);

      procedure Take_Id (Value : out Natural);

      function Get_Data (Id : Natural) return Shape_Data;

      function Get_Text (Id : Natural) return String;

      procedure Set_Last_Mouse_Position (P : Mouse_Position);

      procedure Read_Last_Mouse_Position (P : out Mouse_Position);

   private
      procedure Release_Id (Id : Natural);

      List          : Command_Lists.List;
      Available_Ids : Id_Stack (1 .. Max_Shapes);
      Data          : Shape_Data_Array (1 .. Max_Shapes);
      Stack_Pointer : Integer := 1;

      Last_Mouse_Position : Mouse_Position := No_Mouse_Position;
   end Data_Manager;


   type E_Pixel is record
      R : GLubyte;
      G : GLubyte;
      B : GLubyte;
      A : GLubyte;
   end record;
   pragma Convention (C, E_Pixel);

   type Pixel_Array is array (int range <>) of E_Pixel;
   pragma Convention (C, Pixel_Array);

   type Pixel_Array_Access is access all Pixel_Array;

   type Shape_Object (Kind : Shape_Kind := Circle) is record
      D : Shape_Data (Kind);

      case Kind is
         when Text =>
            Pixels   : Pixel_Array_Access;
            --  TODO: make sure that this structure is freed when needed.
            W, H     : Integer;

         when others =>
            null;
      end case;
   end record;

   Null_Shape : constant Shape_Object := (None, others => <>);

   procedure Draw (Shape : in out Shape_Object);

   type Shape_Array is array (Integer range <>) of aliased Shape_Object;

   At_End : Boolean := False
      with Atomic;

end Display.Kernel;
