--  -----------------------------------------------------------------------
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with GNAT.OS_Lib;           use GNAT.OS_Lib;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Numerics;                      use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with System;                            use System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Calendar;   use Ada.Calendar;

with GL_glu_h;         use GL_glu_h;
with SDL_SDL_h;        use SDL_SDL_h;
with SDL_SDL_stdinc_h; use SDL_SDL_stdinc_h;
with SDL_SDL_video_h;  use SDL_SDL_video_h;
with SDL_SDL_events_h; use SDL_SDL_events_h;
with SDL_SDL_timer_h;  use SDL_SDL_timer_h;
with SDL_SDL_keysym_h; use SDL_SDL_keysym_h;
with SDL_SDL_ttf_h;    use SDL_SDL_ttf_h;

with Ada.Unchecked_Deallocation;
with Ada.Task_Identification;
with Ada.Task_Termination;    use Ada.Task_Termination;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;

package body Display.Kernel is

   Shapes : Shape_Array (1 .. Max_Shapes) := (others => Null_Shape);

   Initialized : Boolean := False with Atomic, Volatile;
   Quadric     : System.Address;

   Window_Width, Window_Height : Integer;

   Last_Key : Key_Type with Atomic;
   --  a shared variable, set concurrently by the Poll_Events routine and read 
   --  by client code

   procedure Draw_Text (Obj : in out Shape_Object)
   with Pre => Obj.Kind = Text;

   procedure Update_Text (Obj : in out Shape_Object)
     with Pre => Obj.Kind = Text;
   
   -----------
   -- Check --
   -----------

   procedure Check (Ret : Int) is
   begin
      if Ret /= 0 then
         raise Display_Error;
      end if;
   end Check;
   
   ----------------------
   -- Read_Current_Key --
   ----------------------

   procedure Read_Current_Key (Key : out Key_Type) is
   begin
      Key := Last_Key;
   end Read_Current_Key;   
  
   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (Color : Color_Type) is
   begin
      case Color is
         when Black =>
            glColor3d (0.2, 0.2, 0.2);

         when Blue =>
            glColor3d (0.0, 0.0, 1.0);

         when Green =>
            glColor3d (0.0, 1.0, 0.0);

         when Cyan =>
            glColor3d (0.0, 1.0, 1.0);

         when Red =>
            glColor3d (1.0, 0.0, 0.0);

         when Magenta =>
            glColor3d (1.0, 0.0, 1.0);

         when Yellow =>
            glColor3d (1.0, 1.0, 0.0);

         when White =>
            glColor3d (1.0, 1.0, 1.0);

      end case;
   end Set_Color;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (Length : Float; Width : Float) is
   begin
      glBegin(GL_QUADS);

      -- Front Face
      glNormal3f (0.0, 0.0, 1.0);
      glVertex3f (0.0, -Width, Width);
      glVertex3f (Length, -Width, Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (0.0, Width, Width);

      -- Back Face
      glNormal3f (0.0, 0.0, -1.0);
      glVertex3f (0.0, -Width, -Width);
      glVertex3f (0.0, Width, -Width);
      glVertex3f (Length, Width, -Width);
      glVertex3f (Length, -Width, -Width);

      -- Top Face
      glNormal3f (0.0, 1.0, 0.0);
      glVertex3f (0.0, Width, -Width);
      glVertex3f (0.0, Width, Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (Length, Width, -Width);

      -- Bottom Face
      glNormal3f (0.0, -1.0, 0.0);
      glVertex3f (0.0, -Width, -Width);
      glVertex3f (Length, -Width, -Width);
      glVertex3f (Length, -Width, Width);
      glVertex3f (0.0, -Width, Width);

      -- Right face
      glNormal3f(1.0, 0.0, 0.0);
      glVertex3f (Length, -Width, -Width);
      glVertex3f (Length, Width, -Width);
      glVertex3f (Length, Width, Width);
      glVertex3f (Length, -Width, Width);

      -- Left Face
      glNormal3f (-1.0, 0.0, 0.0);
      glVertex3f (0.0, -Length, -Length);
      glVertex3f (0.0, -Length, Length);
      glVertex3f (0.0, Length, Length);
      glVertex3f (0.0, Length, -Length);

      glEnd;
   end Draw_Line;

   --------------
   -- Draw_Box --
   --------------

   procedure Draw_Box (X, Y, Width, Height : Float) is
      pragma Unreferenced (X, Y);
      X1 : constant Float := -Width / 2.0;
      Y1 : constant Float := -Height / 2.0;
      X2 : constant Float := -Width / 2.0;
      Y2 : constant Float := Height / 2.0;
      X3 : constant Float := Width / 2.0;
      Y3 : constant Float := Height / 2.0;
      X4 : constant Float := Width / 2.0;
      Y4 : constant Float := -Height / 2.0;

      Dx : constant := 2.0;
      Dy : constant := 2.0;
      Back : constant := -20.0;
      Front : constant := 0.0;
   begin
      glBegin(GL_QUADS);

      glNormal3f(-1.0, 0.0, 0.0);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);

      glNormal3f(1.0, 0.0, 0.0);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X4, Y4, Front);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);

      glNormal3f(0.0, 0.0, 1.0);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X4, Y4, Front);

      glNormal3f(0.0, 0.0, -1.0);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);

      glNormal3f(0.0, 1.0, 0.0);
      glVertex3f(X2, Y2, Front);
      glVertex3f(X3, Y3, Front);
      glVertex3f(X3 - Dx, Y3 - Dy, Back);
      glVertex3f(X2 - Dx, Y2 - Dy, Back);

      glNormal3f(0.0, -1.0, 0.0);
      glVertex3f(X4, Y4, Front);
      glVertex3f(X1, Y1, Front);
      glVertex3f(X1 - Dx, Y1 - Dy, Back);
      glVertex3f(X4 - Dx, Y4 - Dy, Back);

      glEnd;
   end Draw_Box;

   ----------------
   -- Draw_Torus --
   ----------------

   procedure Draw_Torus
     (Inner_Radius : GLfloat;
      Outer_Radius : GLfloat;
      Nsides : GLint;
      Rings : GLint)
   is
      Theta, Phi, Theta1 : GLfloat;
      CosTheta, SinTheta : GLfloat;
      CosTheta1, SinTheta1 : GLfloat;
      RingDelta, SideDelta : GLfloat;
   begin
      RingDelta := 2.0 * Pi / GLfloat (Rings);
      SideDelta := 2.0 * Pi / GLfloat (Nsides);

      Theta := 0.0;
      CosTheta := 1.0;
      SinTheta := 0.0;

      for i in reverse 0 .. Rings - 1 loop
         Theta1 := Theta + RingDelta;
         CosTheta1 := cos(Theta1);
         SinTheta1 := sin(Theta1);
         glBegin(GL_QUAD_STRIP);
         phi := 0.0;
         for j in reverse 0 .. nsides loop
            declare
               CosPhi, SinPhi, Dist : GLfloat;
            begin
               Phi := Phi + SideDelta;
               CosPhi := Cos (Phi);
               SinPhi := Sin (Phi);
               Dist := Outer_Radius + Inner_Radius * CosPhi;

               glNormal3f
                 (cosTheta1 * CosPhi, -SinTheta1 * CosPhi, SinPhi);
               glVertex3f
                 (cosTheta1 * dist, -SinTheta1 * dist, Inner_Radius * SinPhi);
               glNormal3f
                 (cosTheta * CosPhi, -SinTheta * CosPhi, sinPhi);
               glVertex3f
                 (cosTheta * Dist, -SinTheta * Dist,  Inner_Radius * SinPhi);
            end;
         end loop;
         glEnd;

         Theta := Theta1;
         CosTheta := CosTheta1;
         SinTheta := SinTheta1;
      end loop;
   end Draw_Torus;

   ----------
   -- Draw --
   ----------

   procedure Draw (Shape : in out Shape_Object) is
   begin
      case Shape.Kind is
         when Circle =>
            --  glutSolidSphere (Win32.GL.GLdouble (Shape.Radius), 20, 20);
            gluSphere
              (qobj   => Quadric,
               radius => GLdouble (Shape.D.Radius),
               slices => 20,
               stacks => 20);

         when Line =>

            declare
               Dx : constant Float := Shape.D.X2 - Shape.D.X;
               Dy : constant Float := Shape.D.Y2 - Shape.D.Y;
               Length : constant Float := sqrt (Dx  * Dx + Dy * Dy);

               The_Cos : Float;
               Angle   : Float;

               Width : constant := 1.0;
            begin
               if Length = 0.0 then
                  return;
               end if;

               The_Cos := Dx / Length;
               Angle := Arccos (The_Cos) / (2.0 * Pi) * 360.0;

               if Dx >= 0.0 and then Dy >= 0.0 then
                  null;
               elsif Dx < 0.0 and then Dy >= 0.0 then
                  null;
               elsif Dx < 0.0 and then Dy < 0.0 then
                  Angle := -Angle;
               else
                  Angle := -Angle;
               end if;

               glPushMatrix;
               glRotated (GLDouble (Angle), 0.0, 0.0, 1.0);
               glRotated (GLDouble (Seconds (Clock) * 10.0), 1.0, 0.0, 0.0);

               Draw_Line (Length, Width);

               glPopMatrix;
            end;

         when Torus =>
            Draw_Torus
              (Inner_Radius => GLfloat (Shape.D.Inner_Radius),
               Outer_Radius   => GLfloat (Shape.D.Outer_Radius),
               nsides  => 10,
               rings   => 20);

         when Text =>
            Draw_Text (Shape);

         when Box =>
            glPushMatrix;

            Draw_Box
              (X => Shape.D.X,
               Y => Shape.D.Y,
               Width => Shape.D.Width,
               Height => Shape.D.Height);
            glPopMatrix;

         when None =>
            null;

      end case;
   end Draw;

   --------------------
   -- Graphical loop --
   --------------------

   procedure Reshape (W : Integer; H : Integer) is
      Ratio : GLdouble;
   begin
      Window_Width := W;
      Window_Height := H;

      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;

      Ratio := GLdouble (w) / GLdouble (h);

      if w > h then
         glOrtho (-100.0 * Ratio, 100.0 * Ratio, -100.0, 100.0, -100.0, 300.0);
      else
         glOrtho (-100.0, 100.0, -100.0 / Ratio, 100.0 / Ratio, -100.0, 300.0);
      end if;

      glViewport (0, 0, GLsizei (w), GLsizei (h));
      glMatrixMode (GL_MODELVIEW);
   end Reshape;

   ----------
   -- Idle --
   ----------

   procedure Idle is
      Tmp_List : Command_Lists.List;
   begin
      Data_Manager.Flush (Tmp_List);

      for C of Tmp_List loop
         case C.Kind is
            when Set_X =>
               Shapes (C.Index).D.X := C.Value_F;

            when Set_Y =>
               Shapes (C.Index).D.Y := C.Value_F;

            when Set_X2 =>
               Shapes (C.Index).D.X2 := C.Value_F;

            when Set_Y2 =>
               Shapes (C.Index).D.Y2 := C.Value_F;

            when Set_Color =>
               Shapes (C.Index).D.Color := C.Value_C;

            when Set_Radius =>
               Shapes (C.Index).D.Radius := C.Value_F;

            when Set_Inner_Radius =>
               Shapes (C.Index).D.Inner_Radius := C.Value_F;

            when Set_Outer_Radius =>
               Shapes (C.Index).D.Outer_Radius := C.Value_F;

            when Set_Text =>
               GNAT.Strings.Free (Shapes (C.Index).D.The_Text);
               Shapes (C.Index).D.The_Text := C.Value_T;
               Update_Text (Shapes (C.Index));

            when Delete =>
               if Shapes (C.Index).Kind = Text then
                  GNAT.Strings.Free (Shapes (C.Index).D.The_Text);
               end if;

               Shapes (C.Index) := Null_Shape;

            when Store =>
               declare
                  To_Store : Shape_Object (C.Object.Kind);
               begin
                  To_Store.D := C.Object;
                  Shapes (C.Object.Index) := To_Store;
               end;

               if C.Object.Kind = Text then
                  Update_Text (Shapes (C.Object.Index));
               end if;

         end case;
      end loop;

      glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glMatrixMode (GL_MODELVIEW);

      for J in Shapes'Range loop
         if Shapes (J).D.Index /= 0 then
            glPushMatrix;

            if Shapes (J).Kind = Box then
               glTranslated
                 (GLdouble (Shapes (J).D.X + Shapes (J).D.Width / 2.0),
                  GLdouble (Shapes (J).D.Y + Shapes (J).D.Height / 2.0),
                  0.0);
            elsif Shapes (J).Kind /= Text then
               glTranslated
                 (GLdouble (Shapes (J).D.X),
                  GLdouble (Shapes (J).D.Y),
                  0.0);
            end if;

            Set_Color (Shapes (J).D.Color);
            Draw (Shapes (J));
            glPopMatrix;
         end if;
      end loop;
   end Idle;

   task GL_Task;

   surface : access SDL_Surface;
   vidInfo : access SDL_VideoInfo;
   w       : constant Integer := 400;
   h       : constant Integer := 400;

   bpp   : constant Interfaces.C.int := 16;
   flags : constant Interfaces.C.unsigned := SDL_OPENGL + SDL_HWSURFACE + SDL_RESIZABLE;

   Stop : Boolean := False;

   procedure Set_SDL_Video;
   procedure Set_OpenGL;
   procedure Poll_Events;

   type Glubyte_Arrays is array (int range <>) of aliased GLubyte;

   -----------------
   -- Update_Text --
   -----------------

   procedure Update_Text (Obj : in out Shape_Object) is
      fntCourrier : System.Address;
      clrFg : SDL_Color;
      sText : access SDL_SDL_video_h.SDL_Surface;
      C_String : chars_ptr;

      procedure Free is new Ada.Unchecked_Deallocation
        (Pixel_Array, Pixel_Array_Access);
   begin
      if Obj.Kind /= Text then
         return;
      end if;

      if Obj.Pixels /= null then
         Free (Obj.Pixels);
      end if;

      if GNAT.Strings."=" (Obj.D.The_Text, null) or else Obj.D.The_Text.all = "" then
         return;
      end if;

      while not Initialized loop
         null;
      end loop;

      C_String := New_String (Obj.D.The_Text.all);

      fntCourrier := TTF_OpenFont (Interfaces.C.Strings.New_String ("C:\Windows\Fonts\arial.ttf"), 12);

      clrFg := (0, 0, 255, 0);

      sText :=
        TTF_RenderText_Solid
          (fntCourrier, C_String, clrFg);

      Obj.W := Integer (sText.pitch);
      Obj.H := Integer (sText.h);

      declare
         Import_Pixels : aliased Glubyte_Arrays (0 .. int (Obj.W * Obj.H - 1));
         pragma Import (C, Import_Pixels);
         for Import_Pixels'Address use sText.pixels;

      begin
         Obj.Pixels := new Pixel_Array(Import_Pixels'Range);
         Obj.Pixels.all := (others => (0, 0, 0, 0));

         for J in Import_Pixels'Range loop
            declare
               Line : int := J / int (Obj.W);
               Col  : constant int := J - Line * int (Obj.W);
            begin
               Line := int (Obj.H) - Line - 1;

               if Import_Pixels (J) /= 0 then
                  Obj.Pixels (Line * int (Obj.W) + Col).R := 255;
               end if;
            end;
         end loop;
      end;

      Free (C_String);
      TTF_CloseFont (fntCourrier);
      SDL_FreeSurface (sText);
   end Update_Text;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text (Obj : in out Shape_Object) is
      swapbytes, lsbfirst, rowlength, skiprows, skippixels, alignment
        : aliased GLint;
   begin
      if Obj.Pixels = null then
         return;
      end if;

      glNormal3d (0.0, 0.0, 1.0);
      glRasterPos3d (GLdouble (Obj.D.X),  GLdouble (Obj.D.Y), GLDouble (20.0));

      glGetIntegerv(GL_UNPACK_SWAP_BYTES, swapbytes'Unchecked_Access);
      glGetIntegerv(GL_UNPACK_LSB_FIRST, lsbfirst'Unchecked_Access);
      glGetIntegerv(GL_UNPACK_ROW_LENGTH, rowlength'Unchecked_Access);
      glGetIntegerv(GL_UNPACK_SKIP_ROWS, skiprows'Unchecked_Access);
      glGetIntegerv(GL_UNPACK_SKIP_PIXELS, skippixels'Unchecked_Access);
      glGetIntegerv(GL_UNPACK_ALIGNMENT, alignment'Unchecked_Access);

      --  Little endian machines (DEC Alpha for example) could
      --  benefit from setting GL_UNPACK_LSB_FIRST to GL_TRUE
      --  instead of GL_FALSE, but this would require changing the
      --  generated bitmaps too.
      glPixelStorei(GL_UNPACK_SWAP_BYTES, GL_FALSE);
      glPixelStorei(GL_UNPACK_LSB_FIRST, GL_FALSE);
      glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
      glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
      glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

      glDrawPixels
        (width  => int (Obj.W),
         height => int (Obj.H),
         format => GL_RGBA,
         c_type => GL_UNSIGNED_BYTE,
         pixels => Obj.Pixels.all'Address);

      glPixelStorei(GL_UNPACK_SWAP_BYTES, swapbytes);
      glPixelStorei(GL_UNPACK_LSB_FIRST, lsbfirst);
      glPixelStorei(GL_UNPACK_ROW_LENGTH, rowlength);
      glPixelStorei(GL_UNPACK_SKIP_ROWS, skiprows);
      glPixelStorei(GL_UNPACK_SKIP_PIXELS, skippixels);
      glPixelStorei(GL_UNPACK_ALIGNMENT, alignment);
   end Draw_Text;

   -------------
   -- GL_Task --
   -------------

   task body GL_Task is
   begin
      --  SDL is comprised of 8 subsystems. Here we initialize the video
      if SDL_Init(SDL_INIT_VIDEO) < 0 then
         Put_Line ("Error initializing SDL");
         SDL_SDL_h.SDL_Quit;
      end if;

      --  Rather than set the video properties up in the constructor, I set
      --  them in setVideo. The reason for this is that 2 pointers are used
      --  to interact with SDL structures. Once used they convert their
      --  handles into vidInfo and surface tamer variables. That this
      --  occurs inside the function means the pointers will release
      --  their memory on function exit.

      Set_SDL_Video;

      --  openGL is not part of SDL, rather it runs in a window handled
      --  by SDL. here we set up some openGL state

      Set_OpenGL;

      Check (TTF_Init);

      Initialized := True;

      while not Stop loop
         Idle;
         Poll_Events;

         glFlush;
         SDL_GL_SwapBuffers;
         SDL_Delay (1);
      end loop;

      TTF_Quit;
      SDL_SDL_h.SDL_Quit;
      GNAT.OS_Lib.OS_Exit (0);
   end GL_Task;

   -------------------
   -- Set_SDL_Video --
   -------------------

   procedure Set_SDL_Video is
   begin
      --  To center a non-fullscreen window we need to set an environment
      --  variable

      Check (SDL_putenv(New_String ("SDL_VIDEO_CENTERED=center")));

      --  the video info structure contains the current video mode. Prior to
      --  calling setVideoMode, it contains the best available mode
      --  for your system. Post setting the video mode, it contains
      --  whatever values you set the video mode with.
      --  First we point at the SDL structure, then test to see that the
      --  point is right. Then we copy the data from the structure to
      --  the safer vidInfo variable.

      declare
         ptr  : System.Address := SDL_GetVideoInfo;
         for ptr'Address use vidInfo'Address;
      begin
         if ptr = System.Null_Address then
            Put_Line ("Error querying video info");
            SDL_SDL_h.SDL_Quit;
            return;
         end if;
      end;

      --  according to the SDL documentaion, the flags parameter passed to setVideoMode
      --  affects only the 2D SDL surface, not the openGL. To set their properties
      --  use the syntax below. We enable vsync because we are running the loop
      --  unfettered and we don't want the loop redrawing the buffer
      --  while it is being written to screen

      Check (SDL_GL_SetAttribute(SDL_GL_SWAP_CONTROL, 1));--enable vsync
      Check (SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 8));
      Check (SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 8));
      Check (SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 8));
      Check (SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16));
      Check (SDL_GL_SetAttribute(SDL_GL_MULTISAMPLEBUFFERS, 1));
      Check (SDL_GL_SetAttribute(SDL_GL_MULTISAMPLESAMPLES, 2));
      Check (SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1));

      --  the setVideoMode function returns the current frame buffer as an
      --  SDL_Surface. Again, we grab a pointer to it, then place its
      --  content into the non pointery surface variable. I say 'non-pointery',
      --  but this SDL variable must have a pointer in it because it can
      --  access the current pixels in the framebuffer.

      surface := SDL_SetVideoMode(int (w), int (h), bpp, flags);

      if surface = null then
         Put_Line ("Error setting the video mode");
         SDL_SDL_h.SDL_Quit;
         return;
      end if;
   end Set_SDL_Video;

   ----------------
   -- Set_OpenGL --
   ----------------

   procedure Set_OpenGL is
      type GLFloat_Array is array (Natural range <>) of aliased GLfloat;
      light_diffuse : aliased GLFloat_Array
        := (1.0, 1.0, 1.0, 1.0);
      light_position : aliased GLFloat_Array
        := (1.0, 1.0, 1.0, 0.0);
   begin
      Quadric := gluNewQuadric;
      Reshape (w, h);

      glClearColor (0.1, 0.1, 0.1, 0.0);

      glLightfv (GL_LIGHT0, GL_DIFFUSE, light_diffuse (0)'Access);
      glLightfv (GL_LIGHT0, GL_POSITION, light_position (0)'Access);

      glFrontFace (GL_CW);
      glEnable (GL_LIGHTING);
      glEnable (GL_LIGHT0);
      glEnable (GL_DEPTH_TEST);
      glEnable (GL_COLOR_MATERIAL);
      glEnable (GL_NORMALIZE);
      glDepthFunc (GL_LESS);

      glMatrixMode (GL_MODELVIEW);
      gluLookAt (0.0, 0.0, 10.0,
                 0.0, 0.0, 0.0,
                 0.0, 1.0, 0.0);
   end Set_OpenGL;

   -----------------
   -- Poll_Events --
   -----------------

   procedure Poll_Events is
      Evt : aliased SDL_Event;

      function As_Key (K : Special_Key) return Key_Type is
      begin
         return 256 + Special_Key'Pos (K);
      end As_Key;
      
      Up_Arrow    : constant Key_Type := As_Key (KEY_UP);
      Down_Arrow  : constant Key_Type := As_Key (KEY_DOWN);
      Left_Arrow  : constant Key_Type := As_Key (KEY_LEFT);
      Right_Arrow : constant Key_Type := As_Key (KEY_RIGHT);
      
   begin
      while SDL_PollEvent (Evt'Unchecked_Access) /= 0 loop
         case unsigned (Evt.c_type) is
            when SDL_SDL_events_h.SDL_Quit =>
               Stop := True;
            when SDL_KEYDOWN =>               
               case Evt.Key.Keysym.Sym is
                  when SDLK_LEFT =>
                     Last_Key := Left_Arrow;
                  when SDLK_RIGHT =>
                     Last_Key := Right_Arrow;
                  when SDLK_UP =>
                     Last_Key := Up_Arrow;
                  when SDLK_DOWN =>
                     Last_Key := Down_Arrow;
                  when others =>
                     Last_Key := Key_Type (Evt.Key.Keysym.Sym);
               end case;

               if Evt.Key.Keysym.Sym = SDLK_ESCAPE then
                  Stop := True;
               end if;
               
            when SDL_KEYUP =>
               Last_Key := 0;

            when SDL_VIDEORESIZE =>
               Reshape (Integer (Evt.resize.w), Integer (Evt.resize.h));

            when SDL_MOUSEBUTTONDOWN =>
               declare
                  Pos : Mouse_Position := No_Mouse_Position;
               begin
                  if Window_Width > Window_Height then
                     Pos.Y :=
                       -Float (Evt.motion.y) / Float (Window_Height)
                       * 200.0 + 100.0;

                     Pos.X :=
                       (Float (Evt.motion.x) - (Float (Window_Width) / 2.0))
                       / Float (Window_Height) * 200.0;
                  else
                     Pos.X :=
                       Float (Evt.motion.x) / Float (Window_Width)
                       * 200.0 - 100.0;

                     Pos.Y :=
                       -(Float (Evt.motion.y) - (Float (Window_Height) / 2.0))
                       / Float (Window_Width) * 200.0;
                  end if;

                  if Evt.button.button = 1 then
                     Pos.Button := Left;
                  elsif Evt.button.button = 3 then
                     Pos.Button := Right;
                  end if;

                  Data_Manager.Set_Last_Mouse_Position (Pos);
               end;

            when others =>
               null;
         end case;
      end loop;
   end Poll_Events;

   -------------------------
   -- Exception_Reporting --
   -------------------------

   protected Exception_Reporting is
      procedure Report
        (Cause : Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence);
   end Exception_Reporting;

   -------------------------
   -- Exception_Reporting --
   -------------------------

   protected body Exception_Reporting is

      procedure Report
        (Cause : Cause_Of_Termination;
         T     : Ada.Task_Identification.Task_Id;
         X     : Ada.Exceptions.Exception_Occurrence) is
         pragma Unreferenced (Cause, T);
      begin
         Put_Line ("=== UNCAUGHT EXCEPTION ===");
         Put_Line (Exception_Information (X));

         Put_Line (Symbolic_Traceback (X));

         GNAT.OS_Lib.OS_Exit (1);
      end Report;

   end Exception_Reporting;

   ------------------
   -- Command_List --
   ------------------

   protected body Data_Manager is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         for J in Available_Ids'Range loop
            Available_Ids (J) := J;
         end loop;
      end Initialize;

      ----------------------
      -- Schedule_Command --
      ----------------------

      procedure Schedule_Command (C : Command) is
      begin
         List.Append (C);

         case C.Kind is
            when Set_X =>
               Data (C.Index).X := C.Value_F;

            when Set_Y =>
               Data (C.Index).Y := C.Value_F;

            when Set_X2 =>
               Data (C.Index).X2 := C.Value_F;

            when Set_Y2 =>
               Data (C.Index).Y2 := C.Value_F;

            when Set_Color =>
               Data (C.Index).Color := C.Value_C;

            when Set_Radius =>
               Data (C.Index).Radius := C.Value_F;

            when Set_Inner_Radius =>
               Data (C.Index).Inner_Radius := C.Value_F;

            when Set_Outer_Radius =>
               Data (C.Index).Outer_Radius := C.Value_F;

            when Set_Text =>
               GNAT.Strings.Free (Data (C.Index).The_Text);
               Data (C.Index).The_Text := C.Value_T;

            when Delete =>
               if Data (C.Index).Kind = Text then
                  GNAT.Strings.Free (Data (C.Index).The_Text);
               end if;

               Release_Id (C.Index);
               Data (C.Index) := Null_Data;

            when Store =>
               Data (C.Object.Index) := C.Object;

         end case;
      end Schedule_Command;

      -----------
      -- Flush --
      -----------

      procedure Flush (Commands : out Command_Lists.List) is
      begin
         Command_Lists.Move (Commands, List);

         for C of Commands loop
            --  Since text pointers may be deleted by further commands, we
            --  need to ensure that the user receive a copy, and that this
            --  copy reflext the last status of the object. We may have
            --  several identical commands here, which is fine.

            if C.Kind = Set_Text then
               if Data (C.Index).Kind = Text then
                  --  When encountering a text, copy the real text
                  -- (still valid)

                  C.Value_T := new String'(Data (C.Index).The_Text.all);
               else
                  --  otherwise, the text has been deleted, no point in setting
                  --  it

                  C.Value_T := null;
               end if;
            end if;

            if C.Kind = Store and then C.Object.Kind = Text then
               if Data (C.Object.Index).Kind = Text then
                  --  When encountering a text, copy the real text
                  -- (still valid)

                  C.Object.The_Text
                    := new String'(Data (C.Object.Index).The_Text.all);
               else
                  --  otherwise, the text has been deleted, no point in setting
                  --  it

                  C.Object.The_Text := null;
               end if;
            end if;
            end loop;
      end Flush;

      -------------
      -- Take_Id --
      -------------

      procedure Take_Id (Value : out Natural) is
      begin
         Value := Available_Ids (Stack_Pointer);
         Stack_Pointer := Stack_Pointer + 1;
      end Take_Id;

      ----------------
      -- Release_Id --
      ----------------

      procedure Release_Id (Id : Natural) is
      begin
         Stack_Pointer := Stack_Pointer - 1;
         Available_Ids (Stack_Pointer) := Id;
      end Release_Id;

      --------------
      -- Get_Data --
      --------------

      function Get_Data (Id : Natural) return Shape_Data is
      begin
         return Data (Id);
      end Get_Data;

      --------------
      -- Get_Text --
      --------------

      function Get_Text (Id : Natural) return String is
      begin
         return Data (Id).The_Text.all;
      end Get_Text;

      -----------------------------
      -- Set_Last_Mouse_Position --
      -----------------------------

      procedure Set_Last_Mouse_Position (P : Mouse_Position) is
      begin
         Last_Mouse_Position := P;
      end Set_Last_Mouse_Position;

      procedure Read_Last_Mouse_Position (P : out Mouse_Position) is
      begin
         P := Last_Mouse_Position;
         Last_Mouse_Position := No_Mouse_Position;
      end Read_Last_Mouse_Position;

   end Data_Manager;

   --  For further reference, if needed, here is the calls required to draw a
   --  complete cube:

--     procedure draw is
--     begin
--        glTranslatef(0.0, 0.0, -10.0);
--
--        glBegin(GL_QUADS);
--
--        -- Front Face
--        glNormal3f(0.0, 0.0, 1.0);
--        glColor3f(0.2, 0.5, 0.2);
--        glVertex3f(-1.0, -1.0, 1.0);
--        glVertex3f( 1.0, -1.0, 1.0);
--        glVertex3f( 1.0, 1.0, 1.0);
--        glVertex3f(-1.0, 1.0, 1.0);
--
--        -- Back Face
--        glNormal3f(0.0, 0.0, -1.0);
--        glColor3f(0.5, 0.2, 0.5);
--        glVertex3f(-1.0, -1.0, -1.0);
--        glVertex3f(-1.0, 1.0, -1.0);
--        glVertex3f( 1.0, 1.0, -1.0);
--        glVertex3f( 1.0, -1.0, -1.0);
--
--        -- Top Face
--        glNormal3f(0.0, 1.0, 0.0);
--        glColor3f(0.7, 0.5, 0.2);
--        glVertex3f(-1.0, 1.0, -1.0);
--        glVertex3f(-1.0, 1.0, 1.0);
--        glVertex3f( 1.0, 1.0, 1.0);
--        glVertex3f( 1.0, 1.0, -1.0);
--
--        -- Bottom Face
--        glNormal3f(0.0, -1.0, 0.0);
--        glColor3f(0.2, 0.2, 0.8);
--        glVertex3f(-1.0, -1.0, -1.0);
--        glVertex3f( 1.0, -1.0, -1.0);
--        glVertex3f( 1.0, -1.0, 1.0);
--        glVertex3f(-1.0, -1.0, 1.0);
--
--        -- Right face
--        glNormal3f(1.0, 0.0, 0.0);
--        glColor3f(0.2, 0.5, 0.5);
--        glVertex3f( 1.0, -1.0, -1.0);
--        glVertex3f( 1.0, 1.0, -1.0);
--        glVertex3f( 1.0, 1.0, 1.0);
--        glVertex3f( 1.0, -1.0, 1.0);
--
--        -- Left Face
--        glNormal3f(-1.0, 0.0, 0.0);
--        glColor3f(0.8, 0.2, 0.6);
--        glVertex3f(-1.0, -1.0, -1.0);
--        glVertex3f(-1.0, -1.0, 1.0);
--        glVertex3f(-1.0, 1.0, 1.0);
--        glVertex3f(-1.0, 1.0, -1.0);
--
--        glEnd;
--     end draw;


begin
   Data_Manager.Initialize;
   Set_Dependents_Fallback_Handler (Exception_Reporting.Report'Access);
   Set_Specific_Handler 
     (Ada.Task_Identification.Current_Task, 
      Exception_Reporting.Report'Access);   
end Display.Kernel;
