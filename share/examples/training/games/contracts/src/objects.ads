with Display.Basic; use Display.Basic;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package Objects is

   No_Init : constant String (1 .. 10) := "_NOT_INIT_";

   type Object is record
      Id    : String (1 .. 10) := No_Init;
      X, Y  : Float range -200.0 .. 200.0 := 0.0;
      Size  : Float range 0.0 .. 100.0 := 0.0;
   end record;

   procedure Iterate (V : in out Object)
   with
     Pre => V.Id /= No_Init,
     Post =>
       V.Size = V.Size'Old and then
       V.Id = V.Id'Old and then
       Sqrt ((V.X - V.X'Old) ** 2 + (V.Y - V.Y'Old) ** 2) <= V.Size / 10.0;

   type Arr is array (Integer range <>) of Object
     with Dynamic_Predicate =>
       (for all J in Arr'Range =>
            Arr (J).Id = No_Init or else
              (for all K in J + 1 .. Arr'Last => Arr (J).Id /= Arr (K).Id));

   List : Arr (1 .. 100);

end Objects;
