if gdb.breakpoints () != None:
   [l.delete () for l in gdb.breakpoints ()]

class MyBreakpoint (gdb.Breakpoint):
   def __init__ (self, spec):
      super (MyBreakpoint, self).__init__ (spec)

   def stop (self):
      v = gdb.parse_and_eval ("V")
      print ("Integral between [1.0-" + str (v ["slice"]) + "] is " + str (v ["val"]))
      return False

MyBreakpoint ("main.adb:14")

gdb.execute ("run")
