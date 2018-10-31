--
--    Copyright (C) 2003 George Shapovalov
--
--    This program is free software; you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation; either version 2, or (at your option)
--    any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program; if not, write to the Free Software Foundation,
--    Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;  use Ada;
with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with GNAT.Command_Line;
with ATF_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Generic_Simplex;
with Transition_Searches;

procedure Clean_Transitions is

Finish : Exception;

	procedure printUsage is
	begin
		Put_Line("This program attempts to clear noisy transition that can roughly be");
		Put_Line("approximated by boltzman off the interference, roughly resembling");
		Put_Line("sine wave or series of sine waves");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  file");
		Put_Line("outputs goes to stdout");
		New_Line;
		Put_Line("fit options:");
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output");
		Put_Line("-a f    fit on interval from a to b");
		Put_Line("-b f      concerns both boltzman and interf fits");
		Put_Line("-c f    approx transition midpoint, if ommitted autodetect");
		Put_Line("-d f    estimate of responsivity at midpoint (non-fixed)");
		Put_Line("-l f    estimate of left (pre-) and right (post-)");
		Put_Line("-r f    amplitudes (makes corresponding parameter[s] fixed)");
		Put_Line("-e fn   just do a plain subtraction of interference with params given in file");
		Put_Line("-em fn  same as -e, but adjust phase (reference time) to fit indiv trace better");
		Put_Line("input options:");
		Put_Line("-n n    # of column to analyse (leftmost is treated as x, has#0)");
		Put_Line("           if omitted, process (individually) all y's");
		Put_line("-x f    rescale time by multiplying by this factor");
		Put_line("-2      fit by sin(w*x)+sin(2*w*x)");
		Put_Line("output options:");
		Put_Line("-t [d,u] force transition direction: down, up (skip wrongs)");
		Put_Line("-s      skip diverged fits (x0 outside boundary)");
		Put_Line("-sl f   skip fits where A2-A1 < supplied number");
		Put_Line("-sh f   skip fits where A2-A1 > supplied number");
		Put_Line("-kl f   skip fits where T10-90 is larger than specified");
		Put_Line("-kh f   skip fits where T10-90 is smaller than specified");
		Put_Line("-fore, -aft, -exp   format output floats as per RM");
		Put_Line("-o c|f  output (c)leaned trace or (f)it results");
	end printUsage;

-------------------
-- principal types and definitions
type MyReal is new Long_Float;

package MyFloat_IO is new Float_IO(MyReal);
use MyFloat_IO;


begin
exception
	when Finish =>
		null;
end Clean_Transitions;