-- Copyright (c) 2003 George Shapovalov <george@gentoo.org>.  All rights reserved.

-- This smal utility reads results file in ATF format for template search page
-- and dumps list of episode starts one per line.
-- (to beused with abfextract from abftools)

-- # This program is free software; you can redistribute it and/or
-- # modify it under the terms of the GNU General Public License as
-- # published by the Free Software Foundation; either version 2 of the
-- # License, or (at your option) any later version.
-- #
-- # This program is distributed in the hope that it will be useful,
-- # but WITHOUT ANY WARRANTY; without even the implied warranty of
-- # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- # GNU General Public License for more details.
-- #
-- # You should have received a copy of the GNU General Public License
-- # along with this program; if not, write to the Free Software
-- # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
-- # USA


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Atf_IO;

with Ada.Command_Line;  use Ada;
with GNAT.Command_Line;
with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;

procedure List_Starts is

	procedure printUsage is
	begin
		Put_Line("This smal utility reads results file in ATF format for template search page");
		Put_Line("and dumps list of episode starts one per line.");
		Put_Line("(to be used with abfextract from abftools)");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " options  file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-s s    name of column to dump (as in units)");
		Put_Line("-n f    number of column to dump");
		--Put_Line("-g      turn on debug output");
		Put_line("-x f    rescale time by multiplying by this factor");
		New_Line;
	end printUsage;

Finish : Exception;

package MyAtf_IO is new Atf_IO(Float);
use MyAtf_IO;

type paramRec is record
	XScale : Float := 1.0;
	ColName : Unbounded_String := To_Unbounded_String("Event Start Time (ms)");
		-- in case another name is passed
	ColNum : MyAtf_IO.Count := 5;
	--Debug : Boolean := False;
	DataFileName : Unbounded_String := Null_Unbounded_String;
end record;

	procedure processCommandLine(params : out ParamRec) is
		--use Integer_Text_IO;
		use GNAT.Command_Line; use Ada.Command_Line;

		Options : constant String := "h n: s: x:";
		Last:Positive;
	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to process local exceptions
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;

				when 'h' => printUsage;
				--when 'g' => params.Debug := True;

				when 'n' => Get(Parameter,Float(params.ColNum), Last);
				when 's' => params.ColName :=  To_Unbounded_String(Parameter);

				when 'x' => Get(Parameter,Float(params.XScale), Last);

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
			end loop;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error        => Put_Line ("Invalid numeric format for switch" & Full_Switch);raise Finish;
		end;
		params.DataFileName := To_Unbounded_String(Get_Argument(Do_Expansion => True));
		-- some consistency checks
	end processCommandLine;



-------------------------------------------
-- main block

	F:File_Type;
	ATFData : Atf_File;

	params : ParamRec;

begin
	processCommandLine(params);
	ATFData := MyAtf_IO.getData(To_String(params.DataFileName));
		-- first column will get zapped by getData by conversion to XStart/XStep
		-- but its quite likely to hold all zeros anyway

  declare
	dat : Float2DArray renames AtfData.data.data;
		-- triple data in a row is too ugly :)
		-- need to do renaming after data has been initialized
	units : StringArray renames ATFData.Header.units;

  begin
	-- find the column with fragment start times
	-- it's supposed to be #3, but if not, thensearch by name
	if units(params.ColNum) /= To_String(params.ColName) then
		for i in units'Range(1) loop
			if units(i) = To_String(params.ColName) then
				params.ColNum := i;
				exit;
			end if;
		end loop;
	end if;

	-- now that we have column identified we can print the data
	for i in dat'Range(2) loop
		-- start times should be pretty exclusively in ms
		-- so lets try some formatting
		 Put(dat(params.ColNum-1, i)*params.XScale, Fore=>1, Aft=>3, Exp=>0);
		 New_Line;
	end loop;
  end;
	exception
		when Finish =>
			null; -- normal termination
end List_Starts;
