-- Copyright (c) 2003 George Shapovalov <george@gentoo.org>.  All rights reserved.

-- This smal utility reads atf data file and dumps it to stdout
-- as x y_i datasets separated by empty line for every y column
-- This format is supposed to be used with graph tool from plotutils

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

procedure dumpAtf2plot is

	procedure printUsage is
	begin
		Put_Line("This smal utility reads atf data file and dumps it to stdout");
		Put_Line("as x y_i datasets separated by empty line for every y column");
		Put_Line("This format is supposed to be used with graph tool from plotutils");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " options  file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		--Put_Line("-g      turn on debug output");
		Put_line("-x f    rescale time by multiplying by this factor");
		New_Line;
	end printUsage;

Finish : Exception;

package MyAtf_IO is new Atf_IO(Float);
use MyAtf_IO;

type paramRec is record
	XScale : Float := 1.0;
	--Debug : Boolean := False;
	DataFileName : Unbounded_String := Null_Unbounded_String;
end record;

	procedure processCommandLine(params : out ParamRec) is
		--use Integer_Text_IO;
		use GNAT.Command_Line; use Ada.Command_Line;

		Options : constant String := "h x:";
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

	-- adjust x scale if requested
	if params.XScale /= 1.0 then
		ATFData.data.XStart := ATFData.data.XStart * params.XScale;
		ATFData.data.XStep  := ATFData.data.XStep  * params.XScale;
	end if;


  declare
	dat : Float2DArray renames AtfData.data.data;
		-- triple data in a row is too ugly :)
		-- need to do renaming after data has been initialized

	XStart : Float renames AtfData.data.XStart;
	XStep  : Float renames AtfData.data.XStep;

  begin
	-- so, we got the data, lets dump it!
	for episode in dat'Range(1) loop
		for i in dat'Range(2) loop
			Put_Line( Float'Image( XStart + XStep*Float(i-dat'First(2)) ) -- x
				&" "& dat(episode,i)'Img );  -- y
		end loop;
		New_Line;
	end loop;
  end;
	exception
		when Finish =>
			null; -- normal termination
end dumpAtf2plot;