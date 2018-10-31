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
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;  with GNAT.Command_Line; use Ada;
with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters;

with ATF_IO;
with Ada.Numerics.Discrete_Random;

procedure prep_Collage is

Finish : Exception; -- immediate (normal) termination

	procedure printUsage is
	begin
		Put_Line("Reads ATF file and dumps episodes as separate x y pairs");
		Put_Line("shifting time for each, so that they form a 'collage' on a single plot");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  file");
		Put_Line("outputs goes to stdout");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output");
		Put_Line("-n n    number of episodes to dump (defaults to all)");
		Put_Line("-r      randomise selected (instead of first n)");
		Put_Line("-d f    spacing between episodes in whatever time units (defaults to 20% of duration)");
		Put_Line("-fmt [org|grp]  output format - to be imported by Origin (2n xy pairs per line)");
		Put_Line("              or used with graph (x y's separated by blank lines)");
		Put_Line("-fore, -aft, -exp   format output floats as per RM");
	end printUsage;


type MyReal is new Long_Float;
package MyFloat_IO is new Float_IO(MyReal);
package MyAtf_IO is new Atf_IO(MyReal);
use MyFloat_IO; use MyAtf_IO;

type OutputFormat is (Origin,Graph);
type ParamRec is record
	NEpisodes : MyAtf_IO.Count := 0;
	RandomizeEpisodes : Boolean := False;
	SkipInterval : MyReal := 0.0;

	OutFmt : OutputFormat := Graph;
	-- how many header lines to skip
	Fore : Ada.Text_IO.Field := MyFloat_IO.Default_Fore;
	Aft  : Ada.Text_IO.Field := MyFloat_IO.Default_Aft;
	Exp  : Ada.Text_IO.Field := MyFloat_IO.Default_Exp;
		-- formatting of floating numbers output

	-- Generic section
	FileName : Unbounded_String := Null_Unbounded_String;
	Debug : Boolean := False;
end record;


	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		use Integer_Text_IO;
		Options : constant String := "aft: d: exp: fmt: fore: h g n: r";
		Last:Positive;
	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;
		begin -- need to process local exceptions
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;

				when 'a' =>
					if Full_Switch="aft" then
						Get(Parameter,Ada.Text_IO.Field(params.Aft),Last);
					else
						Put_Line("switch "& Full_Switch &" is not recognized");
					end if;

				when 'd' => MyFloat_IO.Get(Parameter,params.SkipInterval, Last);

				when 'e' =>
					if Full_Switch="exp" then
						Get(Parameter,Ada.Text_IO.Field(params.Exp),Last);
					else
						Put_Line("switch "& Full_Switch &" is not recognized");
					end if;

				when 'f' =>
					if Full_Switch="fore" then
						Get(Parameter,Ada.Text_IO.Field(params.Fore),Last);
					elsif Full_Switch="fmt" then
						if Parameter="org" then
							params.OutFmt := Origin;
						elsif Parameter="grp" then
							params.OutFmt := Graph;
						else
							Put_Line("parameter to "& Full_Switch &" must be one of org or grp");
						end if;
					else
						Put_Line("switch "& Full_Switch &" is not recognized");
					end if;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 'n' => Get(Parameter,Integer(params.NEpisodes), Last);

				when 'r' => params.RandomizeEpisodes := True;
				when others =>
					raise Program_Error;         -- should not get here!
			end case;
			end loop;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error        => Put_Line ("Invalid numeric format for switch" & Full_Switch);raise Finish;
		end;
		-- process file name
		declare
			S : String := Get_Argument(Do_Expansion => True);
		begin
			if S /= "" then
				params.FileName := To_Unbounded_String(S);
			else
				Put_Line("Please supply the name of the file containing data to be averaged!");
				raise Finish;
			end if;
		end;
	end processCommandLine;


--------------------
-- main block


	params : ParamRec;
	AtfData : MyAtf_IO.Atf_File;

begin
	ProcessCommandLine(params);
	AtfData := MyAtf_IO.getData(To_String(params.FileName));

	-- some adjustments now that we got data
	if params.NEpisodes = 0 then params.NEpisodes := AtfData.data.data'Length(1); end if;

	declare
		dat : Float2DArray renames AtfData.data.data;
		subtype EpisodeSequence is MyAtf_IO.Count range dat'Range(1);
		package Random_Episodes is new Ada.Numerics.Discrete_Random (EpisodeSequence);
		use Random_Episodes;
		EpGenerator : Generator;

		EpisodeDuration : MyReal := AtfData.data.XStep*MyReal(dat'Length(2)-1);

		-- lets save info on what traces we dump via header
		TraceNumber : array(1..params.NEpisodes) of MyAtf_IO.Count;
	begin
		if params.SkipInterval = 0.0 then
			-- use 20% of duration by default
			params.SkipInterval := 0.2*EpisodeDuration;
		end if;

		-- generate list of episodes to dump
		Reset(EpGenerator);
		for col in 1..params.NEpisodes loop
			if params.RandomizeEpisodes then
				TraceNumber(col) := Random(EpGenerator);
				-- should work for NEpisodes << dat.Length(1)
			else
				TraceNumber(col) := col;
			end if;
		end loop;

		-- two formats I need to use have pretty much nothing in common,
		-- so looks like two separate blocks is all I can do
		case params.OutFmt is
		  when Origin =>
		  	-- every line has n xy pairs with x'a appropriately shifted
			-- output header
			Put("time");
			-- don't want any trailing tabs, this seems to cause
			-- Origin to think there is an extra (empty) column
			for col in 1 .. params.NEpisodes-1 loop
				Put(Latin_1.HT);
				Put("trace");
				Put(Positive(TraceNumber(col)), Width=>1);
				Put(Latin_1.HT& "time");
			end loop;
			Put("trace"); Put(Positive(TraceNumber(params.NEpisodes)), Width=>1);
			New_Line;
			-- and finally the data
			for raw in dat'Range(2) loop
				for col in 1 .. params.NEpisodes loop
					-- put x
					Put(AtfData.data.XStart + AtfData.data.XStep*MyReal(raw-1) +
						MyReal(col-1)*(EpisodeDuration + params.SkipInterval),
						Fore=>params.Fore, Aft=>params.Aft, Exp=>params.Exp);
					Put(Latin_1.HT);
					-- and y
					Put(dat(TraceNumber(col),raw),
						Fore=>params.Fore, Aft=>params.Aft, Exp=>params.Exp);
					if col = params.NEpisodes then
						New_Line;
					else
						Put(Latin_1.HT);
					end if;
				end loop; -- col
			end loop; -- raw

		  when Graph =>
		  	-- every line has single xy pair forone set, then blank line, then next set
			-- no headers this time
		  	for col in TraceNumber'Range loop
				for raw in dat'Range(2) loop
					-- put x
					Put(AtfData.data.XStart + AtfData.data.XStep*MyReal(raw-1) +
						MyReal(col-1)*(EpisodeDuration + params.SkipInterval),
						Fore=>params.Fore, Aft=>params.Aft, Exp=>params.Exp);
					Put(' ');
					-- and y
					Put(dat(TraceNumber(col),raw),
						Fore=>params.Fore, Aft=>params.Aft, Exp=>params.Exp);
						New_Line;
				end loop; -- raw
				New_Line;
			end loop; -- col
		end case;
	end;

exception
	when Finish =>
		null;
end;