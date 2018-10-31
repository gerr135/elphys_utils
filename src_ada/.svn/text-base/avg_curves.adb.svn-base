--**************************************************************************
--   Copyright (C) 2006 by George Shapovalov  <george@gentoo.org>         --
--                                                                        --
--   This program is free software; you can redistribute it and/or modify --
--   it under the terms of the GNU Library General Public License as      --
--   published by the Free Software Foundation; either version 2 of the   --
--   License, or (at your option) any later version.                      --
--                                                                        --
--   This program is distributed in the hope that it will be useful,      --
--   but WITHOUT ANY WARRANTY; without even the implied warranty of       --
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        --
--   GNU General Public License for more details.                         --
--                                                                        --
--   You should have received a copy of the GNU Library General Public    --
--   License along with this program; if not, write to the                --
--   Free Software Foundation, Inc.,                                      --
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.            --
--**************************************************************************


with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

with Ada.Command_Line; use Ada;
with GNAT.Command_Line;
-- with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
-- with Ada.Strings.Fixed;
-- with Ada.Numerics.Long_Elementary_Functions;

with InterpFuncs_Support;


procedure Avg_Curves is

	procedure printUsage is
	begin
		Put_Line("Reads tabulated data and produces a table of averages.");
		Put_Line("Two files are taken, 1st is the 'stats' file, that is updated (or created),");
		Put_Line("the data in the 2nd file is used to update the averages");
		New_Line;
		Put_Line("The original data has a generic column-associated tabulated form, that is:");
		Put_Line("comment line 1");
		Put_Line("...");
		Put_Line("header1   header2   ... headerN");
		Put_Line("val1_1    val2_1    ... valN_1");
		Put_Line("val2_1    val2_2 ...");
		New_Line;
		Put_Line("The 'stats' file also has an N=# line right above headers");
		Put_Line("where the number of averaged entries is stored");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options] stats_file curve_file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h         print this help");
		Put_Line("-g         turn on debug output");
		Put_Line("-c s       process only column identified by its name (matches header)");
		Put_Line("-d{d,e,a}  print stddev (d), stderr (e) or both (a) stats for every parameter");
		Put_Line("            Note! This should match the processed stats file!");
		New_Line;
	end printUsage;


	Finish,
	Not_Implemented : Exception;

	type Statistics_Type is (StdDev, StdErr, Both);

	type ParamRec is record
		statsFileName, dataFileName  : Unbounded_String := Null_Unbounded_String;
		Debug   : Boolean := False;

		NCommentLines : Natural := 0;

		columnName : Unbounded_String := Null_Unbounded_String;
			--  "" - no column requested, process all
			--  otherwise holds the string identifying the header entry

		statsType : Statistics_Type := StdDev;
	end record; -- ParamRec


	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		Last : Positive;

	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("g h c: da dd de n:") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 'c' =>
					params.columnName := To_Unbounded_String(Parameter);

				when 'd' =>
					if    Full_Switch = "da" then params.statsType := Both;
					elsif Full_Switch = "dd" then params.statsType := StdDev;
					elsif Full_Switch = "de" then params.statsType := StdErr;
					else raise Invalid_Switch; end if;

				when 'n' =>
					Ada.Integer_Text_IO.Get(Parameter, params.NCommentLines, Last);

				when others =>
					raise Program_Error;         -- forgot some option
			end case;
		  end loop Options;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error        => Put_Line ("Invalid numeric format for switch " & Full_Switch);raise Finish;
		end;

		declare
			S1 : String := Get_Argument(Do_Expansion => True);
			S2 : String := Get_Argument(Do_Expansion => True);
		begin
			if S1 /= "" then
				params.statsFileName := To_Unbounded_String(S1);
			else
				Put_Line("Please supply a name of a file with averages!");
				raise Finish;
			end if;
			if S2 /= "" then
				params.dataFileName := To_Unbounded_String(S2);
			else
				Put_Line("Please supply a name of a file with new data!");
				raise Finish;
			end if;
			if params.Debug then
				Put_Line(Standard_Error, "stats file: " & To_String(params.statsFileName)
					& ",  data file: " & To_String(params.dataFileName));
			end if;
		end;  -- declare S1, S2
	end processCommandLine;



	-------------------------------------------------------
	--  small utility stuff and duplicate code
	use InterpFuncs_Support;
	params : ParamRec;


	function  Calc_Num_of_stats_Columns(data : Table_IO.DataRec; params : ParamRec) return ParamIndex is
		ColumnRepetitionFactor : ParamIndex := 2;
	  begin
		if params.statsType = Both then
			ColumnRepetitionFactor := 3;
		end if;
		if params.columnName = Null_Unbounded_String then
			return ColumnRepetitionFactor*(data.NCols - 1) + 1;
			--  the "x" column is not duplicated
		else
			return ColumnRepetitionFactor + 1;
		end if;
	  end;


	procedure CopyHeader(stats : in out Table_IO.DataRec; data : in Table_IO.DataRec;
		what : Statistics_Type; from : ParamIndex; AutoCol : Boolean := True) is
		--
		--  if we only copy one columnt the "to" index is always 1
		col : ParamIndex := from;
		use Table_IO;
	  begin
		if not AutoCol then col := 1; end if;
		if params.Debug then
			Put_Line(Standard_Error, "<dbg> CopyHeader, from=" & from'Img & ",  AutoCol=" & AutoCol'Img);
		end if;
		case what is
		  when StdDev =>
			Stats.header(2*col)   := Data.header(from);
			Stats.header(2*col+1) := "d_" & Head(Data.header(from), Count=>Data.header(from)'Length-2);
		  when StdErr =>
			Stats.header(2*col)   := Data.header(from);
			Stats.header(2*col+1) := Data.header(from) & "_err";
		  when Both =>
			Stats.header(3*col)   := Data.header(from);
			Stats.header(3*col+1) := Data.header(from) & "_dev";
			Stats.header(3*col+2) := Data.header(from) & "_err";
		end case;
	  end CopyHeader;



	----------------------------------------------------------------------
	--  main


begin
	processCommandLine(params);
	--
	declare
		DataFile  : Table_IO.DataRec := Table_IO.Read(To_String(params.dataFileName),
			CommentLines => params.NCommentLines);
		NStatsCols : ParamIndex := Calc_Num_of_stats_Columns(DataFile, params);
		StatsFile : Table_IO.DataRec(NCols => NStatsCols,
			NRows => DataFile.NRows, NComments => DataFile.NComments + 1);
	begin
		if params.Debug then
			Put_Line(Standard_Error, "<dbg> DataFile.NCols = " & DataFile.NCols'Img &
				", NStatsCols =" & NStatsCols'Img);
		end if;
		--
		if Ada.Directories.Exists(To_String(params.statsFileName)) then
			StatsFile := Table_IO.Read(To_String(params.statsFileName),
				CommentLines => params.NCommentLines + 1);
			--
			raise Not_Implemented;
		else  --  first averaging,need to create a new stats file
			--  copy comments over
			StatsFile.comments(DataFile.comments'Range) := DataFile.comments;
			StatsFile.comments(StatsFile.comments'Last) := To_Unbounded_String("N=1");
			--  form a header (and check that a proper param name was given)
			StatsFile.header(0) := DataFile.header(0);
			if params.columnName = Null_Unbounded_String then
				for i in DataFile.header'First + 1 .. DataFile.header'Last loop
					CopyHeader(StatsFile, DataFile, What => params.statsType, from=>i);
				end loop;
			else
			  declare
				NColumn : ParamIndex := 0;
					--  numerical index of the requested column,
					--  0 apparently should be invalid - no sense in passing X-column label
				use Ada.Strings.Unbounded; use Ada.Strings;
			  begin
				for i in DataFile.header'First + 1 .. DataFile.header'Last loop
					if Trim(To_Unbounded_String(String(DataFile.header(i))), Side=>Both)
						= Trim(params.columnName, Side=>Both)
					then
						NColumn := i;
					end if;
				end loop;
				if NColumn = 0 then
					Put_Line("specified prameter not found, please check!");
					raise Finish;
				end if;
				CopyHeader(StatsFile, DataFile, what=>params.statsType, from=>NColumn, AutoCol=>False);
			  end;
			end if;  -- columnName = Null..
			--  copy data
		end if;  -- stats file exists
	end;

  exception
	when Finish => Null;

end Avg_Curves;
