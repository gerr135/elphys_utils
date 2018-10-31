--**************************************************************************
--   Copyright (C) 2005 by George Shapovalov  --
--   george@gentoo.org  --
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

with Ada.Text_IO; use Ada.Text_IO; with Ada.Long_Float_Text_IO;
-- with Ada.Strings.Unbounded.Text_IO; -- this line imposed gnat-gcc-4.1 or gnat-gpl requirement..

with Ada.Command_Line; use Ada;
with GNAT.Command_Line;
-- with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
-- with Ada.Strings.Fixed;
-- with Ada.Numerics.Long_Elementary_Functions;

-- with Generic_Simplex;
-- with Golden_Section_Minimization;
with InterpFuncs_Support;


procedure Extract_Values is

	procedure printUsage is
	begin
		Put_Line("Extract fit values at given V or at the maxI or maxA voltages");
		Put_Line("This program expects the output of find_peaks, nicified by filter-find_peaks.py");
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options] file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h        print this help");
		Put_Line("-g        turn on debug output");
		Put_Line("-q        be quiet - only output a single line with results, no headers.");
		Put_Line("            this also triggers prepending the file name to the extracted string");
		New_Line;
		Put_Line("-a        get values at peak amplitude (maxA)");
		Put_Line("-i        get values at peak current (maxI, default)");
		Put_Line("-v f      get values at this V");
		Put_Line("Note:");
		Put_Line("-a, -i and -v are mutually exclusive, only one can be accepted at time!");
	end printUsage;


	Finish,
	Not_Implemented : Exception;

	type ExtractionRequest_Type is (maxA, maxI, givenV);

	type ParamRec is record
		FileName  : Unbounded_String := Null_Unbounded_String;
		Debug   : Boolean := False;
		Quiet   : Boolean := False;

		ExtractAt : ExtractionRequest_Type := maxI;
		V : InterpFuncs_Support.Internal_Float := 0.0;
			--  the requested V, when ExtractAt is maxI or maxA it is updated
			--  to reflect the position of the maximum
	end record; -- ParamRec


	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line;
-- 		--use Ada.Command_Line;
		use InterpFuncs_Support;
		Last : Positive;
		ExtractionRequest_AlreadyGiven : Boolean := False;

	begin
		if Ada.Command_Line.Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("a g h i q v:") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;
				when 'q' => params.Quiet := True;

				when 'a' =>
					if ExtractionRequest_AlreadyGiven then
						Put_Line("only one of -a, -i and -v can be given!");
						raise Finish;
					else
						params.ExtractAt := maxA;
						ExtractionRequest_AlreadyGiven := True;
					end if;

				when 'i' =>
					if ExtractionRequest_AlreadyGiven then
						Put_Line("only one of -a, -i and -v can be given!");
						raise Finish;
					else
						params.ExtractAt := maxI;
						ExtractionRequest_AlreadyGiven := True;
					end if;

				when 'v' =>
					if ExtractionRequest_AlreadyGiven then
						Put_Line("only one of -a, -i and -v can be given!");
						raise Finish;
					else
						Internal_Float_IO.Get(Parameter,params.V,Last);
						params.ExtractAt := givenV;
						ExtractionRequest_AlreadyGiven := True;
					end if;

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
		  end loop Options;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error        => Put_Line ("Invalid numeric format for switch " & Full_Switch);raise Finish;
		end;

		declare
			S : String := Get_Argument(Do_Expansion => True);
		begin
			if S /= "" then
				params.FileName := To_Unbounded_String(S);
			else
				Put_Line("Please supply a name of a file to process!");
				raise Finish;
			end if;
			if params.Debug then
				Put_Line(Standard_Error, "in file: " & To_String(params.FileName) &
					";   extracting at: " & params.ExtractAt'Img);
			end if;
		end;  -- declare S
	end processCommandLine;



	params : ParamRec;
-- 	data  : InterpolatedTable;


begin
	processCommandLine(params);
	--
	declare
		use InterpFuncs_Support;
		use Internal_Float_IO;
		--
		--  initial data table that is read
		data0 : Table_IO.DataRec := Table_IO.Read(To_String(params.FileName));
-- 		NEntries : constant ParamIndex := data0.NCols;
		--
		--  interpolation funcs built from data0
		data  : array(ParamIndex range 1 .. data0.NCols)
			of Interpolation_Quadratic.AverageFunc;
		--
		--  interpolated values - the result
		values : DataRow(data'Range);
		--
		ColIndex : ParamIndex;
	begin
		if params.Debug then
			Put_Line(Standard_Error, "<dbg> NCols =" & data0.NCols'Img &
				",  data0'Range =" & data0.data'First(1)'Img &" .."& data0.data'Last(1)'Img &
				",  data0.header'Range =" & data0.header'First'Img &" .."& data0.header'Last'Img);
			Put_Line(Standard_Error, ",  data'Range =" & data'First'Img &" .."& data'Last'Img  &
				",  values'Range =" & values'First'Img &" .."& values'Last'Img);
		end if;
		--  interpolate the data
		for col in data'Range loop
		  declare
			x, y : Vector(data0.data'Range(2));
		  begin
			for i in data0.data'Range(2) loop
				x(i) := data0.data(0, i);
				y(i) := data0.data(col, i);
			end loop;
			data(col) := Interpolation_Quadratic.Create(x, y);
		  end;
		end loop;
		--
		--  process the data
		if    params.ExtractAt = maxA then
			ColIndex := Table_IO.FindColPosition(data0, "A");
		elsif params.ExtractAt = maxI then
			ColIndex := Table_IO.FindColPosition(data0, "maxI");
		end if;
		if params.Debug then
			Put_Line(Standard_Error, "<dbg> ColIndex =" & ColIndex'Img &
				",  V(before) =" & params.V'Img);
		end if;
		--
		if (params.ExtractAt = maxA) or (params.ExtractAt = maxI) then
		  declare
			eps : Internal_Float := abs(data0.data(0, data0.NRows))/100.0;
			MaxIter : Positive := 1000;
			y : Internal_Float; -- placeholder for FindMaximum
		  begin
			Interpolation.FindMaximum(f=>data(ColIndex),
				a=>data0.data(ParamIndex'First, data0.data'First(2) + 1),
				b=>data0.data(ParamIndex'First, data0.data'Last(2)  - 1),
				--  we shrink the range of V's by one step, as the extreme V's
				--  have flat responce usually - won't converge well..
				eps => eps, MaxIter=>MaxIter,
				x=>params.V, y => y);
			--
			if params.Debug then
				Put_Line(Standard_Error, "<dbg> V(after) =" & params.V'Img &
					",  y = " & y'Img);
			end if;
		  end;
		end if;
		--
		--  now get the data at calculated (or diven) V
		for i in data'Range loop
			values(i) := Interpolation_Quadratic.Evaluate(func=>data(i), x=>params.V);
		end loop;
		--
		--  output the resuls
		if not params.Quiet then
			Put("V = ");
			Put(params.V, Fore=>3, Aft=>2, Exp=>0);
			Put_Line("  (" & params.ExtractAt'Img & ")");
			--  header
			declare
				S : String := Table_IO.Form_HeaderString(data0);
			begin
				Put_Line("  V   " & S(5 .. S'Last));
				--  a quick and dirty nicefication of the output - cutting some extra spaces in the beginning
			end;
		end if;
		--  the results
		if params.Quiet then
			Put(To_String(params.FileName) & "  ");
		end if;
		Put(params.V, Fore=>3, Aft=>2, Exp=>0);
		Put_Line(Table_IO.Form_VectorString(values, FieldWidth=>8, Aft=>2, Exp=>0));
		if not params.Quiet then
			New_Line;
		end if;
	end; -- declare data0, data..

exception
	when Finish => Null;

end Extract_Values;