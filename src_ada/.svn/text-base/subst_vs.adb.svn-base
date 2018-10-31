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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
with GNAT.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with InterpFuncs_Support; use InterpFuncs_Support;

procedure Subst_Vs is

	procedure printUsage is
	begin
		Put_Line("Substitute the regular V's of -70, -60, etc in the fitted/nicified file");
		Put_Line("as if dV compensation was nor performed during leak subtraction");
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h        print this help");
		Put_Line("-g        turn on debug output");
		Put_Line("-v f      starting V (default: -70 mV)");
		Put_Line("-s f      V step (default: +10 mV)");
		Put_Line("Program reads the given file and spits the output to stdout");
	end printUsage;


	Finish,
	Not_Implemented : Exception;

	type ParamRec is record
		FileName  : Unbounded_String := Null_Unbounded_String;
		Debug   : Boolean := False;
		--
		V0 : Internal_Float := -70.0;
		dV : Internal_Float :=  10.0;
	end record; -- ParamRec


	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line;
-- 		--use Ada.Command_Line;
		Last : Positive;

	begin
		if Ada.Command_Line.Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("g h s: v:") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 's' => Internal_Float_IO.Get(Parameter,params.dV,Last);

				when 'v' => Internal_Float_IO.Get(Parameter,params.V0,Last);

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
				Put_Line(Standard_Error, "in file: " & To_String(params.FileName));
			end if;
		end;  -- declare S
	end processCommandLine;


	params : ParamRec;

begin
	processCommandLine(params);
	--
	declare
		data : Table_IO.DataRec := Table_IO.Read(To_String(params.FileName));
	begin
		if params.Debug then
			Put_Line(Standard_Error, "<dbg> NCols =" & data.NCols'Img &
				",  data'Range =" & data.data'First(1)'Img &" .."& data.data'Last(1)'Img &
				",  data.header'Range =" & data.header'First'Img &" .."& data.header'Last'Img);
		end if;
		--  adjust V's
		for row in data.data'Range(2) loop
			data.data(ParamIndex'First, row) := params.V0 + Internal_Float(row - 1)*params.dV;
		end loop;
		--  output the resuls
		Table_IO.Write(data, F=>Ada.Text_IO.Standard_Output, Fore=>3, Aft=>3, Exp=>0);
	end;

exception
	when Finish => Null;
end Subst_Vs;