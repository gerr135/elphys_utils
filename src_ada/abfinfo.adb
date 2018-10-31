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
-- with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
-- with Ada.Strings.Fixed;
with GNAT.Command_Line;

with ABF.Header.Waveform;
use ABF;


procedure abfinfo is

	procedure printUsage is
	begin
		Put_Line("This program prints some basic abf parameters from file header.");
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options] file.abf");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output");
	end printUsage;

	Finish,
	Not_Implemented : Exception;

	type ParamRec is record
		InFile  : Unbounded_String := Null_Unbounded_String;
		Debug     : Boolean := False;
	end record;

	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		
		--Last : Positive;

	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("g h") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;
				
				when others =>
					raise Program_Error;         -- should not get here!
			end case;
		  end loop Options;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
-- 			when Data_Error        => Put_Line ("Invalid numeric format for switch " & Full_Switch);raise Finish;
		end;

		declare
-- 			use Ada.Strings.Fixed;
			S1 : String := Get_Argument(Do_Expansion => True);
		begin
			if S1 /= "" then
				params.InFile := To_Unbounded_String(S1);
			else
				Put_Line("Please supply a name of abf file!");
				raise Finish;
			end if;
			if params.Debug then
				Put_Line("in file: " & To_String(params.InFile));
			end if;
		end;
	end processCommandLine;

	params : ParamRec;
-- 	data : ABF.Data.File_Contents := ABF.Data.ReadABFFile(To_String(params.InFile));
	abfH : ABF.Header.ABFFileHeader;

begin
	processCommandLine(params);
	abfH := ABF.Header.Read_Header(To_String(params.InFile));
	ABF.Header.PrintPrincialParams (abfH);

	exception
	  when Finish =>
		null; -- normal termination
end abfinfo;