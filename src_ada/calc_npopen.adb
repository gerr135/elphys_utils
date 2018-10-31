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
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
with GNAT.Command_Line; with Ada.Command_Line;

with ABF.Data; use ABF;

procedure Calc_NPopen is

	procedure printUsage is
	begin
		Put_Line("Calculates a quick average of a specified segment of .abf or .atf file.");
		Put_Line("The file is expected to be baseline-corrected, although basic bl-correction");
		Put_Line("may be added at some point.");
		Put_Line("usage:");
		Put_Line("   " & Ada.Command_Line.Command_Name & " [options] file");
		New_Line;
		Put_Line("options:");
		Put_Line("-h        print this help");
		Put_Line("-g        turn on debug output");
		Put_Line("-a f      specification of an interval to search, in ms.");
		Put_Line("-b f        Omitting a or b sets it to the corresponding boundary.");
		Put_Line("            If a or b extend outside file contents the values in empty.");
		Put_Line("            part are taken to be 0.");
		Put_Line("-n n      number of data channel, in case there are multiple (default 0)");
		Put_Line("-q        quiet - only output the result (a single number)");
		Put_Line("-stict    Fail instead of extending-by-0's in case of given boundries outside data.");
		New_Line;
		Put_Line("the output goes to stdout");
	end printUsage;


	Finish,
	Not_Implemented : Exception;


	type ParamRec is record
		InFile  : Unbounded_String := Null_Unbounded_String;
		Debug   : Boolean := False;

		--  specification of ADC data channel in case there is ambiguity
		dataChannel : ADC_ChannelIndex;
		dataChannelGiven : Boolean := False;

		--  search window specification
		timeLo, timeHi : TimeBase_mks   := 0.0; -- these are set in command line
		timeLoGiven, timeHiGiven : Boolean := False;
			--  since we process files starting not at 0 we need these
		sampleLo, sampleHi : SampleIndex;   -- these are set during param adjustment

		StrictBoundary : Boolean := False;
			--  if user wants to abort instead of treating missing data as 0's
		Quiet : Boolean := False;
			--  whether to only output the result (single number)
	end record; -- ParamRec




	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		package ADC_ChannelIndex_IO is new Ada.Text_IO.Integer_IO(ADC_ChannelIndex);
		Last : Positive;

	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("a: b: g h n: q strict") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 'a' =>
					TimeBase_IO.Get(Parameter,params.timeLo,Last);
					params.timeLo := 1000.0*params.timeLo; -- boundaries are supplied in ms
					params.timeLoGiven := True;

				when 'b' =>
					TimeBase_IO.Get(Parameter,params.timeHi,Last);
					params.timeHi := 1000.0*params.timeHi; -- boundaries are supplied in ms
					params.timeHiGiven := True;

				when 'n' =>
					ADC_ChannelIndex_IO.Get(Parameter,params.dataChannel,Last);
					params.dataChannelGiven := True;

				when 'q' => params.Quiet := True;

				when 's' =>
					if Full_Switch = "strict" then params.StrictBoundary := True;
					else raise Invalid_Switch; end if;

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
-- 			use Ada.Strings.Fixed;
			S : String := Get_Argument(Do_Expansion => True);
		begin
			if S /= "" then
				params.InFile := To_Unbounded_String(S);
			else
				Put_Line("Please supply a name of data file!");
				raise Finish;
			end if;
			if params.Debug then
				Put_Line("in file: " & To_String(params.InFile));
			end if;
		end;  -- declare S1, S2
	end processCommandLine;


	--  Simple wrappers for often used conversions
	function time2Samples(t, dt : TimeBase_mks) return SampleIndex is
	  begin
		return 1 + SampleCount(TimeBase_mks'Floor(t/dt));
	  end;
	pragma Inline(time2Samples);

	function samples2Time(sample: SampleCount; dt : TimeBase_mks) return TimeBase_mks is
	  begin
		return dt*(sample-1);
	  end;
	pragma Inline(time2Samples);


	procedure doChecks_and_AdjustParams(data : in ABF.Data.File_Contents;
			params : in out ParamRec) is
	  begin
		--  check or detect data channel number
		if params.dataChannelGiven then
			if params.dataChannel > data.numChannels then
				Put_Line("incorrect channel number supplied! Please check parameters!");
				raise Finish;
			end if;
		else
			if data.numChannels > 1 then
				Put_Line("abf file contains >1 data channels, please specify which one to use!");
				raise Finish;
			else
				params.dataChannel := 1;
			end if;
		end if;
		if params.Debug then Put_Line("dataChannel="&params.dataChannel'Img); end if;
		--  search window
		--  check if we got reasonable params
		--  FIXME! The boundaries outside existing data are desirable, but this requires
		--  adjusted library first. Need to revisit this place later..
		if (params.timeLo < 0.0) or
		   (params.timeHi < 0.0) or
		   (params.timeLo >= data.dt * data.SegmentLen) or
		   (params.timeHi >= data.dt * data.SegmentLen)
		then
			Put_Line("Boundaries outside existing data not supported yet.");
			raise Finish;
		end if;
		--  set the sample boundaries
		if params.timeLo = 0.0 then
			--  time starts at 0 anyway, jut set the count appropriately
			params.sampleLo := 1;
		else
			params.sampleLo := time2Samples(t=>params.timeLo, dt=>data.dt);
		end if;
		if params.timeHi = 0.0 then
			params.sampleHi := data.SegmentLen;
			--  backtranslate sample to time, so that we can report it if necessary
			params.timeHi   := samples2Time(sample=>params.sampleHi, dt=>data.dt);
		else
			params.sampleHi := time2Samples(t=>params.timeHi, dt=>data.dt);
		end if;
		if params.sampleLo >= params.sampleHi then
			Put_Line("empty segment passed for analysis!!");
			raise Finish;
		end if;
	  end doChecks_and_AdjustParams;



	params : ParamRec;
-- 	abfH : ABF.Header.ABFFileHeader;

begin  -- main block
	processCommandLine(params);
	declare
		data  : ABF.Data.File_Contents := ABF.Data.ReadABFFile(To_String(params.InFile));
	begin
		doChecks_and_AdjustParams(data, params);
		if params.Debug then Put_Line("read data and adjusted params, performing calculation.."); end if;

		-- the averaging itself.
		case data.acqMode is
		  when GapFree =>
			declare
				dat : ABF.Data.DataSegment renames data.data.channel(params.dataChannel).all;
				S : ADC_UserUnits := 0.0;
			begin
				for i in params.sampleLo .. params.sampleHi loop
					S := S + dat(i);
				end loop;
				S := S / ADC_UserUnits(params.sampleHi - params.sampleLo + 1);

				--  in other acq modes output will likely have to look different
				--  (per sweep averages?), so I'll keep S and output of results locally
				if params.quiet then
					UserUnits_IO.Put(S, Fore=>3, Aft=>2, Exp=>0);
					New_Line;
				else
					Put(To_String(params.InFile) & ":  avg activ on (");
					TimeBase_IO.Put(params.timeLo/1000_000.0, Aft=>2, Exp=>0);
						--  time is in mks!
					Put(", ");
					TimeBase_IO.Put(params.timeHi/1000_000.0, Aft=>2, Exp=>0);
					Put(") = ");
					UserUnits_IO.Put(S, Fore=>3, Aft=>2, Exp=>0);
					New_Line;
				end if;
			end;

		  when EpisodicStimulation => -- not sure if this one will be special
			raise Not_Implemented;
		  when VarLengthEvents | FixedLengthEvents | Oscilloscope =>
			raise Not_Implemented;
		end case;
	end;

  exception
	when Finish => null;
	-- normal termination, in case help was invoked or other legal reason..
end Calc_NPopen;