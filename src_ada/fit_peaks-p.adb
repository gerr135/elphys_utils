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
with Ada.Command_Line; use Ada;
with GNAT.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
-- with Ada.Strings.Fixed;
with Ada.Numerics.Long_Elementary_Functions;

with ABF.Data; with ABF.Header.Waveform;
use ABF; use ABF.Data;
with Generic_Simplex;
with Golden_Section_Minimization;
with InterpFuncs_Support;


procedure Find_Peaks is


	procedure printUsage is
	begin
		Put_Line("Find peak positions and values in leak-subtracted abf file.");
		Put_Line("This program is intended to be used for analysis of NaV ion channel activity,");
		Put_Line("however you may try it on other datasets with similar layout.");
		Put_Line("Only abf input is supported for now, as we need V's and atf does not seem to keep them..");
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options] file.abf [file.peaks]");
		New_Line;
		Put_Line("options:");
		Put_Line("-h        print this help");
		Put_Line("-g        turn on debug output");
		Put_Line("-a f      specification of an interval to search, in ms, time starts at 0!!");
		Put_Line("-b f        Omitting a or b sets it to the corresponding boundary.");
		Put_Line("            Note!! -sp expects -a set to start of steps,");
		Put_Line("               and -sf expects -a set to the beginning of responce!! (often 0.1 ms later)");
		Put_Line("-n n      number of data channel, in case there are multiple (default 0)");
		Put_Line("-w n      number of Out channel on which waveform was supplied (default 0)");
		Put_Line("-s[0,p,f,s,h] smoothing modes:");
		Put_Line("             0 - no smoothing (default), overestimates peaks (as it selects absolute maximum)");
		Put_Line("             p - parabolic fit of the peak, - no rate info");
		Put_Line("             f - fit with model. Extracts rates in addition, but subject to fit convergence..");
		Put_Line("             s - fit with simplified model - no long-term inactivation, non-0 baseline.");
		Put_Line("             h - fit with Hodgkin-Huxley, no long-term inactivation, non-0 baseline.");
		New_Line;
		Put_Line("the output goes to the specified file or stdout");
	end printUsage;


	Finish,
	Not_Implemented : Exception;



	type SmoothingType is (None, Parabolic, FitFull, FitSimplified, FitHodgkinHuxley);

	type ParamRec is record
		InFile, OutFile  : Unbounded_String := Null_Unbounded_String;
		Debug     : Boolean := False;
		--
		Smoothing : SmoothingType := None;
		OnlineLS  : Boolean := False;
		--
		--  search window specification
		timeLo, timeHi : TimeBase_mks   := 0.0; -- these are set in command line
			--  no need for booleans, as 0.0 would effectively mean they are not set
			--  ATTN!! check if start supporting files that do not start at 0 time!
		sampleLo, sampleHi : SampleIndex;   -- these are set during param adjustment
		--
		--  specification of ADC and DAC channels in case there is ambiguity
		wfChannel : WaveformIndex;
		wfChannelSupplied : Boolean := False;
		dataChannel : ADC_ChannelIndex;
		dataChannelSupplied : Boolean := False;
	end record; -- ParamRec




	procedure processCommandLine(params : in out ParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		package ADC_ChannelIndex_IO is new Ada.Text_IO.Integer_IO(ADC_ChannelIndex);
		package WaveformIndex_IO is new Ada.Text_IO.Integer_IO(WaveformIndex);
		Last : Positive;

	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("a: b: g h n: w: s0 sp sf ss sh") is
				when ASCII.NUL => exit;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 'a' =>
					TimeBase_IO.Get(Parameter,params.timeLo,Last);
					params.timeLo := 1000.0*params.timeLo; -- boundaries are supplied in ms

				when 'b' =>
					TimeBase_IO.Get(Parameter,params.timeHi,Last);
					params.timeHi := 1000.0*params.timeHi; -- boundaries are supplied in ms

				when 'n' =>
					ADC_ChannelIndex_IO.Get(Parameter,params.dataChannel,Last);
					params.dataChannelSupplied := True;

				when 'w' =>
					WaveformIndex_IO.Get(Parameter,params.wfChannel,Last);
					params.wfChannelSupplied := True;

				when 's' =>
					if    Full_Switch = "s0" then params.Smoothing := None;
					elsif Full_Switch = "sp" then params.Smoothing := Parabolic;
					elsif Full_Switch = "sf" then params.Smoothing := FitFull;
					elsif Full_Switch = "ss" then params.Smoothing := FitSimplified;
					elsif Full_Switch = "sh" then params.Smoothing := FitHodgkinHuxley;
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
			S1 : String := Get_Argument(Do_Expansion => True);
			S2 : String := Get_Argument(Do_Expansion => True);
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
			if S2 /= "" then
				params.OutFile := To_Unbounded_String(S2);
			else
				params.OutFile := Null_Unbounded_String;
			end if; -- S2 /= ""
			if params.Debug then
				Put_Line("in file: " & To_String(params.InFile) &
					",  out file:" & To_String(params.OutFile) );
			end if;
		end;  -- declare S1, S2
	end processCommandLine;




	--  A simple wrapper for often used conversion
	function time2Samples(t, dt : TimeBase_mks) return SampleIndex is
	  begin
		return 1 + SampleCount(TimeBase_mks'Floor(t/dt));
	  end;
	pragma Inline(time2Samples);


	--  some initial estimates for the current peak positions for NaV channels
	--  uses physical params, rather than SampleIndex, etc, to accomodate
	--  different sampling rates or other settings
	function getNaV_MaxI_Pos(V : DAC_UserUnits)return TimeBase_mks is
		use InterpFuncs_Support;
		use Interpolation;
		NavStepVs : Vector :=
			(-70.0,  -60.0, -50.0, -40.0, -30.0, -20.0, -10.0,   0.0,  10.0,  20.0,  30.0,  40.0);
		NavPeakCenters : Vector := -- delay after step start, in mks
			(1500.0, 750.0, 550.0, 400.0, 300.0, 250.0, 220.0, 200.0, 180.0, 160.0, 150.0, 150.0);

		NavPeaks_Spline : LinearFunc :=
			Create(x => NavStepVs, y => NavPeakCenters);
		vv : Internal_Float := Internal_Float(V);
	  begin
		if vv < -70.0 then vv := -70.0; end if;
		if vv > +40.0 then vv := +40.0; end if;
-- 		if params.Debug then Put_Line("<getNaV_MaxI_Width> passed V="&vv'Img); end if;
		return TimeBase_mks(Evaluate(NavPeaks_Spline, Internal_Float(vv)));
	  end;

	function getNaV_MaxI_Width(V : DAC_UserUnits)return TimeBase_mks is
		use InterpFuncs_Support; use Interpolation;
		NavStepVs : Vector :=
			(-70.0,  -60.0, -50.0, -40.0, -30.0, -20.0, -10.0,  0.0, 10.0, 20.0, 30.0, 40.0);
		NavPeakWidths : Vector := -- half-width of fit window, in mks
			(1000.0, 250.0, 200.0, 120.0,  70.0,  60.0,  50.0, 50.0, 40.0, 40.0, 40.0, 30.0);

		NavWidth_Spline : LinearFunc := Create(x => NavStepVs, y => NavPeakWidths);
		vv : Internal_Float := Internal_Float(V);
	  begin
		if vv < -70.0 then vv := -70.0; end if;
		if vv > +40.0 then vv := +40.0; end if;
-- 		if params.Debug then Put_Line("<getNaV_MaxI_Width> passed V="&vv'Img); end if;
		return TimeBase_mks(Evaluate(NavWidth_Spline, Internal_Float(vv)));
	  end;




	procedure doChecks_and_AdjustParams(data : in ABF.Data.File_Contents;
			params : in out ParamRec) is
		use Abf.Header.Waveform;
	  begin
		--  first some data integrity checks
		if (data.acqMode /= EpisodicStimulation) then
			Put_Line("Inappropriate abf file! Need a leak subtracted NaV activity file");
			if params.Debug then
				Put_Line("acqMode="&data.acqMode'Img&"numEpisodes="&data.wf.numEpisodes'Img);
			end if;
			raise Finish;
		end if;
		--  check and set DAC channel number
		if params.wfChannelSupplied then
			if not data.wf.wfShapes.DACEnabled(params.wfChannel) then
				Put_Line("supplied DAC channel unavailable, please check parameters!");
				raise Finish;
				--  otherwise reuse params.wfChannel for the active DAC number, all is set already
			end if;
		else
			declare
				ActiveDACDetected : Boolean := False;
			begin
				for i in WaveformIndex'Range loop
					if data.wf.wfShapes.DACEnabled(i) then
						if ActiveDACDetected then
							--  multiple DAC enabled, need to ask for specification
							Put_Line("multiple DAC channels detected, please specify the one to use!");
							raise Finish;
						else
							ActiveDACDetected := True;
							params.wfChannel  := i;
						end if; -- ActiveDACDetected
					end if; -- DACEnabled
				end loop;
			end;
		end if; -- wfChannelSupplied
		if params.Debug then Put_Line("wfChannel="&params.wfChannel'Img); end if;
		--  check if we have the right waveform
		-- need epochs: prestep, steps, hold
		if    data.wf.wfShapes.EpochType(params.wfChannel) =
			(Step, Step, Step, Disabled, Disabled, Disabled, Disabled, Disabled, Disabled, Disabled)
		then
			params.OnlineLS := False;
		elsif data.wf.wfShapes.EpochType(params.wfChannel) =
			(Step, Step, Step, Step, Disabled, Disabled, Disabled, Disabled, Disabled, Disabled)
		then
			params.OnlineLS := True;
		else
			Put_Line("Inappropriate abf file! Need a leak subtracted NaV activity!");
			Put_Line("invoke program with -h for more details");
			raise Finish;
		end if;

		--  now check or detect data channel number
		if params.dataChannelSupplied then
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
		--  at this point both params.dataChannel and params.wfChannel should be set properly

		--  search window
		--  check if we got reasonable params
		if (params.timeLo < 0.0) or
		   (params.timeHi < 0.0) or
		   (params.timeLo >= data.dt * data.SegmentLen) or
		   (params.timeHi >= data.dt * data.SegmentLen)
		then
			Put_Line("supplied boundary is outside available data!!");
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
		else
			params.sampleHi := time2Samples(t=>params.timeHi, dt=>data.dt);
		end if;
		if params.sampleLo >= params.sampleHi then
			Put_Line("empty segment passed for analysis!!");
			raise Finish;
		end if;
	  end doChecks_and_AdjustParams;






	type PeakRec is record
		t : TimeBase_mks; -- "precise" position of the peak
		sampleNum : SampleIndex; -- sample # on which peak falls
		value : ADC_UserUnits;   -- value of the peak
		V     : DAC_UserUnits;   -- voltage at which this peak happens
	end record;
	type PeakArray is array(SweepIndex range <>) of PeakRec;


	params : ParamRec;
-- 	abfH : ABF.Header.ABFFileHeader;

begin  -- main block
	processCommandLine(params);
	declare
		data  : ABF.Data.File_Contents := ABF.Data.ReadABFFile(To_String(params.InFile));
		peaks : PeakArray(1 .. data.numSweeps);
	begin
		doChecks_and_AdjustParams(data, params);
		--  now extract the proper segments and find the maximum
		for nSwp in 1 .. data.numSweeps loop
		  declare
			dataSeg : ABF.Data.DataSegment renames data.data.chn(params.dataChannel).swp(nSwp).all;
			swpV : DAC_UserUnits; -- V at the sweep being processed
				--  the peak should be during the steps interval anyway
		  begin
			if params.OnlineLS then
				swpV := -getWFLevel(data, params.wfChannel, nSwp, atEpoch => EpochIndex'First + 2);
					--  '-' to use physiological definition on V
			else
				swpV := -getWFLevel(data, params.wfChannel, nSwp, atEpoch => EpochIndex'First + 1);
			end if;
			--
			case params.Smoothing is
			  when None    =>
				declare
					maxVal : ADC_UserUnits;
					maxIndex : SampleIndex;
				begin
					--  perform simple sequential search for a max
					maxVal := dataSeg(params.sampleLo);
					maxIndex := params.sampleLo;
					for i in params.sampleLo .. params.sampleHi loop
						if dataSeg(i) > maxVal then
							maxIndex := i;
							maxVal   := dataSeg(i);
						end if;
					end loop;
					peaks(nSwp).sampleNum := maxIndex;
					peaks(nSwp).t := data.dt*maxIndex - data.dt*0.5; -- middle of interval
					peaks(nSwp).Value     := maxVal;
					peaks(nSwp).V := swpV;
				end;

			---------------------------------------------------------
			  when Parabolic => -- fit with a*(t-t0)^2+c
				declare
					t0   : TimeBase_mks := getNaV_MaxI_Pos(swpV) + params.timeLo;
					Ind0 : SampleIndex  := time2Samples( t=>t0, dt=>data.dt);
					dInd : SampleIndex  :=
						time2Samples(t=>getNaV_MaxI_Width(swpV), dt=>data.dt)
						- 1; -- to compensate the +1 in conversion function

					type ParamIndex is (a, x0, c);
					type ParamVector is array(ParamIndex) of Long_Float;

					function Chi2_from_parabola(v:ParamVector) return Long_Float is

						function parabola(t : TimeBase_mks; v : ParamVector)return Long_Float is
						  begin
							return v(a) * (Long_Float(t) - v(x0))**2 + v(c);
						  end parabola;

						S : Long_Float := 0.0;
					  begin
						for i in Ind0 - dInd .. Ind0 + dInd loop
							S := S + (Long_Float(dataSeg(i)) - parabola(i*data.dt, v))**2;
						end loop;
						return S;
					  end Chi2_from_parabola;

					procedure Simplex is new Generic_Simplex(Long_Float,
						ParamIndex, ParamVector, goalFunc => Chi2_from_parabola);

					--  initial params and inital step
					v  : ParamVector := (a=>Long_Float(-dataSeg(Ind0)) ,
						x0=>Long_Float(t0), c=>Long_Float(dataSeg(Ind0)));
					dv : ParamVector := (a=>v(a)/10.0, x0=>v(x0)/100.0, c=>v(c)/100.0);

					eps : Long_Float := Chi2_from_parabola(v) / 10_000.0;
					MaxIters : Positive := 10000;

				begin
					if params.Debug then
						Put("parab fit, seeds: t0=");TimeBase_IO.Put(t0,Aft=>1,Exp=>0);
						Put(", Ind0="&Ind0'Img & ", dInd="&dInd'Img);
						New_Line;
					end if;
					Simplex(v, dv, eps, MaxIters);
					--  save fit results
					peaks(nSwp).t := TimeBase_mks(v(x0));
					peaks(nSwp).sampleNum := time2Samples(t=>TimeBase_mks(v(x0)), dt=>data.dt);
					peaks(nSwp).Value := ADC_UserUnits(v(c));
					peaks(nSwp).V := swpV;
				end; -- case Parabolic

			------------------------------------------------------------
			  when FitFull  =>
				-- fit with A * (1 - exp(-((t-t0)/t_on)^P) * (l - (1-l)*exp(-(t-t0)/t_off))
				declare
					--  t0 is fixed - a start of interval
					t0 : TimeBase_mks := data.dt*(params.sampleLo)-0.1;
					use Ada.Numerics.Long_Elementary_Functions;

					type ParamIndex is (t_on, t_off, A, l, P);
					type ParamVector is array(ParamIndex) of Long_Float;

					--  the relation that is supposed to describe NaV channel peak
					--  has steep onset..
					--  this function returns *negative* values, '-' of the normal definition
					--  since we want to use minimization proc to find its extremum..
					function CustomFunc(t : TimeBase_mks; v : ParamVector)return Long_Float is
						tau_on  : Long_Float := v(t_on);
						tau_off : Long_Float renames v(t_off);
						guard : Long_Float := 1.0; -- to put some weight on bad param ranges
					  begin
						--  t_on is unstable at high V's, need to make sure it does not go below 0
						if tau_on < 0.0 then
							guard := 100.0*exp(-tau_on);
							tau_on := 0.0;
						end if;
						return (v(A) * (1.0 - exp( -(Long_Float(t - t0)/tau_on)**v(P) )) *
							(v(l) + (1.0 - v(l))*exp( -Long_Float(t - t0)/tau_off )))
							* guard;
					  end CustomFunc;

					function Chi2_from_Func(v:ParamVector) return Long_Float is
						S : Long_Float := 0.0;
					  begin
						for i in params.sampleLo .. params.sampleHi loop
							S := S + (Long_Float(dataSeg(i)) - CustomFunc(i*data.dt, v))**2;
						end loop;
						return S;
					  end Chi2_from_Func;

					procedure Simplex is new Generic_Simplex(Long_Float,
						ParamIndex, ParamVector, goalFunc => Chi2_from_Func);

					--  initial params and inital step
					v  : ParamVector := (t_on=>100.0, t_off=>300.0,
						 A=>2.5*Long_Float(dataSeg(params.sampleLo + 30)), l=>0.01, P=>1.5);
						 --  that 30 is usually 300 us..
					dv : ParamVector := (t_on=>10.0, t_off=>30.0,
						 A=>v(A)/20.0, l=>0.001, P=>0.001);

					eps : Long_Float := Chi2_from_Func(v) / 10_000.0;
					MaxIters : Positive := 10000;

					function  Custom2Minimize(x:Long_Float) return Long_Float is
					  begin
						return -CustomFunc(TimeBase_mks(x),v);
					  end;

					procedure CustomFunc_Min is new Golden_Section_Minimization(
						Real=>Long_Float, func=>Custom2Minimize);

					use Long_Float_Text_IO;

				begin
					if v(A) < 3.0 then v(A) := 3.0; end if; -- to avoid bad seeding at low amplitudes
					if params.Debug then
						Put("seeds: A=");Put(v(A),Aft=>1,Exp=>0);
						New_Line;
					end if;
					Simplex(v, dv, eps, MaxIters);

					--  print fit results
					Put("V="); DACUserUnits_IO.Put(swpV, Fore=>3,Aft=>1,Exp=>0);
					Put(",  A(pA)=");    Put(v(A),    Fore=>4,Aft=>1,Exp=>0);
					Put(",  t_off(ms)=");Put(v(t_off)/1000.0, Aft=>3,Exp=>0);
					Put(",  t_on(ms)="); Put(v(t_on)/1000.0,  Aft=>3,Exp=>0);
					Put(",  P=");    Put(v(P),    Aft=>3,Exp=>0);
					Put(",  l=");    Put(v(l),    Aft=>3,Exp=>0);
					Put(",  Niter="&MaxIters'Img);
					Put(",  stddev=");
					Put( sqrt(Chi2_from_Func(v))/Long_Float(params.sampleHi - params.sampleLo),
						Aft=>3,Exp=>0);
					New_Line;

					--  convert fit coeffs into time and amplitude of a peak
					--  no precise analytical solution, so we need to run minimization here..
					eps := 0.1; -- in mks this time, reuse var with similar meaning
					CustomFunc_Min(
						a => Long_Float(params.sampleLo*data.dt),
						b => Long_Float(params.sampleHi*data.dt), -- timeLo/Hi may be undefined
						MaxIter => MaxIters, eps => eps,
						x => Long_Float(peaks(nSwp).t),
						f => Long_Float(peaks(nSwp).Value) );

					peaks(nSwp).Value := -peaks(nSwp).Value; -- function was negated for minimization
					peaks(nSwp).sampleNum := time2Samples(t=>peaks(nSwp).t, dt=>data.dt);
					peaks(nSwp).V := swpV;
				end; -- case FitFull

			------------------------------------------------------------
			  when FitSimplified  =>
				-- fit with A * (1 - exp(-((t-t0)/t_on)^P) * exp(-(t-t0)/t_off) + C
				declare
					--  t0 is fixed - a start of interval
					t0 : TimeBase_mks := data.dt*(params.sampleLo)-0.1;
					use Ada.Numerics.Long_Elementary_Functions;

					type ParamIndex is (t_on, t_off, A, P, C);
					type ParamVector is array(ParamIndex) of Long_Float;

					--  the relation that is supposed to describe NaV channel peak
					--  has steep onset..
					--  this function returns *negative* values, '-' of the normal definition
					--  since we want to use minimization proc to find its extremum..
					function CustomFunc(t : TimeBase_mks; v : ParamVector)return Long_Float is
						tau_on  : Long_Float := v(t_on);
						tau_off : Long_Float renames v(t_off);
						guard   : Long_Float := 1.0; -- to put some weight on bad param ranges
					  begin
						--  t_on is unstable at high V's, need to make sure it does not go below 0
						if tau_on < 0.0 then
							guard := 100.0*exp(-tau_on);
							tau_on := 0.0;
						end if;
						return (v(A) * (1.0 - exp( -(Long_Float(t - t0)/tau_on)**v(P) ))
							*exp( -Long_Float(t - t0)/tau_off ) + v(C))
							* guard;
					  end CustomFunc;

					function Chi2_from_Func(v:ParamVector) return Long_Float is
						S : Long_Float := 0.0;
					  begin
						for i in params.sampleLo .. params.sampleHi loop
							S := S + (Long_Float(dataSeg(i)) - CustomFunc(i*data.dt, v))**2;
						end loop;
						return S;
					  end Chi2_from_Func;

					procedure Simplex is new Generic_Simplex(Long_Float,
						ParamIndex, ParamVector, goalFunc => Chi2_from_Func);

					--  initial params and inital step
					v  : ParamVector := (t_on=>100.0, t_off=>300.0,
						 A=>2.5*Long_Float(dataSeg(params.sampleLo + 30)), P=>1.5,
						 C=>Long_Float(dataSeg(params.sampleHi)));
						 --  that 30 is usually 300 us..
					dv : ParamVector := (t_on=>10.0, t_off=>30.0,
						 A=>v(A)/20.0, P=>0.001, C=>1.0);

					eps : Long_Float := Chi2_from_Func(v) / 10_000.0;
					MaxIters : Positive := 10000;

					function  Custom2Minimize(x:Long_Float) return Long_Float is
					  begin
						return -CustomFunc(TimeBase_mks(x),v);
					  end;

					procedure CustomFunc_Min is new Golden_Section_Minimization(
						Real=>Long_Float, func=>Custom2Minimize);

					use Long_Float_Text_IO;

				begin
					if v(A) < 3.0 then v(A) := 3.0; end if; -- to avoid bad seeding at low amplitudes
					if params.Debug then
						Put("seeds: A=");Put(v(A),Aft=>1,Exp=>0);
						New_Line;
					end if;
					Simplex(v, dv, eps, MaxIters);

					--  print fit results
					Put("V="); DACUserUnits_IO.Put(swpV, Fore=>3,Aft=>1,Exp=>0);
					Put(",  A(pA)=");    Put(v(A),    Fore=>4,Aft=>1,Exp=>0);
					Put(",  t_off(ms)=");Put(v(t_off)/1000.0, Aft=>3,Exp=>0);
					Put(",  t_on(ms)="); Put(v(t_on)/1000.0,  Aft=>3,Exp=>0);
					Put(",  P=");    Put(v(P),    Aft=>3,Exp=>0);
					Put(",  C=");    Put(v(C),    Aft=>3,Exp=>0);
					Put(",  Niter="&MaxIters'Img);
					Put(",  stddev=");
					Put( sqrt(Chi2_from_Func(v))/Long_Float(params.sampleHi - params.sampleLo),
						Aft=>3,Exp=>0);
					New_Line;

					--  convert fit coeffs into time and amplitude of a peak
					--  no precise analytical solution, so we need to run minimization here..
					eps := 0.1; -- in mks this time, reuse var with similar meaning
					CustomFunc_Min(
						a => Long_Float(params.sampleLo*data.dt),
						b => Long_Float(params.sampleHi*data.dt), -- timeLo/Hi may be undefined
						MaxIter => MaxIters, eps => eps,
						x => Long_Float(peaks(nSwp).t),
						f => Long_Float(peaks(nSwp).Value) );

					peaks(nSwp).Value := -peaks(nSwp).Value; -- function was negated for minimization
					peaks(nSwp).sampleNum := time2Samples(t=>peaks(nSwp).t, dt=>data.dt);
					peaks(nSwp).V := swpV;
				end; -- case FitSimplified

			------------------------------------------------------------
			  when FitHodgkinHuxley  =>
				-- fit with A * (1 - exp(-((t-t0)/t_on)))^P * exp(-(t-t0)/t_off) + C
				declare
					--  t0 is fixed - a start of interval
					t0 : TimeBase_mks := data.dt*(params.sampleLo)-0.1;
					use Ada.Numerics.Long_Elementary_Functions;

					type ParamIndex is (t_on, t_off, A, P, C);
					type ParamVector is array(ParamIndex) of Long_Float;

					--  the relation that is supposed to describe NaV channel peak
					--  has steep onset..
					--  this function returns *negative* values, '-' of the normal definition
					--  since we want to use minimization proc to find its extremum..
					function CustomFunc(t : TimeBase_mks; v : ParamVector)return Long_Float is
						tau_on  : Long_Float := v(t_on);
						tau_off : Long_Float renames v(t_off);
						guard   : Long_Float := 1.0; -- to put some weight on bad param ranges
					  begin
						--  t_on is unstable at high V's, need to make sure it does not go below 0
						if tau_on < 0.0 then
							guard := 100.0*exp(-tau_on);
							tau_on := 0.0;
						end if;
						return (v(A) * (1.0 - exp( -Long_Float(t - t0)/tau_on ))**v(P)
							* exp( -Long_Float(t - t0)/tau_off ) + v(C))
							* guard;
					  end CustomFunc;

					function Chi2_from_Func(v:ParamVector) return Long_Float is
						S : Long_Float := 0.0;
					  begin
						for i in params.sampleLo .. params.sampleHi loop
							S := S + (Long_Float(dataSeg(i)) - CustomFunc(i*data.dt, v))**2;
						end loop;
						return S;
					  end Chi2_from_Func;

					procedure Simplex is new Generic_Simplex(Long_Float,
						ParamIndex, ParamVector, goalFunc => Chi2_from_Func);

					--  initial params and inital step
					v  : ParamVector := (t_on=>100.0, t_off=>300.0,
						 A=>2.5*Long_Float(dataSeg(params.sampleLo + 30)), P=>1.5,
						 C=>Long_Float(dataSeg(params.sampleHi)));
						 --  that 30 is usually 300 us..
					dv : ParamVector := (t_on=>10.0, t_off=>30.0,
						 A=>v(A)/20.0, P=>0.001, C=>1.0);

					eps : Long_Float := Chi2_from_Func(v) / 10_000.0;
					MaxIters : Positive := 10000;

					function  Custom2Minimize(x:Long_Float) return Long_Float is
					  begin
						return -CustomFunc(TimeBase_mks(x),v);
					  end;

					procedure CustomFunc_Min is new Golden_Section_Minimization(
						Real=>Long_Float, func=>Custom2Minimize);

					use Long_Float_Text_IO;

				begin
					if v(A) < 3.0 then v(A) := 3.0; end if; -- to avoid bad seeding at low amplitudes
					if params.Debug then
						Put("seeds: A=");Put(v(A),Aft=>1,Exp=>0);
						New_Line;
					end if;
					Simplex(v, dv, eps, MaxIters);

					--  print fit results
					Put("V="); DACUserUnits_IO.Put(swpV, Fore=>3,Aft=>1,Exp=>0);
					Put(",  A(pA)=");    Put(v(A),    Fore=>4,Aft=>1,Exp=>0);
					Put(",  t_off(ms)=");Put(v(t_off)/1000.0, Aft=>3,Exp=>0);
					Put(",  t_on(ms)="); Put(v(t_on)/1000.0,  Aft=>3,Exp=>0);
					Put(",  P=");    Put(v(P),    Aft=>3,Exp=>0);
					Put(",  C=");    Put(v(C),    Aft=>3,Exp=>0);
					Put(",  Niter="&MaxIters'Img);
					Put(",  stddev=");
					Put( sqrt(Chi2_from_Func(v))/Long_Float(params.sampleHi - params.sampleLo),
						Aft=>3,Exp=>0);
					New_Line;

					--  convert fit coeffs into time and amplitude of a peak
					--  no precise analytical solution, so we need to run minimization here..
					eps := 0.1; -- in mks this time, reuse var with similar meaning
					CustomFunc_Min(
						a => Long_Float(params.sampleLo*data.dt),
						b => Long_Float(params.sampleHi*data.dt), -- timeLo/Hi may be undefined
						MaxIter => MaxIters, eps => eps,
						x => Long_Float(peaks(nSwp).t),
						f => Long_Float(peaks(nSwp).Value) );

					peaks(nSwp).Value := -peaks(nSwp).Value; -- function was negated for minimization
					peaks(nSwp).sampleNum := time2Samples(t=>peaks(nSwp).t, dt=>data.dt);
					peaks(nSwp).V := swpV;
				end; -- case FitSimplified
			end case;
		  end;
		end loop; -- sweeps

		--  and print the results
		Put_Line("maxima positions (Smoothing="&params.Smoothing'Img&"):");
		Put_Line("     V,     Value,         t,    sample#");
		for nSwp in 1 .. data.numSweeps loop
			--  V's are in elphys "notation" (pipette V) and these are cell attached patches,
			--  converting to physiological (V => -V)
			DACUserUnits_IO.Put(peaks(nSwp).V, Fore=>3, Aft=>2, Exp=>0);
			Put(",   ");
			UserUnits_IO.Put(peaks(nSwp).Value, Fore=>4, Aft=>2, Exp=>0);
			Put(",   ");
			TimeBase_IO.Put(peaks(nSwp).t/1000.0, Fore=>3, Aft=>3, Exp=>0);
			Put(",   ");
			Put(peaks(nSwp).sampleNum'Img);
			New_Line;
		end loop;
	end; -- data block

  exception
	when Finish => null;
	-- normal termination, in case help was invoked or other legal reason..
end Find_Peaks;
