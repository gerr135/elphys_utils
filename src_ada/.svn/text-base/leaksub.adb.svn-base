with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with GNAT.Command_Line;
with Ada.Numerics.Generic_Elementary_Functions;

with ABF.Data; with ABF.Header.Waveform;
use ABF; use ABF.Data;


procedure LeakSub is


	procedure printUsage is
	begin
		Put_Line("This program performs leak subtraction for NaV specific protocol.");
		Put_Line("It expects a waveform consisting of:");
		Put_Line("hold, pre-pulse, steps to var voltages, hold, step to sampling V, hold");
		Put_Line("The current at sampling V is scaled and subtracted.");
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options] file.abf [file.out]");
		New_Line;
		Put_Line("options:");
		Put_Line("-h        print this help");
		Put_Line("-g        turn on debug output");
		New_Line;
		Put_Line("-a        subtract averaged leaks - control step is the same amplitude,");
		Put_Line("            so we can get smoother leaks. Use with stable gigaseals.");
		Put_Line("-c        crop subtraction, so that it does not overflow (default: no cropping,");
		Put_Line("            (scaled sampling can overflow range and subtract too much)");
		Put_Line("-v       detect and adjust dV - uncompensated Vm.");
		Put_Line("            dV gets calculated from prepulse-step pairs with 1 sweep (of highest prepulse I) thrown out");
		Put_Line("            (to get some protection from Cl channels)");
		New_Line;
		Put_Line("-l{c,p,s} use only pre-step (p) or control (c) or step tail (s) for calculating leak R.");
-- 		Put_Line("            (-lp disables -v, since apparently control is not complelty trusted)");
		Put_Line("-s f      shift all starts by f ms - to compensate protocol delay (default: 1.55 ms)");
		Put_Line("-d f      duration of padding interval (default 5 ms)");
		New_Line;
		Put_Line("-n n      number of data channel, in case there are multiple (default 0)");
		Put_Line("-w n      number of Out channel on which waveform was supplied (default 0)");
		Put_Line("-o{b,t,p} output mode: b - abf, t - atf and p - basic csv format.");
		Put_Line("            Default t - atf file.");
	end printUsage;


	--  some "terminology" that will be used in var names and referring to the protocol
	--  "steps" will refer to epoch 3 - sampling steps to various V's
	--  "leak controls" refer to epoch 5 - small step to samle leaks..


	Finish,
	Not_Implemented : Exception;


	type OutputFormat is (ABFFile, ATFFile, PlotDump);
	type LeaksSource is (PrePulse, ControlTail, Both);

	type ParamRec is record
		InFile, OutFile  : Unbounded_String := Null_Unbounded_String;
		outFormat : OutputFormat := ATFFile;

		AverageLeaks : Boolean := False;
			--  whether to subtract leak controls individually or averaged over sweeps
		AdjustV : Boolean := False;
			--  whether to detect uncompensated Vm and adjust V's accordingly
			--  changed default to False, since errors are too big more often than not
		CalcLeaksFrom : LeaksSource := Both;
			--  some of the traces with excess Cl channel activity can be resqued
			--  by calculating leaks and averages from the end of control pulse
			--  instead of pre-pulse region (which is pretty dirty if Cl's go off)
			--  while others may have screwy control pulse -
			--  so only pre-pulse should be used
		CropLeaks : Boolean := False;
			--  whether to crop maximum values of leak controls after rescaling
			--  (to within the acquisition equipment range)
		Debug     : Boolean := False;

		padDuration : TimeBase_mks := 5000.0; -- 5 ms
			--  duration of the pad interval kept before/after section of interest
			--  all averages are done ovr this duration (in all regions)

		onsetDelay  : TimeBase_mks := 1550.0; -- this seems to be universal for Axon-generated protocols
			--  delay between given onset of step in waveform (as calculated from samples*dt)
			--  and the time it commences for real (Digidata specific?)

		--  specification of ADC and DAC channels in case there is ambiguity
		wfChannel : WaveformIndex;
		wfChannelSupplied : Boolean := False;
		dataChannel : ADC_ChannelIndex;
		dataChannelSupplied : Boolean := False;
	end record; -- ParamRec





	function processCommandLine return ParamRec is
		use GNAT.Command_Line; use Ada.Command_Line;
		package ADC_ChannelIndex_IO is new Ada.Text_IO.Integer_IO(ADC_ChannelIndex);
		package WaveformIndex_IO is new Ada.Text_IO.Integer_IO(WaveformIndex);

		Last : Positive;
		params : ParamRec;

	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
		  Options:
		  loop
			case Getopt ("a c d: g h lc lp n: ob op ot s: v w:") is
				when ASCII.NUL => exit;

				when 'a' => params.AverageLeaks := True;

				when 'c' => params.CropLeaks := True;

				when 'd' =>
					TimeBase_IO.Get(Parameter,params.padDuration,Last);
					--  user is expected to enter ms, not mks
					params.padDuration := params.padDuration * 1000.0;

				when 'g' => params.Debug := True;
				when 'h' => printUsage; raise Finish;

				when 'l' =>
					if    Full_Switch = "lp" then
						params.CalcLeaksFrom := PrePulse;
						params.AdjustV := False; -- as described in usage text
					elsif Full_Switch = "lc" then
						params.CalcLeaksFrom := ControlTail;
					end if;

				when 'n' =>
					ADC_ChannelIndex_IO.Get(Parameter,params.dataChannel,Last);
					params.dataChannelSupplied := True;

				when 'o' =>
					if    Full_Switch = "ob" then params.outFormat := ABFFile;
					elsif Full_Switch = "ot" then params.outFormat := ATFFile;
					elsif Full_Switch = "op" then params.outFormat := PlotDump;
					else raise Invalid_Switch; end if;

				when 's' =>
					TimeBase_IO.Get(Parameter,params.onsetDelay,Last);
					--  user is expected to enter ms, not mks
					params.onsetDelay := params.onsetDelay * 1000.0;

				when 'w' =>
					WaveformIndex_IO.Get(Parameter,params.wfChannel,Last);
					params.wfChannelSupplied := True;

				when 'v' => params.AdjustV := True;

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
		  end loop Options;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error        => Put_Line ("Invalid numeric format for switch " & Full_Switch);raise Finish;
		end; -- params loop

		declare
			use Ada.Strings.Fixed;
			S1 : String := Get_Argument(Do_Expansion => True);
			S2 : String := Get_Argument(Do_Expansion => True);
		begin
			if S1 /= "" then
				params.InFile := To_Unbounded_String(S1);
			else
				Put_Line("Please supply a name of abf file to perform leak subtraction!");
				raise Finish;
			end if;
			if S2 /= "" then
				params.OutFile := To_Unbounded_String(S2);
			else
				case params.outFormat is
				  when ATFFile =>
					if S1(S1'Last-3 .. S1'Last) = ".abf" then
						--  just change extension to .atf
						Overwrite(S1, Position=>S1'Last-2, New_Item=>"atf");
						params.OutFile := To_Unbounded_String(S1);
					else
						--  or simply add .atf at the end
						params.OutFile := To_Unbounded_String(S1 & ".atf");
					end if;
				  when ABFFile =>
					if S1(S1'Last-3 .. S1'Last) = ".abf" then
						--  we insert _ls right before extension by default
						params.OutFile := To_Unbounded_String(
							S1(1 .. S1'Last-4) & "_ls.abf");
					else
						params.OutFile := To_Unbounded_String(S1 & "_ls.abf");
					end if;
				  when PlotDump =>
					if S1(S1'Last-3 .. S1'Last) = ".abf" then
						--  just change extension to .plt
						Overwrite(S1, Position=>S1'Last-2, New_Item=>"plt");
						params.OutFile := To_Unbounded_String(S1);
					else
						--  or simply add .atf at the end
						params.OutFile := To_Unbounded_String(S1 & ".plt");
					end if;
				end case;
			end if; -- S2 /= ""
			if params.Debug then
				Put_Line(Standard_Error, "in file: " & To_String(params.InFile) &
					",  out file:" & To_String(params.OutFile) );
			end if;
		end; -- processing arguments
		return params;
	end processCommandLine;





	procedure doChecks_and_AdjustParams(data : in ABF.Data.File_Contents;
			params : in out ParamRec) is
		use Abf.Header.Waveform;
	  begin
		--  first some consistency checks
		if (data.acqMode /= EpisodicStimulation) then
			Put_Line("Inappropriate abf file! Need an episodic file with a specific protocol!");
			if params.Debug then
				Put_Line(Standard_Error, "acqMode="&data.acqMode'Img&"numEpisodes="&data.wf.numEpisodes'Img);
			end if;
			raise Finish;
		end if;
		--  check and set the DAC channel number
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
		--  check if we have the right waveform
		-- need epochs: hold, prestep, steps, hold, leak test, hold
		if data.wf.wfShapes.EpochType(params.wfChannel) /=
			(Step, Step, Step, Step, Step, Step, Disabled, Disabled, Disabled, Disabled)
		then
			Put_Line("Inappropriate abf file! Need an episodic file with a specific protocol!");
			Put_Line("invoke program with -h for more details");
			raise Finish;
		end if;
		if data.wf.wfShapes.EpochInitDuration(params.wfChannel)(2) /= -- sampling steps
		data.wf.wfShapes.EpochInitDuration(params.wfChannel)(4)    -- control step
		then
			Put_Line("The duration of control step should equal the duration of sampling steps!");
			raise Finish;
		end if;
		if params.Debug then Put_Line(Standard_Error, "wfChannel="&params.wfChannel'Img); end if;

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
		if params.Debug then Put_Line(Standard_Error, "dataChannel="&params.dataChannel'Img); end if;
		--  at this point both params.dataChannel and params.wfChannel should be set properly
	  end doChecks_and_AdjustParams;




	--  Calculate uncompensated Vm (dV) and adjust waveform V's to reflect it
	--  Need to adjust in both old and new data structures, so passing in both..
	procedure AdjustVm(data, newData : in out ABF.Data.File_Contents; params : in ParamRec;
			stepsOffset, stepsDuration, padInterval : in SampleCount) is

		function "*"(Left: ADC_UserUnits; Right: DAC_UserUnits) return DAC_UserUnits is
		begin
			return DAC_UserUnits(Left) * Right;
		end;
		pragma Inline("*");

		--  The dV is calculated from the prepulse-step IV pairs (1-2 region tails)
		--  First the sweep with highest I1 is discarded, as a simplistic guard against
		--  pollution by Cl current (they are always upwards)
		--  Then the dV's are averaged and the result is added to original waveform
		V1 : DAC_UserUnits := data.wf.wfShapes.EpochInitLevel(params.wfChannel)(EpochIndex'First + 1);
		V2 : DAC_UserUnits; -- this one changes from sweep to sweep, need a "real" var
		V2_init : DAC_UserUnits := data.wf.wfShapes.EpochInitLevel(params.wfChannel)(EpochIndex'First + 2);
		V2_step : DAC_UserUnits := data.wf.wfShapes.EpochLevelInc (params.wfChannel)(EpochIndex'First + 2);
		I1, I2 : SweepWide_ADCUU(1 .. data.numSweeps); -- average currents
		dV : DAC_UserUnits; dV_S : DAC_UserUnits := 0.0;
		R  : Resistance; R_S : Resistance := 0.0;

		maxI1 : ADC_UserUnits := ADC_UserUnits'First;
		maxI1_Index : SweepIndex;

		use UserUnits_IO;  use DACUserUnits_IO;
		use Resistance_IO; use SweepIndex_IO;

	  begin  -- AdjustVm
		-- prepare the averages and find max I1
		for nSwp in 1 .. data.numSweeps loop
			I1(nSwp) := 0.0; I2(nSwp) := 0.0;
			for i in 1 .. padInterval loop
				I1(nSwp) := I1(nSwp) +
					data.data.chn(params.dataChannel).swp(nSwp)(i + stepsOffset - padInterval);
				I2(nSwp) := I2(nSwp) +
					data.data.chn(params.dataChannel).swp(nSwp)
					(i + stepsOffset + stepsDuration - padInterval);
			end loop;
			I1(nSwp) := I1(nSwp) / ADC_UserUnits(padInterval);
			I2(nSwp) := I2(nSwp) / ADC_UserUnits(padInterval);
			--  find maxI1
			if I1(nSwp) > maxI1 then
				maxI1 := I1(nSwp);
				maxI1_Index := nSwp;
			end if;
		end loop; -- sweeps

		--  separate loop is necessary because we are skipping one sweep
		for nSwp in 1 .. data.numSweeps loop
			if nSwp /= maxI1_Index then
				V2 := V2_init + DAC_UserUnits(nSwp - 1) * V2_step;
				dV := (I2(nSwp)*V1 - I1(nSwp)*V2)/DAC_UserUnits(I1(nSwp) - I2(nSwp));
				R  := (V2 - V1) / (I2(nSwp) - I1(nSwp));
				dV_S := dV_S + dV;
				R_S  := R_S + R;

				if params.Debug then
					Put(Standard_Error, "dV[");
					Put(Standard_Error, nSwp, Width=>2);
					Put(Standard_Error, "] = ");
					Put(Standard_Error, dV, Fore=>3, Aft=>2, Exp=>0);
					Put(Standard_Error, "   (V, I)'s: (");
					Put(Standard_Error, V1, Fore=>3, Aft=>1, Exp=>0);
					Put(Standard_Error, ", ");
					Put(Standard_Error, I1(nSwp), Fore=>3, Aft=>2,Exp=>0);
					Put(Standard_Error, "),   (");
					Put(Standard_Error, V2, Fore=>3, Aft=>1, Exp=>0);
					Put(Standard_Error, ", ");
					Put(Standard_Error, I2(nSwp), Fore=>3, Aft=>2,Exp=>0);
					Put_Line(Standard_Error, ")");
				end if; -- Debug
			end if; -- index match
		end loop;

		dV_S := dV_S / DAC_UserUnits(data.numSweeps - 1);
		R_S  := R_S  / Resistance(data.numSweeps - 1);
		--  R and dV are nice to have always, will just dump them to stderr for now..
		Put(Standard_Output, "<dV> = ");
		Put(Standard_Output, dV_S, Fore=>3, Aft=>2, Exp=>0);
		Put(Standard_Output, ";   <R> = ");
		Put(Standard_Output, R_S, Fore=>2, Aft=>2, Exp=>0);
		New_Line(Standard_Output);

		for i in EpochIndex'Range loop
			data.wf.wfShapes.EpochInitLevel(params.wfChannel)(i) :=
				data.wf.wfShapes.EpochInitLevel(params.wfChannel)(i) + dV_S;
			newData.wf.wfShapes.EpochInitLevel(params.wfChannel)(i) :=
				NewData.wf.wfShapes.EpochInitLevel(params.wfChannel)(i) + dV_S;
		end loop;
	  end;


	--  Initializes a new struct and fills it with relevant data from the source
	function  CopySlice(Source : File_Contents;
		NumWFChannel : WaveformIndex; -- active waveform channel number
		offset, duration, padInterval : SampleCount) return File_Contents is

		Destination : File_Contents(acqMode => EpisodicStimulation,
			numChannels => Source.numChannels,
			numSweeps   => Source.numSweeps,
			SegmentLen  => padInterval + duration + padInterval
		);

		use Abf.Header.Waveform;
		--  a shorthand, as we will be doing a few assignments to this one
		wfs : Waveform_Shapes renames Destination.wf.wfShapes;

	  begin
		--  generic fields
		Destination.dt := Source.dt;
		Destination.ADCMin := Source.ADCMin;
		Destination.ADCMax := Source.ADCMax;
		Destination.comment := Source.comment;
		Destination.channelName := Source.channelName;
		Destination.units       := Source.units;
		--  create an appropriate waveform
		Destination.wf.numRuns     := Source.wf.numRuns;
		Destination.wf.numEpisodes := Source.wf.numEpisodes;
		Destination.wf.NumSamplesPerEpisode := Destination.SegmentLen;
		wfs := Source.wf.wfShapes;
		wfs.EpochType(NumWFChannel) :=
			(Step, Step, Step, Disabled, Disabled, Disabled, Disabled, Disabled, Disabled, Disabled);
		wfs.EpochInitLevel(NumWFChannel) :=
			(wfs.EpochInitLevel(NumWFChannel)(EpochIndex'First + 1), -- hyperpolarizing prepulse
			 wfs.EpochInitLevel(NumWFChannel)(EpochIndex'First + 2), -- steps
			 wfs.EpochInitLevel(NumWFChannel)(EpochIndex'First + 3), -- holding after steps
			 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
		wfs.EpochLevelInc(NumWFChannel) :=
			(0.0, wfs.EpochLevelInc(NumWFChannel)(EpochIndex'First + 2),
			 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
		wfs.EpochInitDuration(NumWFChannel) :=
			(padInterval, duration, padInterval,
			 0, 0, 0, 0, 0, 0, 0);
		wfs.EpochDurationInc(NumWFChannel)  := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

		return Destination;
	  end CopySlice;






	params : ParamRec := processCommandLine;


	data : ABF.Data.File_Contents := ABF.Data.ReadABFFile(To_String(params.InFile));
	abfH : ABF.Header.ABFFileHeader := ABF.Header.Read_Header(To_String(params.InFile));
		--  this one is needed for safer abf output,
		--  in principle we can completely reconstruct a header for writing out..


begin -- main
	--  we need to track some params that are not saved in either of "stndard" Axon formats (.abf, .atf)
	--  Lets try to keep obligatory output down to single line. Dump this to stderr for now..
	--  Start by printing file name..
-- 	Put(Standard_Output, To_String(params.InFile) & Latin_1.HT);

	doChecks_and_AdjustParams(data, params);

	declare
		--  determine the size of new data and allocate new struct
		--  we skeep hold, pre-pulse and subtraction regions and keep padDuration ms on each side..
		padInterval : SampleCount := SampleCount(
			TimeBase_mks'Floor(params.padDuration / data.dt));
		onsetSeek   : SampleCount := SampleCount(
			TimeBase_mks'Floor(params.onsetDelay / data.dt));

		stepsOffset   : SampleCount :=  -- EpochIndex is 0-based
			onsetSeek +
			data.wf.wfShapes.EpochInitDuration(params.wfChannel)(0) +
			data.wf.wfShapes.EpochInitDuration(params.wfChannel)(1);
		stepsDuration : SampleCount :=  -- This is a duration, not offset!!!
			data.wf.wfShapes.EpochInitDuration(params.wfChannel)(2);
		controlOffset : SampleCount :=  -- onsetSeek is already added via stepsOffset !!!
			stepsOffset + stepsDuration +
			data.wf.wfShapes.EpochInitDuration(params.wfChannel)(3);

		--  average of control epochs
		controlSlice : DataSegment (1 ..
				1 + padInterval + stepsDuration + padInterval) := (others => 0.0);

		newData : File_Contents := CopySlice(Source => data,
			NumWFChannel => params.wfChannel, offset => stepsOffset,
			duration => stepsDuration, padInterval => padInterval);

		use UserUnits_IO; use DACUserUnits_IO;
	begin
		if params.Debug then
			Put_Line(Standard_Error, "all samples="&data.SegmentLen'Img
				&", NumSamplesPerEpisode="&data.wf.NumSamplesPerEpisode'Img);
			Put(Standard_Error, "epoch durations: ");
			for i in EpochIndex'First .. 5 loop
				Put(Standard_Error, "  "&data.wf.wfShapes.EpochInitDuration(params.wfChannel)(i)'Img);
			end loop;
			New_Line(Standard_Error);
			Put_Line(Standard_Error, "new samples=" & newData.SegmentLen'Img &
			      ",  padInterval=" & padInterval'Img &
			      ",  onsetSeek="   & onsetSeek'Img);
			Put_Line(Standard_Error, "stepsOffset=" & stepsOffset'Img &
				",  stepsDuration=" & stepsDuration'Img &
				",  controlOffset=" & controlOffset'Img);
-- 			newData.data.chn(1).swp(1).all := (others=>0.0);
			--  just a simple check of array slice addressing
		end if;

		--  average control sweeps if requested and prepare controlSlice
		if params.AverageLeaks then
			for i in 1 - padInterval .. stepsDuration + 1 + padInterval loop
				declare
					S : ADC_UserUnits := 0.0;
				begin
					for nSwp in 1 .. newData.numSweeps loop
						S := S + data.data.chn(params.dataChannel)
							.swp(nSwp)(controlOffset + i);
					end loop;
					controlSlice(i + padInterval) := S / ADC_UserUnits(newData.numSweeps);
				end;
			end loop;
		end if;

		if params.AdjustV then
			AdjustVm(data, newData, params, stepsOffset, stepsDuration, padInterval);
		end if;

		Sweeps:  --  main loop
		for nSwp in 1 .. newData.numSweeps loop
		  declare
			--  some shorthands for readability
			V  : Abf.Header.Waveform.EpochLevelArray
				:= data.wf.wfShapes.EpochInitLevel(params.wfChannel);
				--  V stands for voltage here,
				--  cannot use rename like for oldDat below, because this is
				--  a discriminant dependent variant (of a record)
			V1 : DAC_UserUnits renames V(EpochIndex'First + 1);
			V2 : DAC_UserUnits := V(EpochIndex'First + 2) + -- this one changes from sweep to sweep
				DAC_UserUnits(nSwp - 1) *
				data.wf.wfShapes.EpochLevelInc(params.wfChannel)(EpochIndex'First + 2);
			V3 : DAC_UserUnits renames V(EpochIndex'First + 3);
			V4 : DAC_UserUnits renames V(EpochIndex'First + 4);
			V5 : DAC_UserUnits renames V(EpochIndex'First + 5);
			dV : DAC_UserUnits := 0.0; -- Vm adjustment

			oldDat : DataSegment renames data.data.chn(params.dataChannel).swp(nSwp).all;

			--  average levels before steps and leak controls
			StepAvg, controlAvg : ADC_UserUnits;
				--  not assigning 0.0 here to keep initialization near the use and
				--  so that warnings are thrown off if they are (mis)used before intended time..

		  begin
			--  make sure controlSlice is all set..
			if not params.AverageLeaks then -- if yes, controlSlice was prepared above
				controlSlice := oldDat(controlOffset + 1 - padInterval ..
						controlOffset + stepsDuration + 1 + padInterval);
			end if;

			--  calc the averages,
			case params.CalcLeaksFrom is
			  when ControlTail =>
				declare
					ctailAvg : ADC_UserUnits := 0.0;
				begin
					for i in 1 .. padInterval loop
						ctailAvg := ctailAvg + controlSlice(stepsDuration + i);
					end loop;
					ctailAvg    := ctailAvg / ADC_UserUnits(padInterval);
					StepAvg     := ctailAvg * ADC_UserUnits( V1/V4 );
					controlAvg  := ctailAvg * ADC_UserUnits( V3/V4 );
					if params.Debug then
						Put(Standard_Error, "(-lc given) StepAvg=");Put(StepAvg,Aft=>2,Exp=>0);
						Put(Standard_Error, ", controlAvg=");Put(controlAvg,Aft=>2,Exp=>0);
						New_Line(Standard_Error);
					end if;
				end;

			  when PrePulse =>
				StepAvg := 0.0;
				for i in 1 .. padInterval loop
					StepAvg := StepAvg + oldDat(i + stepsOffset - padInterval);
				end loop;
				StepAvg     := StepAvg / ADC_UserUnits(padInterval);
				controlAvg  := StepAvg * ADC_UserUnits( V3/V1 );
				if params.Debug then
					Put(Standard_Error, "(-lp given) StepAvg=");
					Put(Standard_Error, StepAvg,Aft=>2,Exp=>0);
					Put(Standard_Error, ", controlAvg=");
					Put(Standard_Error, controlAvg,Aft=>2,Exp=>0);
					New_Line(Standard_Error);
				end if;

			  when Both => -- do this over prepad intervals
				StepAvg := 0.0; controlAvg := 0.0;
				for i in 1 .. padInterval loop
					StepAvg    := StepAvg    + oldDat(i + stepsOffset - padInterval);
					controlAvg := controlAvg + controlSlice(i);
				end loop;
				StepAvg    := StepAvg    / ADC_UserUnits(padInterval);
				controlAvg := controlAvg / ADC_UserUnits(padInterval);
				if params.Debug then
					Put(Standard_Error, "StepAvg=");
					Put(Standard_Error, StepAvg,Aft=>2,Exp=>0);
					Put(Standard_Error, ", controlAvg=");
					Put(Standard_Error, controlAvg,Aft=>2,Exp=>0);
					New_Line(Standard_Error);
				end if;
			end case; -- CalcLeaksFrom

			--
			--  finally perform leak subtraction
			--
			Pre_padding:
			for i in 1 .. padInterval loop
				newData.data.chn(params.dataChannel).swp(nSwp)(i) :=
					oldDat(i + stepsOffset - padInterval) - StepAvg;
			end loop Pre_padding;

			--  in Steps and Post_padding we subtract scaled controls,
			--  so we may need to crop subtracted values..
			if params.CropLeaks then
			  declare
				leak : ADC_UserUnits;
			  begin
				Steps_wLS:
				for i in 1 .. stepsDuration loop
					leak := StepAvg + ADC_UserUnits( (V2 - V1) / (V4 - V3) ) *
						(controlSlice(i + padInterval) - controlAvg);

					CropValue(data, nChan=>params.dataChannel, value=> leak);
					newData.data.chn(params.dataChannel).swp(nSwp)(padInterval + i) :=
						oldDat(i + stepsOffset) - leak;
				end loop Steps_wLS;

				Post_padding_wLS:
				for i in stepsDuration + 1 .. stepsDuration + padInterval loop
					leak := StepAvg * ADC_UserUnits(V3/V1) + -- ATTN! why I don't just use controlAvg here?
						ADC_UserUnits( (V3 - V2) / (V5 - V4) )
						* (controlSlice(i + padInterval) - controlAvg);

					CropValue(data, nChan=>params.dataChannel, value=> leak);
					newData.data.chn(params.dataChannel).swp(nSwp)(padInterval + i) :=
						oldDat(i + stepsOffset) - leak;
				end loop Post_padding_wLS;
			  end; -- cropping declare

			else -- no cropping
				Steps:
				for i in 1 .. stepsDuration loop
					newData.data.chn(params.dataChannel).swp(nSwp)(padInterval + i) :=
						oldDat(i + stepsOffset) - StepAvg -
						ADC_UserUnits( (V2 - V1) / (V4 - V3) )
						* (controlSlice(i + padInterval) - controlAvg);
				end loop Steps;

				Post_padding:
				for i in stepsDuration + 1 .. stepsDuration + padInterval loop
					newData.data.chn(params.dataChannel).swp(nSwp)(padInterval + i) :=
						oldDat(i + stepsOffset) - StepAvg * ADC_UserUnits(V3/V1) -
							-- ATTN! why I don't just use controlAvg above?
						ADC_UserUnits( (V3 - V2) / (V5 - V4) )
						* (controlSlice(i + padInterval) - controlAvg);
				end loop Post_padding;
			end if; -- CropLeaks

		  end; -- renaming declare
		end loop Sweeps;

		--  finally output the result
		case params.outFormat is
		  when ABFFile =>
			ABF.Data.WriteAbf(To_String(params.OutFile), newData, abfH);
		  when ATFFile =>
			ABF.Data.WriteAtf(To_String(params.OutFile), newData);
		  when PlotDump =>
			ABF.Data.Print2Plot(newData);
		end case;
	end; -- main block + newData declare

  exception
	when Finish => null;
	-- normal termination, in case help was invoked or other legal reason..
end LeakSub;

