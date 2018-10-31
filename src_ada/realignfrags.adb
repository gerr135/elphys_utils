--    realignFrags -
--    reads fragmetns stored in an ATF file,
--    n-pt smoothes them and then realigns by max derivative on given interval
--    or fits Boltzman funcs and performs sub-step realignment
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
with Ada.Command_Line;  use Ada;
with GNAT.Command_Line;

with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Numerics.Generic_Elementary_Functions;

with ATF_IO;
with Transition_Searches;

procedure realignFrags is


	procedure printUsage is
	begin
		Put_Line("this program realigns set of episodes in atf file");
		Put_Line("by max n-pt diference or sub-step by fitting boltzman's");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " options  file");
		Put_Line("output goes to stdout, ATF format");
		New_Line;
		Put_Line("utility options:");
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output");
		New_Line;
		Put_Line("common options:");
		Put_line("-x f    rescale time by multiplying by this factor");
		Put_Line("-c f    center of alignment estimate (mks)");
		Put_Line("-w f    alignment window half-width (mks) around -c center");
		Put_Line("-t [u|d]   transition direction: upwards or downwards (autodetect if omitted)");
		New_Line;
		Put_Line("max difference options:");
		Put_Line("-n n    N points in smoothing window, use 5, 7 or 9 (default 7))");
		Put_Line("-d n    N points for derivative calculation (default 5)");
		Put_Line("-f [t]     force maxDiff estimation even if -c is given");
		Put_Line("          (otherwise only boltzman fit is performed on c +- w)");
		Put_Line("-e      use template search instead of simple max diff");
		New_Line;
		Put_Line("boltzman fit options:");
		Put_Line("-s f    estimate of 10-90 rise time at midpoint");
		Put_Line("-m n    N point for amplitude estimate (default 7)");
		Put_Line("-y      fit smoothed traces (but align original)");
		Put_Line("-u f    use this value to set fit windows (estimated x0 +- u)");
		Put_Line("-z      skip boltzman fit (only do maxDiff realignment)");
		New_Line;
		Put_Line("output options:");
		Put_Line("-o      omit episodes that diverged");
		Put_Line("-k f    omit episodes whose dx is larger than specified value");
		Put_Line("-v f    omit episodes whose x0 deviates from -c value by more than specified amount");
		New_Line;
	end printUsage;


------------------------------------

Finish : Exception; -- normal immediate termination

type MyReal is new Long_Float;

package MyFloat_IO is new Float_IO(MyReal);
use MyFloat_IO;

package MyFuncs is new Ada.Numerics.Generic_Elementary_Functions(MyReal);
use MyFuncs;


package MyAtf_IO is new Atf_IO(MyReal);
use MyAtf_IO; -- need :=, etc..

package MySearches is new Transition_Searches(MyReal,MyAtf_IO);
use MySearches;

-- input paramters

type ParamRec is record
	XScale : MyReal := 1.0;
	Center : MyReal; --mks
	CenterIsGiven : Boolean := False;
	AlignWidth : MyReal := 0.0; --mks
	forceMaxDiffSearch : Boolean := False;
		-- force estimation of max difference even if -c is given
	doTemplateSearch : Boolean := False;
	templateAmplitude : MyReal := 0.0;

	NSmoothing : MyAtf_IO.Count := 7; -- N pts to do smoothing (simple average)
	NDeriv     : MyAtf_IO.Count := 5; -- will look at "average" derivative
		-- with linear derivative definition this just means y_i+n - y_i
		-- use 5 for now, because that ~ corresponds to 10-90 rise time
	NAvg       : MyAtf_IO.Count := 7; -- N pts for A1/A2 estimation
		-- note: if -w is given, it (or otherwise full interwal) should
		-- give search window > 2*NAvg + 3 (assuming 2x-5x oversampling)

	transitDir : Transition_Direction := Autodetect;
	T10_90 : MyReal := 0.0;
	TIsGiven : Boolean := False;
	SkipBoltzman : Boolean := False;
	fitSmoothed  : Boolean := False;

	outputSkipped : Boolean := True;
		-- whether to otuput episodes that do not "qualify"
		-- this (and fitParamRec.skipEpisode) really is a general hook here,
		-- originally inserted for tracing episodes whose fit diverged

	dxLimit : MyReal := 0.0;
		-- skip episodes with dx > this value.
		-- 0.0 indicates that flag was not given
	x0MaxDeviation : MyReal := 0.0;
		-- skip episodes whose x0 deviates by more than this value
	fitWidth  : MyReal := 0.0;
		-- define fit window by individual center +- this param
		-- instead of global interval

	Debug : Boolean := False;
	DataFileName : Unbounded_String := Null_Unbounded_String;
 end record;



	procedure processCommandLine(params : out ParamRec) is
		use Integer_Text_IO;
		use GNAT.Command_Line; use Ada.Command_Line;

		Options : constant String := "c: d: e f g h k: m: n: o s: t: u: v: w: x: y z";
		Last:Positive;
	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin  -- need to handle local exceptions
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;

				when 'c' =>
					Get(Parameter,MyReal(params.Center),Last);
					params.CenterIsGiven := True;

				when 'd' => Get(Parameter,Integer(params.NDeriv), Last);
				when 'f' => params.forceMaxDiffSearch := True;

				when 'g' => params.Debug := True;
				when 'h' => printUsage;

				when 'k' => Get(Parameter,MyReal(params.dxLimit), Last);
				when 'm' => Get(Parameter,Integer(params.NAvg), Last);
				when 'n' => Get(Parameter,Integer(params.NSmoothing), Last);
				when 'o' => params.outputSkipped := False;

				when 's' =>
					Get(Parameter,MyReal(params.T10_90),Last);
					params.TIsGiven := True;

				when 't' =>
					if    Parameter="u"  then params.transitDir := Up;
					elsif Parameter="d"  then params.transitDir := Down;
					else
						Put_Line("-t should be one of u or d!");
						raise Finish;
					end if;


				when 'u' => Get(Parameter,MyReal(params.fitWidth), Last);
				when 'v' => Get(Parameter,MyReal(params.x0MaxDeviation), Last);
				when 'w' => Get(Parameter,MyReal(params.AlignWidth),Last);
				when 'x' => Get(Parameter,MyReal(params.XScale), Last);

				when 'y' => params.fitSmoothed  := True;
				when 'z' => params.SkipBoltzman := True;

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
		if (params.AlignWidth /= 0.0 or params.x0MaxDeviation /= 0.0 )
				and not params.CenterIsGiven then
			Put_Line("options -v and -w require -c !!");
			raise Finish;
		end if;
		if ( params.fitSmoothed or params.fitWidth /= 0.0 )
				and params.SkipBoltzman then
			Put_Line("conflicting options -y (fit smoothed) and -z (skip fit) are given!");
			raise Finish;
		end if;
		if params.SkipBoltzman and params.CenterIsGiven
				and not params.forceMaxDiffSearch then
			Put_Line("both -z and -c are given, but no -f. What should I do?");
			raise Finish;
		end if;
	end processCommandLine;


	procedure adjustStuff (dat: Atf_File; params : in out ParamRec) is
	begin
		-- adjust x scale if requested
		if params.XScale /= 1.0 then
			dat.data.XStart := dat.data.XStart * params.XScale;
			dat.data.XStep  := dat.data.XStep  * params.XScale;
		end if;

		-- check if supplied centr is reasonable
		if params.CenterIsGiven then
			declare
				diCenter : MyAtf_IO.Count := MyAtf_IO.Count( MyReal'Rounding(
					(params.Center-dat.data.XStart)/dat.data.XStep ) );
				-- distance in points from start of data
			begin
				if diCenter < params.NAvg or
						diCenter > dat.data.data'Last(2) - params.NAvg then
					Put_Line("Specified Center (-c) value is outside of data boundaries");
					raise Finish;
				end if;
			end;
		end if;
	end;


-- checks combination of parameters and fit results and assigns fit.SkipAlignment
-- Boltzman fit convergence is checked in the doFit procedure
function ifWeSkipEpisode(fit:FitParamRec; params:ParamRec)return Boolean is
begin
	-- first pass up if epsode already marked for skipping
	if fit.skipAlignment or fit.TooDistorted then
		return True;
	end if;

	-- check -v and -k options
	if ( params.dxLimit /= 0.0 and then fit.dx > params.dxLimit ) or
		( params.x0MaxDeviation /= 0.0 and then
		MyReal'Copy_Sign(fit.x0 - params.Center, 1.0) > params.x0MaxDeviation ) then

		return True;
	end if;

	-- check if fit result match transition direction (if given)
	case params.transitDir is
		when Up =>
			if fit.A2 < fit.A1 then return True; end if;
		when Down =>
			if fit.A2 > fit.A1 then return True; end if;
		when Autodetect =>
			null;
	end case;

	-- nothing triggered so far
	return False;
end;


--------------------------
-- main block

	F:File_Type;
	ATFData : Atf_File;
		-- whereto we read the data

	newData : Atf_DataPtr;
		-- will have smaller interval in the end and will do resampling

	params : ParamRec;



begin
	processCommandLine(params);
	ATFData := MyAtf_IO.getData(To_String(params.DataFileName));

	-- do consistency checks and necessary adjustments
	-- between data and params
	adjustStuff(AtfData,params);


	if params.Debug then
		Put("NEntries=");Put(Integer(ATFData.data.NEntries));
		Put(" ("& AtfData.data.data'First(1)'Img &" .. "& AtfData.data.data'Last(1)'Img &");  ");
		Put("Length=");Put(Integer(ATFData.data.Length));
		Put(" ("& AtfData.data.data'First(2)'Img &" .. "& AtfData.data.data'Last(2)'Img &")");
		New_Line;
		Put("xstart=");Put(MyReal(ATFData.data.XStart));
		Put(",   xstep=");Put(MyReal(ATFData.data.XStep));
		New_Line;
	end if;

	Searching_Block:
	declare
		-- first few handy renames
		dat : Atf_Data renames ATFData.data.all;

		iMin, iMax : MyAtf_IO.Count; -- search region
		xMin : MyReal; -- x position corresponding to iMin
			-- (just a helper var really)
			-- XStep is conserved

		fitResults : FitArray(ATFData.data.data'Range(1));
		-- matches NEntries

		smoothed : Atf_DataPtr;
			-- may be used by maxDiff search or during fit


	begin
		-- determine search region (indeces)
		if params.CenterIsGiven then
			calcIndeces_of_Search_Region(dat,
				center => params.Center, width => params.AlignWidth,
				iMin => iMin, iMax => iMax,
				Debug => params.Debug);
		else -- search entire episodes
			iMin := dat.data'First(2);
			iMax := dat.data'Last(2);
		end if;
		if iMax - iMin < 2*params.NAvg + params.nDeriv then
			Put_Line("search interval is too short!");
			Put_Line("iMin="& iMin'Img &",  iMax="& iMax'Img);
			raise Finish;
		end if;

		if params.Debug then
			Put("iMin=");Put(Integer(iMin), Width=>1);Put(",  iMax=");
			Put(Integer(iMax), Width=>1);New_Line;
		end if;

		--estimate amplitudes and check for consistency with transition direction
		-- if given. Will set tooDistorted if difference < stdError
		assignAmplitudes(dat, fitResults,
			iMin=>iMin, iMax=>iMax, nPt=>params.NAvg);
		-- assign xMin from iMin
		xMin := dat.XStart +
			MyReal( iMin - dat.data'First(2) )*dat.XStep;
		if params.Debug then
			Put_Line("xMin="& xMin'Img);
		end if;

		-- find transition candidates
		if ( not params.CenterIsGiven or else params.forceMaxDiffSearch )
				or params.fitSmoothed then
			-- realign to points by max difference
			smoothed := smooth(dat,params.NSmoothing);

			FindMaxDiffs(smoothed.all, fitResults,
				iMin=>iMin+params.NSmoothing/2, -- omit non-smoothed ends
				iMax=>iMax-params.NSmoothing/2,
				NDiff=>params.NDeriv,
				Direction => params.transitDir,
				Debug => params.Debug);
		else
			-- Center estimate was given in this case
			-- use it to uniformly fill fitResults
			for i in fitResults'Range loop
				fitResults(i).x0 := params.Center;
			end loop;
		end if;

		-- now only dx estimate left
		declare
			TheSlope : MyReal := 0.7*AtfData.data.XStep;
		begin
			if params.TIsGiven then TheSlope := params.T10_90/(2.0*Log(9.0)); end if;
			for i in fitResults'Range loop
				fitResults(i).dx := TheSlope;
			end loop;
		end;

		-- do the fit
		if not params.SkipBoltzman then
			for episode in dat.data'Range(1) loop
				-- could have left looping and checking tooDistorted
				-- to the doFit itself, but it seems a bit clearer this way
				-- doFit only processes one slice
				if fitResults(episode).tooDistorted then
					goto Loop_End;
					-- I really hate goto's,
					-- but there are too many indentation levels here
				end if;

				LocalIMin_Max:
				declare
					MyIMin : MyAtf_IO.Count := iMin;
					MyIMax : MyAtf_IO.Count := iMax;
				begin
					if  params.fitWidth /= 0.0 then
						-- individual fit intervals were requested
						-- reuse indices procedure,
						-- this time with local iMin/Max
						calcIndeces_of_Search_Region(dat,
							center => fitResults(episode).x0, width => params.fitWidth,
							iMin => MyIMin, iMax => MyIMax,
							Debug => params.Debug);
					end if;

					fitSlice:
					declare
						slice : FloatArray(MyIMin .. MyIMax);
						x0 : MyReal renames fitResults(episode).x0;
						dataToFit : Atf_DataPtr := AtfData.data;
					begin
						if params.fitSmoothed then
							-- smoothing is done above for this reson as well
							dataToFit := smoothed;
						end if;

						-- form a slice to pass to doFit
						for i in MyIMin .. MyIMax loop
							slice(i) := dataToFit.data(episode,i);
						end loop;

						-- and finally the fit itself
						doFit( slice,
							fit => fitResults(episode),
							XStart => xMin, XStep => dat.XStep,
							Debug => params.Debug);
					end fitSlice;

				end LocalIMin_Max;

				<<Loop_End>>
				null;  -- need a statment to which to connect label here
			end loop;
		end if;

		-- run through fit results and mark episodes to skip
		for eps in fitResults'Range loop
			fitResults(eps).skipAlignment :=
				ifWeSkipEpisode(fitResults(eps),params);
		end loop;

		-- and finally realign episodes
		newData := adjustData(dat,fitResults,
			Center => params.Center, AlignByCenter => params.CenterIsGiven,
			outputSkipped => params.outputSkipped,
			Debug => params.Debug);


		-- all done, output results

		-- need to do header inside Searching_Block
		-- since I need fitResults
		if params.outputSkipped then
			writeHeader(Header => AtfData.Header.all);
		else
			-- we may have less episodes if any diverged,
			-- so lets adjust header
			declare
				newHeader : ATF_Header(NComments=>AtfData.Header.NComments,
					NEntries => newData.data'Length(1) + 1); -- its a header (re: +1)

				skippedEpisodes : MyAtf_IO.Count := 0;
			begin
				newHeader.comments := AtfData.Header.comments;

				-- now units, first copy the x's
				newHeader.units(newHeader.units'First) :=
					AtfData.Header.units(AtfData.Header.units'First);

				-- and then the rest, omitting skipped ones
				for episode in AtfData.Header.units'First + 1 ..
						AtfData.Header.units'Last loop
					if fitResults(episode-1).skipAlignment then
						skippedEpisodes := skippedEpisodes + 1;
					else
						newHeader.units(episode - skippedEpisodes) :=
							AtfData.Header.units(episode);
					end if;
				end loop;

				writeHeader( Header => newHeader );
			end;
		end if;

	exception

		when All_Diverged =>
			Put_Line("no episodes converged for this data set!");
			Put_Line("try different parameters or estimates");
			raise Finish;

	end Searching_Block;

	writeData(Data => newData.all);

	exception
		when Finish => null; -- normal termination
end realignFrags;
