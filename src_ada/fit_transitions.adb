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
with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
--with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with GNAT.Command_Line;
with ATF_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Generic_Simplex;
with Transition_Searches;

procedure Fit_Transitions is

Finish : Exception; -- immediate (normal) termination

	procedure printUsage is
		-- could be a local procedure
		-- but this way I don't need to write a separate description :)
	begin
		Put_Line("This program fits series of episodes to Boltzman function.");
		Put_Line("Traces (or specified intervals) should contain a single transition");
		Put_Line("(direction is autodetected) and have at least 10 (or specified #)");
		Put_Line("leading/trailing points. Otherwise fix amplitudes by specifying them.");
		Put_Line("Initial parameter estimates are done in the assumption of 2x-5x oversampling.");
		Put_Line("Outputs a table of fit results");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  file");
		Put_Line("outputs goes to stdout");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output");
		Put_Line("-a f    fit on interval from a");
		Put_Line("-b f    to b (full trace if ommitted)");
		Put_Line("-c f    approx transition midpoint, if ommitted autodetect");
		Put_Line("-d f    estimate of responsivity at midpoint (non-fixed)");
		Put_Line("-l f    estimate of left (pre-) and right (post-)");
		Put_Line("-r f    amplitudes (makes corresponding parameter[s] fixed)");
		Put_Line("-n n    N points to use for A1, A2 estimates (default 9)");
		Put_line("-x f    rescale time by multiplying by this factor");
		Put_line("-2[f]   fit by double-Boltzman. If optional number (no space!)");
		Put_Line("           is supplied, it is used as seeding value for delta");
		Put_Line("  output manipulation:");
		Put_Line("-t [d,u] force transition direction: down, up (skip wrongs)");
		Put_Line("-s      skip diverged fits (x0 outside boundary)");
		Put_Line("-sl f   skip fits where A2-A1 < supplied number");
		Put_Line("-sh f   skip fits where A2-A1 > supplied number");
		Put_Line("-kl f   skip fits where T10-90 is larger than specified");
		Put_Line("-kh f   skip fits where T10-90 is smaller than specified");
	end printUsage;

-------------------
-- principal types and definitions
type MyReal is new Long_Float;

package MyFloat_IO is new Float_IO(MyReal);
use MyFloat_IO;

package MyFuncs is new Ada.Numerics.Generic_Elementary_Functions(MyReal);
use MyFuncs;

package MyAtf_IO is new Atf_IO(MyReal);

package MyTransSearches is new Transition_Searches(MyReal,MyAtf_IO);
use MyTransSearches;

type ComLineParamRec is record
	a,b : MyReal := 0.0;    -- interval of interest
	aIsSet, bIsSet : Boolean := False;  -- a or b can legitimately be 0.0, need to trace if they were set
	iMin,iMax : MyAtf_IO.Count := 1; -- "real" boundaries, !!don't forget to adjust when data is fetched!
	center : MyReal := 0.0; -- estimate of transition midpoint
	centerIsSet : Boolean := False;  -- center can also legitimately be 0.0
	doubleBoltzman : Boolean := False; -- whether to fit with double Boltzman
	dlt : MyReal := 0.0;    -- use this for the estimate of delta between midpoints if double Boltzman is requested
	dltSet : Boolean := False; -- did we get dlt specified?
	dx : MyReal := 0.0;     -- estimate of the slope
	fixAl, fixAr : Boolean := False; -- whether to fix amplitudes
	A1, A2 : MyReal := 0.0; -- fix A1, A2 at these values
	N2avg : MyAtf_IO.Count := 9; -- -n, N points to do an estimate of leading/trailing aplitudes
	tScale : MyReal := 0.0; -- factor to rescale time
	InputFileName : Unbounded_String := Null_Unbounded_String;
	Debug : Boolean := False;
	-- output parameters
	minDx, maxDx : MyReal := 0.0;  -- skip fits with dx outside min/max
	minDA, maxDA : MyReal := 0.0;  -- skip fits with A2-A1 outside min/max
	skipDiverged : Boolean := False;
	transitDir : Transition_Direction := Autodetect;
end record;

type FitParamRec is record
	A1, A2 : MyReal := 0.0; -- amplitudes,
	x0,dx : MyReal := 0.0;   -- midpoint and slope of distribution
	eps : MyReal := 0.0;    -- minimal chi2
	tooDistorted : Boolean := False;
		-- set during assigning seedings, indicates whether actual fit is skipped
	dlt : MyReal := 0.0;    -- delta for 2xBoltzman fit
end record;


type FitArray is array(MyAtf_IO.Count range <>) of FitParamRec;
type FitArrayPtr is access FitArray;


------------------------------
-- procedural part

	procedure processCommandLine(params : in out ComLineParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		Options : constant String := "2? a: b: c: d: g h kl: kh: l: n: r: s sl: sh: t: x:";
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
				when 'h' => printUsage;
				when 'a' =>
					Get(Parameter,MyReal(params.a),Last);
					params.aIsSet := True;
				when 'b' =>
					Get(Parameter,MyReal(params.b),Last);
					params.bIsSet := True;
				when 'c' =>
					Get(Parameter,MyReal(params.Center),Last);
					params.centerIsSet := True;
				when 'd' => Get(Parameter,MyReal(params.dx),Last);
				when 'g' => params.Debug := True;

				when 'k' =>
					if    Full_Switch = "kl" then
						Get(Parameter,MyReal(params.minDx),Last);
						-- now convert T10-90 in dx
						params.minDx := params.minDx/(2.0*Log(9.0));
					elsif Full_Switch = "kh" then
						Get(Parameter,MyReal(params.maxDx),Last);
						-- now convert T10-90 in dx
						params.maxDx := params.maxDx/(2.0*Log(9.0));
					end if;

				when 'n' => Get(Parameter,Integer(params.N2avg), Last);
				when 'l' =>
					Get(Parameter,MyReal(params.A1), Last);
					params.fixAl := True;
				when 'r' =>
					Get(Parameter,MyReal(params.A2), Last);
					params.fixAr := True;

				when 's' =>
					if    Full_Switch = "s"  then
						params.skipDiverged := True;
					elsif Full_Switch = "sl" then
						Get(Parameter,MyReal(params.minDA),Last);
					elsif Full_Switch = "sh" then
						Get(Parameter,MyReal(params.maxDA),Last);
					end if;

				when 't' =>
					if    Parameter="u"  then params.transitDir := Up;
					elsif Parameter="d"  then params.transitDir := Down;
					else
						Put_Line("-t should be one of u or d!");
						raise Finish;
					end if;

				when 'x' => Get(Parameter,MyReal(params.tScale), Last);
				when '2' =>
					params.doubleBoltzman := True;
					if Parameter /= "" then
						Get(Parameter,MyReal(params.dlt), Last);
						params.dltSet := True;
					end if;

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
			end loop;
		exception
			when Invalid_Switch =>
				Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter =>
				Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error =>
				Put_Line ("Invalid numeric format for switch" & Full_Switch);raise Finish;
		end;
		params.InputFileName := To_Unbounded_String(Get_Argument(Do_Expansion => True));

		-- some consistency checks
		if params.transitDir /= Autodetect and
				(params.fixAl or params.fixAr) then
			Put_Line("passing -t only makes sence if A1 and A2 are unfixed!");
			raise Finish;
		end if;
	end processCommandLine;


procedure printResults(results:FitArray; params:ComLineParamRec;
		dat : MyAtf_IO.Atf_Data) is

	function resultsOk(fit:fitParamRec; params:ComLineParamRec; dat : MyAtf_IO.Atf_Data)
			return Boolean is -- here we collect all output checks
	begin
		if params.Debug then Put("resultsOk called.."); end if;
		if params.skipDiverged then
			if fit.tooDistorted then return False; end if;
			declare
				use MyAtf_IO;
				xMin : MyReal := dat.XStart + dat.XStep*MyReal(params.iMin-dat.data'First);
				xMax : MyReal := dat.XStart + dat.XStep*MyReal(params.iMax-dat.data'First);
			begin
				if (fit.x0<=xMin) or (fit.x0>=xMax) then
					return False;
				end if;
			end;
		end if;

		case params.transitDir is
			when Up =>
				if fit.A2<fit.A1 then return False; end if;
			when Down =>
				if fit.A2>fit.A1 then return False; end if;
			when Autodetect =>
				null;
		end case;

		if params.minDA /= 0.0 and then (
			( params.transitDir = Up    and fit.A2 - fit.A1 < params.minDA ) or
			( params.transitDir = Down  and fit.A1 - fit.A2 < params.minDA ) )
				then return False;
		end if;
		if params.maxDA /= 0.0 and then (
			( params.transitDir = Up    and fit.A2 - fit.A1 > params.maxDA ) or
			( params.transitDir = Down  and fit.A1 - fit.A2 > params.maxDA ) )
				then return False;
		end if;

		if params.minDx /= 0.0 and then
				fit.dx < params.maxDx then return False; end if;
		if params.maxDx /= 0.0 and then
				fit.dx > params.maxDx then return False; end if;

		-- did not trigger anything so far,
		if params.Debug then Put_Line("looks fine!"); end if;
		return True;
	end;

begin
	--Put_Line("results of fitting "& To_String(params.InputFileName) &"with Blotzman curves");
	-- some fancy headings first
	if params.fixAl and not params.fixAr then
		Put_Line("A1 fixed at"& params.A1'Img);
	elsif params.fixAr and not params.fixAl then
		Put_Line("A2 fixed at"& params.A2'Img);
	elsif params.fixAl or params.fixAl then
		Put_Line("A1 and A2 fixed at ("& params.A1'Img &", "& params.A2'Img &")");
	end if;
	--New_Line;
	Put("NEpisode,   x0,   dx,   T");
	if params.doubleBoltzman then Put(",  dlt"); end if;
	if not params.fixAl then Put(",   A1"); end if;
	if not params.fixAr then Put(",   A2"); end if;
	Put(",   epsilon");
	New_Line;

	-- and the actual data
	for i in results'Range loop
		if resultsOk(results(i),params,dat) then
			Put(Integer(i),Width => 4);Put(", "); -- episode #
			Put(results(i).x0'Img &", "& results(i).dx'Img); -- x0 and dx
			Put(", "& MyReal'Image(2.0*Log(9.0)*results(i).dx));  -- T (calculated from dx)
			if params.doubleBoltzman then
				Put(", "& results(i).dlt'Img);
			end if;
			if not params.fixAl then Put(", "& results(i).A1'Img); end if;
			if not params.fixAr then Put(", "& results(i).A2'Img); end if;
			Put(", "& MyReal'Image(results(i).eps/(results(i).A2-results(i).A1)**2) );
			New_Line;
		end if;
	end loop;
end;


procedure adjustStuff(data: in out MyAtf_IO.Atf_Data; params : in out ComLineParamRec) is
	use MyAtf_IO;
begin
	-- rescale the time if requested
	if params.tScale /= 0.0 then
		data.XStart := data.XStart*params.tScale;
		data.XStep  := data.XStep *params.tScale;
	end if;
	-- now set iMin/iMax
	if params.aIsSet then
		-- fisrt check if requested boundaries are sane
		if params.a < data.XStart or
		   params.a > data.XStart + MyReal(data.Length - 1)*data.XStep then
			Put_Line("supplied a is outside the data range!");
			raise Finish;
		end if;
		params.iMin := data.data'First(2) + MyAtf_IO.Count(MyReal'Floor( (params.a - data.XStart)/data.XStep ));
	else
		params.iMin := data.data'First(2);
	end if;
	-- iMax
	if params.bIsSet then
		if params.b < data.XStart or
		   params.b > data.XStart + MyReal(data.Length - 1)*data.XStep then
			Put_Line("supplied b is outside the data range!");
			raise Finish;
		end if;
		params.iMax := data.data'First(2) + MyAtf_IO.Count(MyReal'Floor( (params.b - data.XStart)/data.XStep ));
	else
		params.iMax := data.data'Last(2);
	end if;
	-- check if the interval is long enough
	if params.iMax - params.iMin < 2*params.N2avg + 5 then
		-- + 5 to allow some space for the transition itself. Should be >= 3 points
		-- assuming >2x oversampling
		Put_Line("fitting interval is too short!");
		Put_Line("iMin="& params.iMin'Img &",  iMax="& params.iMax'Img);
		raise Finish;
	end if;
	if params.Debug then
		Put_Line("iMin="& params.iMin'Img &",  iMax="& params.iMax'Img);
	end if;
	-- check if
end;


--------------------------
-- processing procedures

function assignSeedingValues(data:MyAtf_IO.Atf_Data; episode : MyAtf_IO.Count;
			params : ComLineParamRec) return fitParamRec is
	-- main magic happens here :), doFit is just an invocation of standard functions..
	-- ??Add some basic baseline correction here or separately before this function?
	fit : FitParamRec;
	use MyAtf_IO; -- need ">", etc.
begin
	-- check leading/trailing fragments and set A1/r
	if params.fixAl then
		fit.A1 := params.A1;
	else
		declare
			S:MyReal := 0.0;
		begin
			for i in params.iMin .. params.iMin + params.N2avg-1 loop
				S := S + data.data(episode,i);
			end loop;
			fit.A1 := S/MyReal(params.N2avg);
		end;
	end if;
	if params.fixAr then
		fit.A2 := params.A2;
	else
		declare
			S:MyReal := 0.0;
		begin
			for i in params.iMax - params.N2avg+1 .. params.iMax loop
				S := S + data.data(episode,i);
			end loop;
			fit.A2 := S/MyReal(params.N2avg);
		end;
	end if;
-- 	-- detect direction and set dx (assume ~3x oversampling)
-- 	if fit.A2 > fit.A1 then -- upwards
-- 		fit.dx := 0.7*data.XStep;
-- 	else -- downwards
-- 		fit.dx := -0.7*data.XStep;
-- 	end if;
	-- dx should always be positive
	fit.dx := 0.7*data.XStep;
	--finally detect the candidate for midpoint
	declare
		diff, MaxDiff : MyReal := 0.0;
		-- note that while it is possible to have all diffs on the "wrong" side of zero
		-- in the middle part, it would indicate a *really* misshaped transition
		-- so the fit later on would not converge anyway.
		-- The code checks for (unadjusted) params.iMed=Count'First
		-- and correspondingly skips such episode
		NDiff : MyAtf_IO.Count := 3;
			-- step size,
			-- 3 should work nicely as this approximates transition duration in datapoints
			-- also, we are guaranteed to have at least 5 points in the middle
		sgn : MyReal := MyReal'Copy_Sign(1.0, fit.A2 - fit.A1);
			-- sign of transition. Use to change ">" into "<" for downwards transition
		tmpI : MyAtf_IO.Count := MyAtf_IO.Count'First;
	begin
		for i in params.iMin + params.N2avg .. params.iMax - params.N2avg - NDiff loop
		--no sense to search on the ends, where we require steady intervals
			diff := data.data(episode,i+NDiff) - data.data(episode,i);
			if diff * sgn > MaxDiff * sgn then
				MaxDiff := diff;
				tmpI := i;
			end if;
		end loop;
		if tmpI = MyAtf_IO.Count'First then
			fit.x0 := data.XStart;
			fit.tooDistorted := True;
			if params.Debug then Put_Line("episode "& episode'Img &"is too distorted"); end if;
		else
			fit.x0 := data.XStart + data.XStep*MyReal(tmpI + NDiff/2 - data.data'First(2));
			fit.tooDistorted := False;
		end if;
	end;

	return fit;
end;


procedure doFit(data:MyAtf_IO.Atf_Data; episode : MyAtf_IO.Count;
			fit : in out fitParamRec; params : ComLineParamRec) is
	-- define types and base function

	-- to do this "correctly" I would need 8 different instantiations of Simplex
	-- and correspondingly as many variants of types defined. Quite tedious.
	-- will do a selection of function type in the function itself
	function Base_Goal(x0,dx, -- midpoint and slope of Boltmann
			dlt,              -- delta between midpoints for 2xBoltzman
			A1,A2:MyReal)      -- "outer" amplitudes, every individual Boltzman uses (A2-A1)/2
			return MyReal is

		function Boltzman(x:MyReal; x0,dx,A1,A2:MyReal) return MyReal is
		begin
			return A2 + (A1-A2)/(1.0+Exp((x-x0)/dx));
		end;
		pragma Inline(Boltzman);

		S : MyReal := 0.0;

		Fraction : constant MyReal := 2.0/3.0; -- relative position of intermediate level
		dA1 : constant MyReal := (A2 - A1)*Fraction;
		dA2 : constant MyReal := (A2 - A1)*(1.0-Fraction);
		--dA : MyReal := (A2 - A1)/2.0;
			-- positive, not -2*dA is what is used in Boltzman
		use MyAtf_IO;
	begin
		if params.doubleBoltzman then
			for i in params.iMin .. params.iMax loop
				declare
					x : MyReal := data.XStart + data.XStep*MyReal(i-data.data'First);
				begin
					S := S + ( data.data(episode,i) - ( A2 -
						dA1 / ( 1.0 + Exp((x-x0)/dx) ) -
						dA2 / ( 1.0 + Exp((x-x0-dlt)/dx) )
					) )**2;
				end;
			end loop;
		else
			for i in params.iMin .. params.iMax loop
				S := S + ( data.data(episode,i) -
					Boltzman(data.XStart + data.XStep*MyReal(i-data.data'First),
					x0 => x0, dx => dx, A1 => A1, A2 => A2) )**2;
			end loop;
			S := S * Cosh(dlt/data.XStep); -- constrain dlt so that it does not run off
		end if;
		return S/MyReal(data.data'Length(2));
	end;

	-- some general vars used in Simplex invocation
	maxIter : Positive := 1000;  -- max Simplex iterations
	epsFactor : MyReal := 1000.0; -- how much to reduce starting epsilon

begin -- doFit
	-- check what params are fixed
	if params.fixAl and params.fixAr then
	  declare
		-- first get enough stuff to instantiate Simplex
		type Index is (x0,dx,dlt);
		type Vector is array(Index) of MyReal;

		function goal(v:Vector)return MyReal is
		begin
			return Base_Goal(x0=>v(x0), dx=>v(dx), dlt=>v(dlt),
				A1=>fit.A1, A2=>fit.A2); -- substitute preassigned
		end;

		procedure Simplex is new Generic_Simplex(MyReal,Index,Vector,goal);

		--define local (mutable) vars
		v : Vector := (x0=>fit.x0, dx=>fit.dx, dlt=>params.dlt);
			--  take dlt from params, return into fit
		dv : constant Vector := ( x0=>v(x0)/2.0, dx=>v(dx)/2.0, dlt=>data.XStep/2.0 );

	  begin
	  	--just a few lines of "actual" code :)
		fit.eps := goal(v)/epsFactor;
			-- will probably never reach it, but I just want the best estimate
	  	Simplex(v,dv,fit.eps,maxIter);
		-- that's it, just read the parameters back now
		fit.x0 := v(x0);
		fit.dx := v(dx);
		fit.dlt := v(dlt);
	end;

	-----------------------------------
	elsif params.fixAl then
	  declare
		-- need to basically copy bunch of code with few minor modifications
		-- !!be careful about what gets pre-assigned!
		type Index is (x0,dx,dlt,A2);
		type Vector is array(Index) of MyReal;

		function goal(v:Vector)return MyReal is
		begin
			return Base_Goal(x0=>v(x0), dx=>v(dx), dlt=>v(dlt),
				A1=>fit.A1, A2=>v(A2)); -- substitute preassigned
		end;
		procedure Simplex is new Generic_Simplex(MyReal,Index,Vector,goal);
		v : Vector := (x0=>fit.x0, dx=>fit.dx, dlt=>params.dlt, A2=>fit.A2);
		dv : constant Vector := ( x0=>v(x0)/2.0, dx=>v(dx)/2.0,
			dlt=>data.XStep/5.0, A2=>v(A2)/10.0 );

	  begin
		fit.eps := goal(v)/epsFactor;
		Simplex(v,dv,fit.eps,maxIter);
		fit.x0 := v(x0);
		fit.dx := v(dx);
		fit.dlt := v(dlt);
		fit.A2 := v(A2);
	end;

	-----------------------------------
	elsif params.fixAr then
	  declare
		type Index is (x0,dx,dlt,A1);
		type Vector is array(Index) of MyReal;

		function goal(v:Vector)return MyReal is
		begin
			return Base_Goal(x0=>v(x0), dx=>v(dx), dlt=>v(dlt),
				A1=>v(A1), A2=>fit.A2);
		end;
		procedure Simplex is new Generic_Simplex(MyReal,Index,Vector,goal);
		v : Vector := (x0=>fit.x0, dx=>fit.dx, dlt=>params.dlt, A1=>fit.A1);
		dv : constant Vector := ( x0=>v(x0)/2.0, dx=>v(dx)/2.0,
			dlt=>data.XStep/5.0, A1=>v(A1)/10.0 );

	  begin
		fit.eps := goal(v)/epsFactor;
		Simplex(v,dv,fit.eps,maxIter);
		fit.x0 := v(x0);
		fit.dx := v(dx);
		fit.dlt := v(dlt);
		fit.A1 := v(A1);
	end;

	------------------------------------
	else
	  declare -- all four unfixed
		type Index is (x0,dx,dlt,A1,A2);
		type Vector is array(Index) of MyReal;

		function goal(v:Vector)return MyReal is
		begin
			return Base_Goal(x0=>v(x0), dx=>v(dx), dlt=>v(dlt),
				A1=>v(A1), A2=>v(A2));
		end;
		procedure Simplex is new Generic_Simplex(MyReal,Index,Vector,goal);
		v : Vector := (x0=>fit.x0, dx=>fit.dx, dlt=>params.dlt,
			A1=>fit.A1, A2=>fit.A2);
		dv : constant Vector := ( x0=>v(x0)/2.0, dx=>v(dx)/2.0, dlt=>data.XStep/5.0,
			A1=>v(A1)/10.0, A2=>v(A2)/10.0 );

	  begin
		fit.eps := goal(v)/epsFactor;
		Simplex(v,dv,fit.eps,maxIter);
		fit.x0 := v(x0);
		fit.dx := v(dx);
		fit.dlt := v(dlt);
		fit.A1 := v(A1);
		fit.A2 := v(A2);
	end; end if;

end doFit;


------------------------------
-- main block


ComLineParams : ComLineParamRec;
fitParams : fitParamRec;
dat : MyAtf_IO.Atf_File;
results : FitArrayPtr;

begin
	processCommandLine(ComLineParams);
	dat := MyAtf_IO.getData(To_String(ComLineParams.InputFileName));
	-- data and some global params have to be adjusted now that we have both in
	adjustStuff(dat.data.all,ComLineParams);

	results := new FitArray(dat.data.data'Range(1));

	if ComLineParams.Debug then
		Put_Line("episodes in file: "& dat.data.data'First(1)'Img &" to "& dat.data.data'Last(1)'Img);
	end if;
	for episode in dat.data.data'Range(1) loop
		results(episode) :=
				assignSeedingValues(dat.data.all,episode,ComLineParams);
				--this just seeds fitparams with estimates
		if not results(episode).TooDistorted then
			doFit(dat.data.all,episode,results(episode),ComLineParams);
		end if;
	end loop;

	printResults(results.all,ComLineParams,dat.data.all);
		-- no time rescaling at this point
exception
	when Finish => null;
end Fit_Transitions;
