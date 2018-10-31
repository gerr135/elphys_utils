with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Generic_simplex;
with Quadratic_Splines;

package body Transition_Searches is

-----------------
function smooth(S : ATF_Data; -- input data
				Npt : MyAtf_IO.Count)   -- Width of smoothing interval
				return ATF_DataPtr is

	tmp : MyAtf_IO.Float2DArray(S.data'Range(1), S.data'Range(2))
			:= (others => (others => 0.0));
	hw : constant MyAtf_IO.Count := Npt/2; -- smoothing window half-width

begin
		for episode in S.data'Range(1) loop
			--leave NSmoothing/2 points intact on both ends
			for i in S.data'First(2) .. S.data'First(2)+ hw - 1 loop
				tmp(episode,i) := S.data(episode,i);
			end loop;
			--now do the smoothing
			for i in S.data'First(2)+hw .. S.data'Last(2)-hw loop
				declare
					tmp1 : Real := 0.0;
				begin
					for j in -hw .. hw loop
						tmp1 := tmp1 + S.data(episode,i+j);
					end loop;
					tmp(episode,i) := tmp1;
				end;
			end loop;
			--skip again
			for i in S.data'Last(2)- hw +1 .. S.data'Last(2) loop
				tmp(episode,i) := S.data(episode,i);
			end loop;
		end loop;
		return new Atf_Data'(NEntries => S.NEntries, Length => S.Length,
			data => tmp, XStart => S.XStart, XStep => S.XStep ); -- from the record
end smooth;


----------
procedure calcIndeces_of_Search_Region (S : Atf_Data;
		center,width : Real;
		iMin, iMax : out MyAtf_IO.Count;
		Debug : Boolean := False) is

	nCenter : constant MyAtf_IO.Count :=
		MyAtf_IO.Count( 1.0 + Real'Floor((center - S.XStart)/S.XStep) );
		-- index of the center

	use Ada.Integer_Text_IO;

begin
	-- determine region to search
	if width > center then
		iMin := S.data'First(2);
		-- start at the very edge of smoothed region
	else
		iMin := S.data'First(2) + MyAtf_IO.Count( Real'Floor((center - width - S.XStart)/S.XStep) );
		--array bounds start at 1
	end if;
	if width+center > S.XStart + Real(S.Length)*S.XStep  then
		iMax := S.data'Last(2);
	else
		iMax := S.data'First(2) + MyAtf_IO.Count( Real'Floor((center + width - S.XStart)/S.XStep) );
	end if;

	-- basic consistency check
	if iMin >= iMax then
		Put_Line("alignment range appears to be null (iMin>iMax). Please check your input parameters!!");
		raise Constraint_Error;
	end if;
end;


---------------
procedure assignAmplitudes(dat : Atf_Data;
		fits : in out FitArray;
		iMin, iMax : MyAtf_IO.Count;
		Npt : MyAtf_IO.Count := 7) is
begin
	for episode in dat.data'Range(1) loop
		declare
			S1, S2, SD1, SD2 : Real := 0.0;
		begin
			for i in iMin .. iMin + Npt-1 loop
				S1 := S1 + dat.data(episode,i);
				SD1 := SD1 + dat.data(episode,i)**2;
			end loop;
			fits(episode).A1 := S1/Real(Npt);

			for i in iMax - Npt + 1 .. iMax loop
				S2 := S2 + dat.data(episode,i);
				SD2 := SD2 + dat.data(episode,i)**2;
			end loop;
			fits(episode).A2 := S2/Real(Npt);

			-- check if difference is masekd by noise
			if 2.0 * (S2-S1)**2 * Real(Npt*(Npt-1)) < SD1 + SD2 then
				-- A2-A1 < average stdError
				fits(episode).tooDistorted := True;
			end if;
		end;
	end loop;
end;


procedure FindMaxDiffs(S:Atf_Data;
		fits: in out FitArray;
		iMin, iMax : MyAtf_IO.Count;
		NDiff : MyAtf_IO.Count := 3;
		direction : Transition_Direction := Autodetect;
		Debug : Boolean := False) is

begin
	for episode in S.data'Range(1) loop
		declare
			diff, MaxDiff : Real := 0.0;
				-- note that while it is possible to have all diffs on the "wrong" side of zero
				-- in the middle part, it would indicate a *really* misshaped transition
				-- so the fit later on would not converge anyway.

			sgn : Real := Real'Copy_Sign(1.0, fits(episode).A2 - fits(episode).A1);
				-- sign of transition. Use to change ">" into "<" for downwards transition

			tmpI : MyAtf_IO.Count := S.data'First(2);

			fit : fitParamRec renames fits(episode);
				-- a convenience rename

		begin
			-- set sgn accordingly to direction (if given)
			if direction = Up then
				sgn := 1.0;
			elsif direction = Down then
				sgn := -1.0;
			-- else its Autodetect and we already assigned that one
			end if;

			for i in iMin + 1 .. iMax - NDiff loop
					-- should cut even more (NAvg) on both ends
					-- will just cut 1 at the beginning
					-- to make S.data'First(2) impossible inside loop
				diff := S.data(episode,i+NDiff) - S.data(episode,i);
				if diff * sgn > MaxDiff * sgn then
					MaxDiff := diff;
					tmpI := i;
				end if;
			end loop;

			if tmpI = S.data'First(2) then
				-- no valid transitions for set orientation were found
				fit.x0 := S.XStart;
				fit.tooDistorted := True;
				if Debug then
					Put_Line("episode "& episode'Img &"is too distorted");
				end if;
			else
				fit.x0 := S.XStart +
					S.XStep*Real(tmpI + NDiff/2 - S.data'First(2));
			end if;

		end;
	end loop;
end;



------------------------
-- doFit

procedure doFit(dat : FloatArray;
		fit : in out FitParamRec;
		XStart, XStep : Real;
		Debug : Boolean := False) is

	function Base_Goal(x0,dx,A1,A2:Real) return Real is

		function Boltzman(x:Real; x0,dx,A1,A2:Real) return Real is
			package MyFunctions is new Ada.Numerics.Generic_Elementary_Functions(Real);
			use MyFunctions;
		begin
			return A2 + (A1-A2)/(1.0 + Exp((x-x0)/dx));
		end;
		pragma Inline(Boltzman);

		S : Real := 0.0;
	begin
		for i in dat'Range loop
			S := S + ( dat(i) -
				Boltzman( XStart + XStep*Real(i-dat'First),
				x0 => x0, dx => dx, A1 => A1, A2 => A2) )**2;
		end loop;
		return S/Real(dat'Length);
	end;

	type Index is (x0,dx,A1,A2);
	type Vector is array(Index) of Real;

	function goal(v:Vector)return Real is
	begin
		return Base_Goal(x0=>v(x0), dx=>v(dx),
			A1=>v(A1), A2=>v(A2));
	end;

	procedure Simplex is new Generic_Simplex(Real,Index,Vector,goal);

	v : Vector := (x0=>fit.x0, dx=>fit.dx, A1=>fit.A1, A2=>fit.A2);
	dv : constant Vector := ( x0=>v(x0)/7.0, dx=>v(dx)/5.0,
			A1=>v(A1)/17.0, A2=>v(A2)/17.0 );

	-- some general vars used in Simplex invocation
	maxIter : Positive := 100;
	ReduceEps : constant Real := 10000.0;
		-- trying to get the best fit. Eps is unlikely to be reduced by this much

begin

	fit.eps := goal(v)/ReduceEps;
	Simplex(v,dv,fit.eps,maxIter, quiet => not Debug);

	fit.x0 := v(x0);
	fit.dx := v(dx);
	fit.A1 := v(A1);
	fit.A2 := v(A2);

	if fit.x0 < XStart or fit.x0 > XStart + XStep*Real(dat'Length) then
		fit.skipAlignment := True;
		if Debug then Put_Line("fit diverged!"); end if;
	end if;
	if Debug then New_Line; end if;

end doFit;



-------------------------
function adjustData(dat: Atf_Data;
		fit : FitArray;
		Center : Real;
		AlignByCenter : Boolean := False;
		outputSkipped : Boolean := True;
		Debug : Boolean := False) return Atf_DataPtr is

	package MySplines is new Quadratic_Splines(Real,
		Index=>MyAtf_IO.Count, Vector=>MyAtf_IO.FloatArray);
	use MySplines;

	newDat : Atf_DataPtr;
		-- global interval has to only include area defined in all (shifted)
		-- episodes, so it will be compressed,
		-- but we will also decrease step. So resampling has to be done in any case

	minX0 : Real := Real'Last;
	maxX0 : Real := Real'First;
	useCenter : Real := Center;
		-- boundaries and center of x0's

	newEpisodeLength : MyAtf_IO.Count;
		-- we have compressed interval and resample it with smaller step..

	newNEpisodes : MyAtf_IO.Count := dat.NEntries;
		-- in case we skip episodes that diverged

	skippedEpisodes : MyAtf_IO.Count := 0;
		-- track these diverged episodes

begin
	-- check how many episodes converged and whether we output
	-- the ones that did not converge
	if not outputSkipped then
		for i in fit'Range loop
			if fit(i).skipAlignment then
				newNEpisodes := newNEpisodes - 1;
			end if;
		end loop;
	end if;

	if newNEpisodes = 0 then
		-- not a single episode converged!!
		raise All_Diverged;
	end if;

	-- find min and max shifts
	for episode in fit'Range loop
		if not fit(episode).skipAlignment then
			if minX0 > fit(episode).x0 then minX0 := fit(episode).x0; end if;
			if maxX0 < fit(episode).x0 then maxX0 := fit(episode).x0; end if;
		end if;
	end loop;
	if Debug then
		Put_Line("min/max shifts: minX0="& minX0'Img &",  maxX0="& maxX0'Img);
	end if;

	newEpisodeLength :=
		2*dat.data'Length(2) - 1 -
		MyAtf_IO.Count( Real'Ceiling( 2.0*( maxX0-minX0 )/dat.XStep ) );
		-- trying to compensate for dispersin due to moves by doubling sampling
	if Debug then
		Put_Line("new episode length = "& newEpisodeLength'Img);
	end if;

	-- check if we align by center and calc average x0 if necessary
	-- omit all non-converged episodes, as was done for min/maxX0
	if not AlignByCenter then
		declare
			S : Real := 0.0;
		begin
			for episode in fit'Range loop
				if not fit(episode).skipAlignment then
					S := S + fit(episode).x0;
				end if;
			end loop;
			useCenter := S/Real(newNEpisodes);
		end;
	end if;
	if Debug then
		Put_Line("useCenter = "& useCenter'Img);
	end if;


	-- finally we can create newData
	newDat := new Atf_Data(NEntries => newNEpisodes,
			Length => newEpisodeLength );

	newDat.XStart := dat.XStart - (minX0 - useCenter);
	newDat.XStep  := dat.XStep/2.0;


	for episode in dat.data'Range(1) loop
		declare
			xVec, yVec : MyAtf_IO.FloatArray(dat.data'Range(2));
			S : AverageSpline;
			x0 : Real := fit(episode).x0;
		begin
			-- if fit diverged:
			if fit(episode).skipAlignment then
				if outputSkipped then
					-- do not move it
					x0 := useCenter;
				else
					-- or skip altogether
					skippedEpisodes := skippedEpisodes + 1;
						-- need to adjust index of allocated data array
					goto Block_End;
				end if;
			end if;

			-- fill xVec with adjusted x's
			-- and yVec with episode data
			for i in xVec'Range loop
				xVec(i) := dat.XStart - (x0 - useCenter) +
					dat.XStep*Real(i-xVec'First);
				yVec(i) := dat.data(episode,i);
			end loop;

			-- initiate and then tabulate spline
			S := Create(xVec,yVec);
			for i in newDat.data'Range(2) loop
				declare
					x : Real := newDat.XStart +
						newDat.XStep * Real(i-newDat.data'First(2));
				begin
					newDat.data(episode - skippedEpisodes, i ) := Evaluate(S, x);
				exception
					when XOutOfRange =>
						Put_Line("XOutOfRange raised in adjustData");
						Put_Line("episode="& episode'Img &", i="& i'Img
							&", at x="& x'Img );
						raise;
				end;
			end loop;

			<<Block_End>>
			null;
		end;
	end loop;

	return newDat;

end adjustData;

end Transition_Searches;
