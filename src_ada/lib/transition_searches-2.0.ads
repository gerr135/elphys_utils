with Atf_IO;

generic
	type Real is digits <>;
	with package MyAtf_IO is new Atf_IO(TheFloat => Real);
		-- make sure they both operate on the same Real
package Transition_Searches is

use MyAtf_IO;

-- common stuff

All_Diverged : exception;
	-- raised when trying to adjust data where no episodes
	-- have fits converged

type Transition_Direction is (Down,Up,Autodetect);
--type Steps_to_do is (MaxDiff, Boltzman, Both);

type IndexArray is array(MyAtf_IO.Count range <>) of Integer;
	-- use for adjustment deltas (0 = no shift, negatives - shift to the left)
	-- or for index positions

type FitParamRec is record
	A1, A2 : Real := 0.0; -- amplitudes,
	x0,dx : Real := 0.0;   -- midpoint and slope of distribution
	eps : Real := 0.0;    -- minimal chi2
	tooDistorted : Boolean := False;
		-- set during assigning seedings, indicates whether actual fit is skipped

	skipAlignment : Boolean := False;
		-- set if fi diverges (returned x0 outside search area)
end record;

type FitArray is array(MyAtf_IO.Count range <>) of FitParamRec;
type FitArrayPtr is access FitArray;



-- smothes episodes via running average
-- returns smoothed copy (so that original, unfiltered data could be reused)
function smooth(S : ATF_Data; -- input data
				Npt : MyAtf_IO.Count)   -- Width of smoothing interval
				return ATF_DataPtr;


-- makes sence to recalculate float boundaries into indeces only once
procedure calcIndeces_of_Search_Region (S : Atf_Data;
		center,width : Real;
		iMin, iMax : out MyAtf_IO.Count;
		Debug : Boolean := False);


-- estimate A1/A2 and do some basic consistency checking
-- Will set tooDistorted for given episode if difference < stdError
procedure assignAmplitudes(dat : Atf_Data;
		fits : in out FitArray;
		iMin, iMax : MyAtf_IO.Count; -- estimate inside at these points
		Npt : MyAtf_IO.Count := 7); -- N points for the estimate window


-- search for max diffs, according to direction
-- !!NOTE: A1/A2 should already be seeded!
procedure FindMaxDiffs(S:Atf_Data;  -- data to process
		fits: in out FitArray;      -- array of max positions
		iMin, iMax : MyAtf_IO.Count;  -- region to search
		NDiff : MyAtf_IO.Count := 3;  -- step size
		direction : Transition_Direction := Autodetect;
		Debug : Boolean := False);


-- performs Boltzman fits to the episodes.
-- No parameters are constrained
procedure doFit(dat : FloatArray;  -- takes
		fit : in out FitParamRec;
		XStart, XStep : Real;
		Debug : Boolean := False);


-- aligns episodes according to fit results
function adjustData(dat: Atf_Data;
		fit : FitArray;
		Center : Real; AlignByCenter : Boolean := False;
			-- pass params.CenterIsGiven
			-- if true, aligns by Center valuse, otherwise by average transition x0
		outputSkipped : Boolean := True;
		Debug : Boolean := False) return Atf_DataPtr;


end Transition_Searches;
