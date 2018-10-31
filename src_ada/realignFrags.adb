--    realignFrags -
--    reads fragmetns stored in an ATF file,
--    n-pt smoothes them and then realigns by max derivative on given interval
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
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters;
with GNAT.Command_Line;
with ATF_IO;
procedure realignFrags is

package MyAtf_IO is new Atf_IO(Float);

-- input paramters
type DerivDirection is (Down,Up);

type ParamRec is record
	AlignCenter: ThisFloat := 50.0; --mks
	AlignWidth : ThisFloat := 20.0; --mks
	NSmoothing : MyAtf_IO.Count := 5; -- width of smoothing window
	RescaleX : Boolean := True;
	Debug : Boolean := False;
	DataFileName:Unbounded_String := Null_Unbounded_String;
	DerivStep : MyAtf_IO.Count := 5; -- will look at "average" derivative
		-- with linear derivative definition this just means y_i+n - y_i
		-- use 5 for now, because that ~ corresponds to 10-90 rise time
	DerivDir : DerivDirection := Down; -- onset of downwards "opening"
end record;

	procedure printUsage is
	begin
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " options  file");
		Put_Line("output goes to stdout, ATF format");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-c f    center of alignment (mks)");
		Put_Line("-w f    alignment window half-width (mks)");
		Put_Line("-g      turn on debug output");
		Put_Line("-n n    N points in smoothing window, use 5, 7 or 9 (default 5))");
		Put_Line("-d n    N points for derivative calculation (default 5)");
		Put_Line("-u      align upwards transitions (default downwards)");
		Put_line("-x      do not rescale 1st column (s->mks and back)");
	end printUsage;

	procedure processCommandLine(params : out ParamRec) is
		use Integer_Text_IO;
		use GNAT.Command_Line;
		Options : constant String := "c: d: g h n: u w: x";
		Last:Positive;
	begin
		begin
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;
				when 'h' => printUsage;
				when 'c' => Get(Parameter,Float(AlignCenter),Last);
				when 'w' => Get(Parameter,Float(AlignWidth),Last);
				when 'g' => Debug := True;
				when 'n' => Get(Parameter,Integer(NSmoothing), Last);
				when 'd' => Get(Parameter,Integer(DerivStep), Last);
				when 'x' => RescaleX := False;
				when 'u' => DerivDir := Up;

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
			end loop;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
			when Data_Error        => Put_Line ("Invalid numeric format for switch" & Full_Switch);
		end;
		DataFileName := To_Unbounded_String(Get_Argument(Do_Expansion => True));
		-- some consistency checks
		if AlignWidth >= AlignCenter then
			Put_Line("width is larger than center position!");
		end if;
	end processCommandLine;


-- principal data handling procedures
	function smooth(S : ATF_Data; -- input data
				Npt : Atf_IO.Count)   -- N smoothing points (simple average)
				return ATF_DataPtr is

		tmp : Float2DArray(S.data'Range(1), S.data'Range(2)) := (others => (others => 0.0));
		hw : constant Atf_IO.Count := Npt/2; -- smoothing window half-width
	begin
		for col in S.data'Range(1) loop
			--leave NSmoothing/2 points intact on both ends
			for i in S.data'First(2) .. S.data'First(2)+ hw - 1 loop
				tmp(col,i) := S.data(col,i);
			end loop;
			--now do the smoothing
			for i in S.data'First(2)+hw .. S.data'Last(2)-hw loop
				declare
					tmp1 : ThisFloat := 0.0;
				begin
					for j in -hw .. hw loop
						tmp1 := tmp1 + S.data(col,i+j);
					end loop;
					tmp(col,i) := tmp1;
				end;
			end loop;
			--skip again
			for i in S.data'Last(2)- hw +1 .. S.data'Last(2) loop
				tmp(col,i) := S.data(col,i);
			end loop;
		end loop;
		return new Atf_Data'(NEntries => S.NEntries, Length => S.Length,
			data => tmp, XStart => S.XStart, XStep => S.XStep ); -- from the record
	end smooth;


type IndexArray is array(Atf_IO.Count range <>) of Integer;
-- use for adjustment deltas (0 = no shift, negatives - shift to the left)
-- no sub-step positioning as all calculations are only
-- stepwise defined

	procedure CalcDeltas(S:Atf_Data; dlt: in out IndexArray; center,width : ThisFloat) is
		-- for every column
		-- find max (negative) derivative on the interval of search
		-- calc delta to put max at center
		iMin,iMax:Atf_IO.Count;
		nCenter : constant Atf_IO.Count :=
			Atf_IO.Count( 1.0 + ThisFloat'Floor((center - S.XStart)/S.XStep) );
		-- index of the center
	begin
		-- determine region to search
		if width > center then
			iMin := S.data'First(2) + NSmoothing/2;
			-- start at the very edge of smoothed region
		else
			iMin := Atf_IO.Count( 1.0 + ThisFloat'Floor((center - width - S.XStart)/S.XStep) );
			--array bounds start at 1
		end if;
		if width+center > S.XStart + ThisFloat(S.Length)*S.XStep  then
			iMax := S.data'Last(2) - NSmoothing/2;
		else
			iMax := Atf_IO.Count( 1.0 + ThisFloat'Floor((center + width - S.XStart)/S.XStep) );
		end if;
		if Debug then -- debug info
			Put("iMin=");Put(Integer(iMin), Width=>1);Put(",  iMax=");
			Put(Integer(iMax), Width=>1);New_Line;
		end if;
		-- basic consistency check
		if iMin >= iMax then
			Put_Line("alignment range appears to be null (iMin>iMax). Please check your input parameters!!");
			raise Constraint_Error;
		end if;

		-- the adjustment calculation itself
		if Debug then Put("nset=    "); end if;
		for nset in S.data'Range(1) loop
			if Debug then Put(To_String(4*Latin_1.BS));Put(Integer(nset),width=>4); end if;
			declare
				maxDer, der : ThisFloat := S.data(nset,iMin+DerivStep) - S.data(nset,iMin);
				nMAx : Atf_IO.Count := iMin;
				-- no need to divide by step, since we are just looking
				-- for maximum (amplitude) value (which itself is negative)
			begin
				for i in iMin +1 .. iMax - DerivStep loop
					-- already calculated @iMin, so start @ +1
					der := S.data(nset,i+DerivStep) - S.data(nset,i);
					if (der < maxDer and derivDir=Down) or (der > maxDer and derivDir=up)
					then -- preset direction
						maxDer := der;
						nMax := i;
					end if;
				end loop;
				-- finally assign delta
				dlt(nset) := Integer(nMax) - Integer(nCenter);
				--convert to Integer both separately to avoid Range_Error;
-- 				if Debug then
-- 					Put(";  dlt[");Put(Integer(nset),Width=>1);
-- 					Put("]=");Put(dlt(nset),width=>1);New_Line;
-- 				end if;
			end;
		end loop;
	end;


type SubDeltaArray is array(Atf_IO.Count range <>) of Float;
-- use for adjustment deltas (0 = no shift, negatives - shift to the left)


	function adjustData(dat: Atf_Data; dlt : IndexArray) return Atf_DataPtr is
		-- adjusts unsmoothed (original) set of data
		-- given array of deltas
		dltMin, dltMax : Integer := 0; -- need to constraint min<=0, max>=0
		newDat : Atf_DataPtr;
	begin
		-- determine min/max deltas
		for i in dlt'Range loop
			if dlt(i) < dltMin then dltMin := dlt(i); end if;
			if dlt(i) > dltMax then dltMax := dlt(i); end if;
		end loop;
		if Debug then
			New_Line;Put("dltMin=");Put(dltMin,Width=>1);Put(",  dltMax=");
			Put(dltMax,width=>1);Put_Line(";");
		end if;
		-- shifts can be defned as:
		-- a'(i) = a(i + dlt);
		newDat := new Atf_Data(NEntries => dat.NEntries,
					Length => dat.Length - Atf_IO.Count(dltMax-dltMin) );
		newDat.XStart := dat.XStart;
		newDat.XStep := dat.XStep;
		--and the array itself
		for nset in dat.data'Range(1) loop
			if Debug then
				New_Line;
				Put("for nset=");Put(Integer(nset), Width => 1);
				Put(";  dlt=");Put(dlt(nset),width=>1);
				Put(",  (");Put(Integer(dat.data'First(2)),width=>1);Put("..");
				Put(Integer(dat.data'Last(2)),width=>1);
				Put(");  i=    "); -- for the shift loop
			end if;
			for i in 1 + Atf_IO.Count(-dltMin) .. dat.data'Last(2) - Atf_IO.Count(dltMax)  loop
				--without constraint on dltMin/dltMax in declaration
				--indeces can get out of range
				if Debug then
					Put(To_String(4*Latin_1.BS));Put(Integer(i)+dlt(nset)+1,Width=>4);
				end if;
				newDat.data(nset, i - Atf_IO.Count(-dltMin)) :=
					dat.data(nset, Atf_IO.Count(Integer(i) + dlt(nset) ) );
			end loop;
		end loop;
		return newDat;
	end adjustData;

-- main block
	F:File_Type;
	data : Atf_File;
	adjDataPtr:Atf_DataPtr;
begin
	processCommandLine;
	data := Atf_IO.getData(To_String(DataFileName));
	if RescaleX then
		-- internally keep x in mks!!!
		data.data.XStart := data.data.XStart * 1000_000.0;
		data.data.XStep := data.data.XStep * 1000_000.0;
	end if;

	if Debug then
		Put("NEntries=");Put(Integer(data.data.NEntries));
		Put(";  Length=");Put(Integer(data.data.Length));
		New_Line;
		Put("xstart=");Put(Float(data.data.XStart));
		Put(",   xstep=");Put(Float(data.data.XStep));
		New_Line;
	end if;

	-- realign to points by max difference
	declare
		smoothed : Atf_DataPtr := smooth(data.data.all,NSmoothing);
		deltas : IndexArray(1 .. data.data.NEntries) := (others => 0);
		--will hold alignment dInd values
	begin
		--prepare smoothed
		--smooth(smoothed.data,);
		CalcDeltas(smoothed.all, deltas, AlignCenter, AlignWidth);
		adjDataPtr := AdjustData(data.data.all, deltas);
	end;

	-- sub-timestep realignment, use simplex minimization
	declare
		--subDeltas : subDeltasArray := calcSubDeltas(adjDataPtr,)
	begin
		null;
	end;

	-- all done, output results
	if RescaleX then
		-- convert time (X column) back to seconds
		adjDataPtr.XStart := adjDataPtr.XStart / 1000_000.0;
		adjDataPtr.XStep  := adjDataPtr.XStep  / 1000_000.0;
	end if;
	writeHeader(Header => data.Header.all);
	writeData  (Data => adjDataPtr.all);
end realignFrags;
