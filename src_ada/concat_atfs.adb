with ATF_IO;
with Ada.Command_Line;
with Ada.Text_IO; with Ada.Integer_Text_IO;
use Ada.Text_IO;  use  Ada.Integer_Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters;
with Ada.Strings.Unbounded.Text_IO;
use Ada;

procedure concat_ATFs is

Finish : exception; -- insta-exit
Accepted_LengthDifference : constant := 0.1;
	-- turn this into parameter if necessary

	procedure printUsage is
	begin
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " file1 file2");
		New_Line;
		Put_Line("The program concatenates multiple atf files.");
		Put_Line("Files should contain fragments of similar lengths (10% deviation is tolerated)");
		Put_Line("output goes to stdout, ATF format");
	end printUsage;

package MyAtf_IO is new Atf_IO(Float);

type Atf_array is array(Positive range <>) of MyAtf_IO.Atf_File;
type AtfsPtr is access Atf_array;

	procedure ProcessCommandLine(files:out AtfsPtr) is
		use Command_Line;
	begin
		if Argument_Count = 0 or else
				( Argument_Count = 1 and Argument(1) = "-h" ) then
			printUsage;
			raise Finish;
		end if;
		files := new Atf_array(1..Argument_Count);
		for i in 1..Argument_Count loop
			files(i):=MyAtf_IO.getData(Argument(i));
		end loop;
	end;

files : AtfsPtr;
newFile : MyAtf_IO.Atf_File := (null,null);
totalEntries : MyAtf_IO.Count := 0;
minLength : MyAtf_IO.Count := MyAtf_IO.Count'Last;
maxLength : MyAtf_IO.Count := 0;

use MyAtf_IO; -- otherwise "+"'s, "<"'s, etc. will look ugly

begin
	processCommandLine(files);
	-- now that we got the data, check length correspondency
	-- and calculate totals
	for i in files'Range loop
		totalEntries := totalEntries + files(i).data.NEntries;
		if  minLength > files(i).data.Length then minLength := files(i).data.Length; end if;
		if  maxLength < files(i).data.Length then maxLength := files(i).data.Length; end if;
	end loop;
	--Put_Line("totalEntries="& totalEntries'Img &
	--		", minLength="& minLength'Img &",  maxLength="& maxlength'Img);
	if Float(maxLength - minLength)/Float(maxLength) > Accepted_LengthDifference then
		Put_Line("Sample lengths vary too much!");
		Put_Line("minLength="& minLength'Img &",  maxLength="& maxlength'Img);
		raise Finish;
	end if;
	-- the processing itself, header
	newFile.header := new Atf_header(Ncomments => 0,
			NEntries => totalEntries + 1 );
			-- Header.NEntries=data.NEntries+1, but all X-axis units
			-- except from the 1st file will get discarded

	newFile.Header.units(1..files(1).Header.units'Last) :=
		files(1).Header.units(files(1).Header.units'Range);
	declare
		entryNum : MyAtf_IO.Count := files(1).Header.units'Length;
		--use Latin_1;
	begin
		--Put("entryNum=    ");
		for fileInd in files'First+1 .. files'Last loop
			newFile.Header.units(entryNum+1 .. entryNum+files(fileInd).Header.units'Last-1 ) :=
				files(fileInd).Header.units(files(fileInd).Header.units'First+1 ..
					files(fileInd).Header.units'Last);
			--Text_IO.Put(BS&BS&BS&BS);Put(Integer(entryNum),Width =>4);
			entryNum := entryNum + files(fileInd).Header.units'Length-1;
		end loop;
		--New_Line;
	end;
	-- and the data
	newFile.data := new Atf_Data(NEntries => totalEntries, Length => minLength);
	newFile.data.XStart := files(1).data.XStart;
	newFile.data.XStep := files(1).data.XStep;
	declare
		entryNum : MyAtf_IO.Count := 0;
	begin
		for fileInd in files'Range loop
			for entryInd in 1 .. files(fileInd).data.NEntries loop
				entryNum := entryNum + 1;
				-- should I check for XStep consistency?
				for i in newFile.data.data'Range(2) loop -- 1..minLength
					newFile.data.data(entryNum, i) :=
						files(fileInd).data.data(entryInd, i);
				end loop;
			end loop;
		end loop;
	end;
	-- all ready, print results
	WriteAtf(newFile);
exception
	when Finish => null; -- normal termination, silently bail out
end;