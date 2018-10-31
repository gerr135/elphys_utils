with Ada.Text_IO;use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;use Ada.Characters;

generic
	type TheFloat is digits <>;
package ATF_IO is

File_FormatError:exception;
Header_Data_MismatchError:exception;

--HeaderLine: constant string := "ATF"&Latin_1.HT&"1.0";
HeaderLine : constant string := "ATF";

type Count is range 0 .. Natural'Last;
-- define Count here, as relying on 0 and other stuff is handy
-- full scale is quite large, but may be necessary

type StringArray is array(Count range <>) of Unbounded_String;

type ATF_Header(NComments,NEntries : Count := 0) is limited record
	comments:StringArray(1 .. NComments);
	--place to store comments in case they are needed
	units:StringArray(1 .. NEntries);
	--reads in all the units
end record;
type ATF_HeaderPtr is access ATF_Header;

--some data types
type FloatArray is array(Count range <>) of TheFloat;
type FloatArrayPtr is access FloatArray;
type Float2DArray is array (Count range <>, Count range <>) of TheFloat;

type Atf_Data (NEntries,Length:Count := 0) is record
		-- Length data lines in file
		-- NEntries is 1 less than in header - skip 1st (X) column
	XStart,XStep : TheFloat;
	data: Float2DArray(Count range 1 .. NEntries, Count range 1 .. Length);
end record;
type Atf_DataPtr is access Atf_Data;

type Atf_File is record
	Header : Atf_HeaderPtr;
	data :   Atf_DataPtr;
end record;
type Atf_FilePtr is access Atf_File;

--keep opening/closing files on the same (top) level
function getHeader(F:File_Type) return ATF_HeaderPtr;
--takes freshly opened file, checks header and returns initialized record
--constructor of sorts. stores access to F just in case
--leaves file position right at the data

procedure PrintHeader(F:File_Type := Standard_Output; Header: ATF_HeaderPtr);
-- quick and dirty test-print of header

procedure readData(F: in out File_Type; Header:ATF_Header; DataPtr: out Atf_DataPtr);
-- reads the data portion, initializes the record;
-- note, this function expects file position to be at the data start;

procedure writeHeader(F:File_Type := Standard_Output; Header:Atf_Header);
--writes header in proper format to open file from active position

procedure writeData(F:File_Type := Standard_Output; Data:Atf_Data);
-- writes data to file from current position

procedure writeAtf(Atf : Atf_File; name : String := "");
procedure writeAtf(Header:Atf_Header; Data:Atf_Data; name : String := "");
-- opens file and writes both header and data in proper format
-- checks that header corresponds to the data (NEntries match)
-- raises Header_Data_MismatchError otherwise
-- use writeHeader/writeData to write inconsistent stuff..
--
-- default name - "", corresponds to Standard_Output

function getData(name : String) return Atf_File;
-- "high level" data access function

end ATF_IO;
