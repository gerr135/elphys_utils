with ATF_IO;use ATF_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
with Ada.Integer_Text_IO;use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;  use Ada.Float_Text_IO;

with ATF_IO; use ATF_IO;

procedure tst_ATFIO is
	F:File_Type;
	Header:ATF_HeaderPtr;
	data : Atf_DataPtr;
begin
	Put_Line("starting..");
	Open(F,In_File,Command_Line.Argument(1));
	Header:=getHeader(F);
	Put_Line("tst_atfio: Header formed.");
	--printHeader(Header => Header);

	Flush;

	getData(F, Header.all, data);
	Close(F);
	New_Line;Put_Line("atf data:");
	Put("NEntries=");Put(Integer(data.NEntries));
	Put(";   Length=");Put(Integer(data.Length));
	New_LIne;

	Put("x0=");Put(Float(data.XStart));
	Put(";   dx=");Put(Float(data.XStep));
	New_LIne;


end;
