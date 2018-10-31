with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada;
with Ada.Integer_Text_IO;use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;  use Ada.Float_Text_IO;

with ATF_IO;

procedure tst_ATFIO1 is
	package MyAtf_IO is new Atf_IO(Float);
	use MyAtf_IO;

	data : Atf_File;
begin
	Put_Line("starting..");
	data := getData(Command_Line.Argument(1));

	New_Line;Put_Line("atf data:");
	Put("NEntries=");Put(Integer(data.data.NEntries));
	Put(";   Length=");Put(Integer(data.data.Length));
	New_LIne;

	Put("x0=");Put(Float(data.data.XStart));
	Put(";   dx=");Put(Float(data.data.XStep));
	New_LIne;

	Put_Line("Now the writeAtf dump:");New_Line;
	writeAtf(Atf => data);

end;
