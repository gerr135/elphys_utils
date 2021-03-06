--with Ada.Text_IO;use Ada.Text_IO;
with Ada.Characters.Latin_1;use Ada.Characters;
with Ada.Integer_Text_IO;use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;  use Ada.Float_Text_IO;

package body ATF_IO is

function getHeader(F:File_Type) return ATF_HeaderPtr is
	Header : String(1..3);AtfVer : Float;
	numread:integer;
	NComments,NEntries : Count;
	FileHeader : ATF_HeaderPtr;
begin
	Get_Line(F,Header,numread);Get(F,AtfVer);Skip_Line(F);
	--Put("numread=");Put(numread);Put(";   header='" & Header(1..numread) & "', ver=");Put(AtfVer);New_Line;
	--Put("Expected'Length=");Put(HeaderLine'Length);Put_Line(";   expected='" & HeaderLine & "'");
	if numread /= HeaderLine'Length or Header /= HeaderLine then
		raise File_FormatError;
	end if;
	Get(F, Integer(NComments)); Get(F, Integer(NEntries)); Skip_Line(F);
	--Put("NComments=");Put(Integer(NComments));
	--Put(";  NEntries=");Put(Integer(NEntries));New_Line;

	--now we can initialize the structure
	FileHeader := new ATF_Header(NComments,NEntries);
	for i in 1..NComments loop
		--couldn't find a standard function to reas unbounded string
		declare
			bufLen : constant Integer :=128;
			tmpStr:String(1..bufLen);
		begin
			FileHeader.comments(i) := Null_Unbounded_String;
			loop
				Get_Line(F, tmpStr, numread);
				FileHeader.comments(i) := FileHeader.comments(i) & tmpStr(1..numread) ;
				exit when numread /= bufLen;
			end loop;
		end;
	end loop;
	--Put_Line("comments done..");

	--now units
	for i in 1..NEntries loop
		--Put("i=");Put(Integer(i));Put(";  ");
		declare
			c : Character;
			str:Unbounded_String := Null_Unbounded_String;
		begin
			Get(F,c);
			if c /= '"' then raise File_FormatError; end if;
			loop
				Get(F, c);
				--Put(c);
				exit when c='"';
				str := str & c;
			end loop;
			if i<NEntries then --skip this if last entry
				Get (F, c);
				-- all fields are tab-delimited
				if c /= Latin_1.HT then raise File_FormatError; end if;
			else
				--Put("Last unit,");
				Skip_Line(F); --set position at the start of data
				--Put("Skip_Line finished");
			end if;
			FileHeader.units(i):= str;
		end;
		--New_Line;
	end loop;
	--Put_Line("ended units loop");


	--finally Header is filled
	return FileHeader;
end getHeader;


procedure printHeader(F:File_Type := Standard_Output; Header:ATF_HeaderPtr) is
begin
	Put_Line("ATF header info:");
	Put("NComments=");Put(Integer(Header.NComments));Put(";   ");
	Put("NEntries=");Put(Integer(Header.NEntries));Put_Line(";");
	for i in 1 .. Header.NComments loop
		Put_Line(To_String(Header.comments(i)));
	end loop;
	for i in 1 .. Header.NEntries loop
		Put('"');Put(To_String(Header.units(i)));Put('"' & Latin_1.HT);
	end loop;
	New_Line(2);
end;

procedure readData(F: in out File_Type; Header:ATF_Header; dataPtr : out Atf_DataPtr) is
	SavePos : constant Positive_Count := Line(F); --save present position,
	--as we are supposedly right at the data start
begin
	declare
		x0,dx:TheFloat;
		NumLines:Count := 0;
	begin
		Get(F,Float(x0)); Skip_Line(F);  -- get first x
		Get(F,Float(dx)); Skip_Line(F);
		dx := dx - x0; -- calculate dx
		-- ATF files contain 1st column of x values
		-- which are autogenerated
		NumLines :=2;
		loop
			Skip_Line(F);
			NumLines := NumLines + 1;
			exit when End_of_File(F);
		end loop;

		--instantiate the data
		DataPtr := new Atf_Data (NEntries => Header.NEntries-1 , Length => NumLines);
		DataPtr.XStart := x0; DataPtr.XStep := dx;
		DataPtr.data := (others => (others => 0.0));
	end;

	--now we can reset the file and read the bulk of it
	--Put("N data lines = ");Put(Integer(NumLines));New_Line;
	Ada.Text_IO.Reset(F);
	Set_Col(F,1);
	Set_Line(F,SavePos);
	--Put("reset file pos, current col=");Put(Integer(Col(F)));New_Line;

	declare
		tmpX : TheFloat;
	begin
		for i in 1 .. DataPtr.Length loop
			Get(F,Float(tmpX)); -- get rid of 1st column entry
			--Put("x=");Put(Float(tmpX));Put(";   (j,y[j])=");
			for j in 1 .. DataPtr.NEntries loop
				Get(F,Float(DataPtr.data(j,i)));
				--Put("(");Put(Integer(j));Put(",");
				--Put(Float(DataPtr.data(j,i)));Put(")  ");
			end loop;
			Skip_Line(F);
			--New_Line;
		end loop;
	end;
end;


function getData(name : String) return Atf_File is
	F:File_Type;
	Atf : Atf_File := (null, null);
begin
	Open(F,In_File,name);
	Atf.Header := getHeader(F);
	readData(F, Atf.Header.all, Atf.data);
	Close(F);
	return Atf;
end getData;


---------------------------------------------
-- output routines

procedure writeHeader(F:File_Type := Standard_Output; Header:Atf_Header) is
begin
	Put_Line(F,HeaderLine & Latin_1.HT & "1.0");
	Put(Integer(Header.NComments), Width => 1); --strip any leading blanks
	Put(Latin_1.HT);
	Put(Integer(Header.NEntries), Width => 1);
	New_Line;
	-- start writing comments
	for i in Header.comments'Range loop
		Put_Line(F,To_String(Header.comments(i)));
	end loop;
	-- units
	for i in Header.units'Range loop
		Put(F,'"');Put(F,To_String(Header.units(i)));Put(F,'"' & Latin_1.HT);
	end loop;
	New_Line(F);
end writeHeader;

procedure writeData(F:File_Type := Standard_Output; Data:Atf_Data) is
	-- Clampfit cannot read atf files if time column has positive exponent
	useExp : Field := 2;
begin
	if Data.XStep > 0.1 then useExp := 0; end if;
	for i in Data.data'Range(2) loop
		Put(F,Float(Data.XStart) + Float(i-1) * Float(Data.XStep),
			Fore => 1, Exp => useExp); -- x value
		Put(F,Latin_1.HT);
		for col in Data.data'Range(1) loop -- Header.NEntries-1
			Put(F, Float(Data.data(col,i)), Fore => 2);
			Put(F,Latin_1.HT);
		end loop;
		New_Line(F);
	end loop;
end writeData;

procedure writeAtf(Atf : Atf_File; name : String := "") is
begin
	writeAtf(Atf.Header.all,Atf.Data.all,name);
end writeAtf;

procedure writeAtf(Header:Atf_Header; Data:Atf_Data; name : String := "") is
	F:File_Type;
begin
	if Header.NEntries /= Data.NEntries+1 then
		raise Header_Data_MismatchError;
	end if;
	if name /= "" then
		Create(F,Out_File,name);
		writeHeader(F,Header);
		writeData(F,Data);
		Close(F);
	else
		writeHeader(Header => Header);
		writeData  (Data => Data);
	end if;
end writeAtf;

end ATF_IO;
