with Ada.Text_IO; use Ada.Text_IO;
with ATF_IO;
with Ada.Command_Line;  use Ada;
with Ada.Strings.Unbounded.Text_IO;use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with GNAT.Command_Line;

procedure AlignFrags_vertically is
	package myAtf_IO is new Atf_IO(Float);use MyAtf_IO;

	Finish : Exception;

type ParamRec is record
	height  : Float := 0.0;  -- desired amplitude
	width   : Float := 20.0;
	Debug   : Boolean := False;
	Rescale : Boolean := True;
	DataFileName : Unbounded_String := Null_Unbounded_String;
end record;

	procedure printUsage is
	begin
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  file");
		Put_Line("output goes to stdout, ATF format");
		New_Line;
		Put_Line("options:");
		Put_Line("-h      print this help");
		Put_Line("-l f    desired amplitude");
		Put_Line("-w f    width of baseline region");
		Put_Line("-g      turn on debug output");
		Put_line("-x      do not rescale 1st column (s->mks and back)");
	end printUsage;

	function processCommandLine return ParamRec is
		use Integer_Text_IO;
		use GNAT.Command_Line;
		Options : constant String := "g h l: w: x";
		Last:Positive;
		--
		params : ParamRec;
	begin
		if Command_Line.Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;

		begin
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;
				when 'h' => printUsage;
				when 'l' => Float_Text_IO.Get(Parameter,Float(params.height),Last);
				when 'w' => Get(Parameter,Float(params.width),Last);
				when 'g' => params.Debug := True;
				when 'x' => params.Rescale := False;

				when others =>
					raise Program_Error;         -- should not get here!
			end case;
			end loop;
		exception
			when Invalid_Switch    => Put_Line ("Invalid Switch " & Full_Switch);
			when Invalid_Parameter => Put_Line ("No parameter for " & Full_Switch);
			when Data_Error        => Put_Line ("Invalid numeric format for switch " & Full_Switch);
		end;
		params.DataFileName := To_Unbounded_String(Get_Argument(Do_Expansion => True));
		return params;
	end processCommandLine;


	params : ParamRec := processCommandLine;
	Contents : Atf_File := getData(To_String(params.DataFileName));
	AvgLen : myAtf_IO.Count := myAtf_IO.Count( Float'Truncation(params.width/Contents.data.XStep) );
	dat : Float2DArray renames Contents.data.data;
begin
	for curve in dat'Range(1) loop
		-- calc average amplitude
		declare
			S : Float := 0.0;
		begin
			for i in dat'First(2) .. dat'First(2)+AvgLen-1 loop
				S := S + dat(curve,i);
			end loop;
			S := S/Float(AvgLen);
			-- got the average, now do adjustment
			for i in dat'Range(2) loop
				dat(curve,i) := dat(curve,i) + params.Height - S;
			end loop;
		end;
	end loop;

	WriteAtf(name => "", Atf => Contents);

	exception
		when Finish => null; --gracefully leave the app on the jump
end AlignFrags_Vertically;
