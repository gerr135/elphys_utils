with Interfaces.C;

package ABF is
pragma Pure(ABF);

package C renames Interfaces.C;

-- general filetype definitions
type BinaryFormat  is (Int,Float);
type OperationMode is (VarLenEvents, FixLenEvents, GapFree, HighSpeed_Oscilloscope, Waveform);

type FileTypeRec is record
	binFmt : BinaryFormat;
	opMode : OperationMode;
end record;

-- channel types
type IntChannel   is array(Positive range <>) of C.short;
type floatChannel is array(Positive range <>) of C.C_float;
	-- individual channel arrays

type ChannelRec is record
end record;

end ABF;