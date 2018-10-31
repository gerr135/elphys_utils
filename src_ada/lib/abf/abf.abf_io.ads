-- ???Copyright (c) 2003 George Shapovalov <george@gentoo.org>.  All rights reserved.

-- # This program is free software; you can redistribute it and/or
-- # modify it under the terms of the GNU General Public License as
-- # published by the Free Software Foundation; either version 2 of the
-- # License, or (at your option) any later version.
-- #
-- # This program is distributed in the hope that it will be useful,
-- # but WITHOUT ANY WARRANTY; without even the implied warranty of
-- # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- # GNU General Public License for more details.
-- #
-- # You should have received a copy of the GNU General Public License
-- # along with this program; if not, write to the Free Software
-- # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
-- # USA

-- This child package defines some general high-level ABF IO routines
-- Made ABFFile limited controlled to ease its handling
-- (plus explicit allocation/destruction management and assignment are
-- hardly necessary here)

with ABF.Header_Strict;

package ABF.ABF_IO is

type AbfFile is new Limited_Controlled with private;

procedure Open(F : in out AbfFile; Name : String);
	-- opens the file that Name points to, reads header and
	-- performs basic consistency checks

-- some general checks
function BinaryFmt(F : AbfFile) return BinaryFormat;
function OpMode(F:AbfFile) return OperationMode;
function FileType(F:AbfFile) return FileTypeRec;

procedure AssignDefaults(F : out AbfFile;
	opMode : OperationMode := GapFree;
	binFmt : BinaryFormat  := Float);
	-- creates a new header, no actual writing is performed

procedure Flush(F : AbfFile);
	-- flushes any pending writes to disk

procedure Close(F : AbfFile);
	-- flushes and closes the file



private

type AbfFile is new Limited_Controlled with record
	F : File_Type;
	Header : Header_Struct.ABFFileHeader;
	Dirty : Boolean := False; -- indicator of dirty buffers
end record;

procedure Initialize(F : AbfFile);
procedure Finalize(F:AbfFile);


end ABF.ABF_IO;