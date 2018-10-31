with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with sorted_Pairs;

procedure test_Sorted_Pairs is
	package MyList is new sorted_Pairs (Integer,Float,"<");	use MyList;
	lst : List(5);
begin
	Put_Line("Please enter list of numbers >");
	loop
		declare
			int : Integer;
		begin
			Get(int);
			addPair(lst, EntryRec'(int,Float(int)), drop => Max );
		exception
			when Data_Error => exit; -- exit loop on any non-numeric
		end;
	end loop; -- now we should have the structure formed
	--output
	New_Line;Put_Line("some largest entries:");
	for i in 1..getNumEntries(lst) loop
		Put("i=");Put(i,width=>2);Put("(");
		Put(getStorage(lst,i).key,width=>1); Put(",  ");
		Put(getStorage(lst,i).store,fore=>1); Put(");");
		New_Line;
	end loop;
end test_Sorted_Pairs;