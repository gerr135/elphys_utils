--with Ada.Text_IO; use Ada.Text_IO;
--with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body sorted_Pairs is

function minKey(lst:List) return Key_Type is
begin
	return lst.lst(1).key;
end;

function maxKey(lst:List) return Key_Type is
begin
	return lst.lst(lst.N).key;
end;

function getStorage(lst:List; key:Key_Type) return Storage is
begin
	for i in 1..lst.N loop
		if lst.lst(i).key = key then return lst.lst(i).store; end if;
	end loop;
	raise matchError;
end;

function getStorage(lst:List; i:Positive) return EntryRec is
begin
	if i>lst.N then raise Constraint_Error; end if;
	return lst.lst(i);
end;

function getNumEntries(lst:List) return Natural is
begin
	return lst.N;
end;



procedure addPair(lst: in out List; pair:EntryRec; drop:overflowAction := Min) is
	-- sorts, checks overflow and inserts
	--intended use is relatively small lists, so insertion is done via linear search
	i : Positive range 1 .. lst.N+1 := 1;
begin
	--need special treatment ehrn N=0 to start array
	if lst.N=0 then
		lst.N := 1;
		lst.lst(1) := pair;
		return;
	end if;
	while i <= lst.N and then lst.lst(i).key < pair.key loop i:=i+1; end loop;
	-- now i=1 or lst>n+1 if new min or max respectively
	if (i=1 and drop=Min) or (drop=Max and i=lst.Max+1) then return; end if;
	--Put(",  i=");Put(Integer(i));
	-- note, if N<Max, there is still some space to add stuff..
	if lst.N < lst.Max then --always slide right
		lst.lst(i+1..lst.N+1) := lst.lst(i..lst.N);
		lst.lst(i) := pair;
		lst.N := lst.N + 1;
		return;
	end if;
	if drop = Min then
		--slide left, rely on compiler ti figure out proper order of copy
		lst.lst(1..i-2) := lst.lst(2..i-1);
		lst.lst(i-1) := pair;
	else
		lst.lst(i+1..lst.Max) := lst.lst(i..lst.Max-1);
		lst.lst(i) := pair;
	end if;
end addPair;

end sorted_Pairs;