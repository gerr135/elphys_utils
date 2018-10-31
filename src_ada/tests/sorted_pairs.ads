generic
	type Key_Type is private;
	type Storage is private;
	with function "<"(left,right:Key_Type) return Boolean;
package sorted_Pairs is
-- list of (key, sotrage) pairs, sorted by key

matchError : exception; -- raised by getStorage if supplied key is not found

type EntryRec is record
	key:Key_Type;
	store:Storage;
end record;

type List(Max:Positive:=10) is limited private;

function minKey(lst:List) return Key_Type;
function maxKey(lst:List) return Key_Type;

function getNumEntries(lst:List) return Natural;
	-- returns lst.N
function getStorage(lst:List; key:Key_Type) return Storage;
	-- this one has an associated exception
function getStorage(lst:List; i:Positive) return EntryRec;
	-- standard return at index. Internally checks range

type OverflowAction is (Min, Max);

procedure addPair(lst: in out List; pair:EntryRec; drop:overflowAction := Min);
-- if list is full, will drop either min or max element, according to drop passed
-- no special situation ever arises, so no exceptions in this module
--
-- also, if added value is outside the current min/max on the srop side,
-- new value is dropped, so no prior check is necessary outside addPair

private
	type ListArray is array (Integer range <>) of EntryRec;
	-- sorted in ascending order: List(1)<List(2)
	-- gets filled from the beginning: 1..N

	type List(Max : Positive := 10) is record
		lst: ListArray(1..Max);
		N:Natural:=0;
	end record;

end sorted_Pairs;