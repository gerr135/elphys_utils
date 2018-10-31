--
--
--  Ada Spec: Common types and instantiations
--
--  Description: A helper module to instantiate Interpolated_funcs
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2005-2006
--
--  Copyright: See COPYING file that comes with this distribution
--
--

with Interpolated_Funcs.Quadratic;
-- with Interpolated_Funcs.Linear;
with Column_Table_IO;
with Ada.Text_IO;

package InterpFuncs_Support is

	type Internal_Float is new Long_Float;

	type VectorIndex is new Positive;
	type ParamIndex is new Natural;
		--  so that we do not accidentally mix them

	type Vector  is array (VectorIndex range <>) of Internal_Float;
		--  array of some parameter values, - "the column"
 	type DataRow is array (ParamIndex  range <>) of Internal_Float;
 		--  values of different parameters at the same "index" - "the row"

	type DataTable is array(ParamIndex range <>, VectorIndex range <>) of Internal_Float;


	---------------------------------
	--  The packages

	package Interpolation is new Interpolated_Funcs(Internal_Float, VectorIndex, Vector);
-- 	package NavPeaks_Interpolation_Linear is new NavPeaks_Interpolation.Linear;
	package Interpolation_Quadratic is new Interpolation.Quadratic;


	package Table_IO is new Column_Table_IO(
		Real_Type => Internal_Float,
		ColIndex  => ParamIndex,
		RowIndex  => VectorIndex,
		DataTable => DataTable,
		Vector    => DataRow,
		MaxHeaderLength => 7);

	package Internal_Float_IO is new Ada.Text_IO.Float_IO(Internal_Float);


end InterpFuncs_Support;