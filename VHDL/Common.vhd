-- Common declarations package
-- Andrew Read
-- Created 21 August 2019
-- Ref: https://stackoverflow.com/questions/16872950/can-custom-types-be-used-in-port-declaration
-- IS_GLOBAL_INCLUDE must be checked for Vivado to recognize this package

package Common is
	type ESP_control_type is (plus_one, minus_one, zero, from_ts, no_change);			-- ts = tread store
	type SSP_control_type is (plus_one, minus_one, zero, from_ts, from_es, no_change);	-- es = exception stack
end Common;