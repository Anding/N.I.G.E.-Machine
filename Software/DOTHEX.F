: .hex-helper
	hex
	u.
;

: .hex
	['] .hex-helper catch throw
;