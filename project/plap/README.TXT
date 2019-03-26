Information from appendix 3 of the 2017 VAP report (data until June 2016)

plap.csv
	all data from appendix 3

plap-loc.csv:	Locations of PLAP fields

bbch.csv
	Primary, Secondary: BBCH numbers.

	Secondary: is used when there are two crops in the field.

biomass.csv
	Shoot, Tubers: g/m^2

fertilizer.csv
	N, P, K, S: kg/ha

manganese.cvs: Liming and manganese

irrigate.csv
	Water: mm

sow.csv
	Seed: kg/ha
	Depth: cm
	Row: cm
	Plants: m^-2
	
harvest.csv
	Stubble: cm
	Yield, Straw: hkh/ha
	YieldDM%, StrawDM%: %

products.csv:
	Volume: L/ha
	Mass: g/ha

slurry.csv
	Weight: Mg w.w./ha
	DM%: %
	N ,NH4-N, P, K: kg/ha

tillage.cvs:
	Depth: cm

genman.py: Generate plap_man.dai

plap-soil.dai: Description of PLAP fields

plap-crop.dai: Daisy crops for us in PLAP fields

plap-setup.dai: Setups for all PLAP fields.
	