**************************************************************
  WRmetDFO - Meteorological Data Preprocessor for BlastDFO
**************************************************************

This tool preprocesses WR meteorological data to provide improved run stability for BlastDFO

	Balloon met data are adjusted and decimated as appropriate with the tower and DASS data left unchanged.
	Processing of records is as follows:
		Saves data at 1.05 mb pressure change for the first 10 mb of pressure drop.
		Saves pressure level at delp millibar pressure intervals and inserts in saved records 
			if no temperature, wind speed or wind direction slope changes are recorded within delp.
			delp = 1.05 up to 600 ft, then 2.05 up to 1000 ft, then 14 - int(press/100) higher up.
		Averages new and previous levels and replaces the previous saved level
			if the new and previous levels are within 1.5*(new pressure/1000.0) mb.

To run this tool place the executable in the same folder as the met data to be processed.
Double click on the executable or open a command prompt in the data folder and type the name of the executable.
Follow the commands:
 'Enter name of input met data file to be processed:'
 'Enter the output BlastDFO met file name:'

The default output file name is the same as the input file name with '_dfo' appended prior to the extension.

Upon completion, statistics of the met data records encountered and processed are printed.

