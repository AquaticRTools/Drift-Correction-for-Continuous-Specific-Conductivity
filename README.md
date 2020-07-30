# Drift-Correction-for-Continuous-Specific-Conductivity
This code determines correction periods and linearly drift corrects specific conductivity values from a data set with field visits and continuous logger data. It also assigns accuracy ratings according to USGS protocols based on percent difference between logger and field meter values.

We applied this code to data collected with hobo conductivity loggers that are only factory calibrated, so we compared logger values to same-day calibrated multi-parameter sonde stream values.  We did not perform pre- and post- cleaning checks and assumed all error was drift.  The input dataset is already corrected for daylight savings, and bad data (out of water, bad battery, etc) has already been deleted.  The original dataset had over 1 million records; the dataset here is half of that copied to a new file.

The code has several for loops.  To follow what they are doing, set the for loop variable (usually i) to a number and run through the code.

Linear Drift Correction Applied:
CorrectedValue(t)=rawvalue(t)+AmountDriftTs+
   (t-ts)*((AmountDrifte-AmountDrifts)/(TimeDrifte-TimeDrifts))

- t=time of logger data point
- ts=time at start of correction
- Amount Drifte= Amount of Drift at end of correction
- Amount Drifts= Amount of Drift at start of correction
- TimeDrifte= Time at end of correction
- TimeDrifts= Time at start of correction
