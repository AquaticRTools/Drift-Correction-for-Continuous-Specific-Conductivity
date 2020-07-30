# Drift-Correction-for-Continuous-Specific-Conductivity
This code determines correction periods and calculates lineary drift corrected specific conductivity values from a data set with field visits and continuous logger values. 

We applied this code to data collected with hobo conductivity loggers that are only factory calibrated, so we compared logger values to same-day calibrated multi-parameter sonde stream values.  We did not perform pre- and post- cleaning checks, and assumed all error was drift.  The input dataset is already corrected for daylight savings and bad data (out of water, bad battery, etc) has already been deleted.

Drift Correction:
CorrectedValue(t)=rawvalue(t)+AmountDriftTs+
   (t-ts)*((AmountDrifte-AmountDrifts)/(TimeDrifte-TimeDrifts))

t=time of logger data point
ts=time at start of correction
Amount Drifte= Amount of Drift at end of correction
Amount Drifts= Amount of Drift at start of correction
TimeDrifte= Time at end of correction
TimeDrifts= Time at start of correction
