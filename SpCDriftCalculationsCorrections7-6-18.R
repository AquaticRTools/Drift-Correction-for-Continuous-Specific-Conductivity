---
  title: "Specific Conductivity Drift Calculations and Corrections"
author: "Leah Ettema"
date: "July 6th, 2018"
output: pdf_document
---
####READ ME#####
###Steps to process conductivity data from hobo loggers (See SOP)
  
  ###1. Delete bad logger data in Aquarius, clock drift correct
  #2. Import corrected field visit (sonde) data into Aquarius
  #3. Export continuous temp, continuous raw conductance, 
  # and FV data from Aquarius and run through ReformatAQData&Export.R. 
  #That .R file reformats Aquarius exported data to long form. In this code, from that file, we import
  #DateTime, Site, Type (Field vs Continuous), and SpC value.  

#This correction code assumes bad logger data (dry or dead battery) has been deleted.  
#Otherwise, a drift correction could be applied based on the bad data.  For example, 
#if the logger recorded 10 and the field meter value was 350, a drift of 340 would be 
#applied to all logger data before the reading, which is likely incorrect.

#Once we can access Aquarius remotely, we could take excel files out of the picture

---

library("stringr")
library("tidyverse")
library("reshape2")
library("lubridate")
library("broom")
library("readxl")
library(writexl)

#1. Import continuous data and reformat data ####
#This spreadsheet has all continuous and field visit data.  
#The continuous hobo valus are compared and corrected to calibrated field meter field visit values.
setwd("Z:/Desktop")
SpC<-read.csv("SpCQA5-21-18_Github.csv",stringsAsFactors=FALSE, na.string = "") #from ReformatAQData&Export.R, line262 ish

#correct wierd .csv column
if(names(SpC)[1]=="X")
{
  SpC<-SpC[,-1]
}

#create data frame of field visit values
SCFV<- SpC %>% 
  filter(Type=="FSpC")
SCFV$DateTime<-as.POSIXct(SCFV$DateTime, tz="EST") 

#create 'old' dataframe to correct
SCold<-SpC 
SCold$DateTime<-as.POSIXct(SCold$DateTime, tz="EST")

#Order by datetime then site (critical for code below)
SCold<-SCold[order(SCold$DateTime),]
SCold<-SCold[order(SCold$Site),]

#Check on which sites are in dataset
sites<-unique(SCold$Site)

#index vector of field visit locations in SCold (all other values are continuous)
FVs<-which(SCold$Type=="FSpC") 

### 2. Calculate Drift Amount ####
#For this project, hourly specific conductivity values (eg. continuous raw conductance and temperature) 
#were collected at 40 locations from 3 months to 5 years using Hoboware loggers.  Field visits were conducted 
#every 3 to 12 months to clean the loggers and download data. According to USGS, calibration drift error 
#is the result of electronic drift in the sensor reading from the last time the sensor was calibrated 
#and is determined by the difference between cleaned-sensor readings in standard solutions or buffers 
#and the true, temperature-compensated value of the standard solution or buffer. Specific conductivity 
#calibration drift is assumed to  occur at a constant, linear rate during the service interval. (cite USGS)

#This continuous monitoring project did not calibrate meters in standard solutions.  We used same-day calibrated multi-parameter 
#sondes to measure specific conductivity in stream.  

#The continuous logger values used to calcluated drift were recorded on the hour 
#before and after the field visit.  This code chunk determines the difference between the multi-parameter sonde data (field visit data) 
#to the closest logger (+/- 1 hour) values. The difference between the logger value recorded on the hour before the field meter 
#measurement will encompass both drift error and fouling error.  If there is a logger value recorded both before and after the field meter 
#measurement and cleaning, the amount of fouling versus drift error can be distinguished.  However, there is often not a post-cleaning logger value, 
#so differentiating between the two types of error is not possible for this project.  So, this code corrects for drift by treating differences 
#in logger and field meter as soley drift error, even though in some situations that error will also encompass fouling.    

#Citations:
  #1. USGS continuous monitors guidelines
#2. USGS Specific Electrical conductance 6.3

###Code chunk calculate drifts and percent drift values between logger and field meter
#Data is in order by time, then site

#Creating blank variables to fill in during for loop.  
#R is faster indexing vectors rather than dataframes
SCold$CorPeriod<-NA 
SCold$Diff<-as.numeric(NA)
SCold$Diff2<-as.numeric(NA)
SCold$TotalError<-as.numeric(NA)
SCold$pTE<-as.numeric(NA)
SCold$Mdrift<-as.numeric(NA)
SCold$pMd<-as.numeric(NA)
SCold$Fouling<-NA
SCold$TimeDiffE<-NA
SCold$TimeDiffL<-NA

#This loop extracts times for each field visit and then extracts the value in the row above and below it in SCold.  
#It then performs QA checks to ensure the values are logger(not field visits values), from the same site, and within an hour or two from it. 
#(If field meter visit was at 11:55 AM, the next logger value could be recorded at 13:00 if logger was out of water at 12:00).  
#Once QA checks are done, it extracts the rows from SCold for the field visits and logger values that need to be compared for that site.  
#Then drift calculations are performed on extracted values and placed back into SCold.  So, in SCold, all field visits will have drift 
#values and the continuous data drift values will be NA.

#Calculating SpC Drift ####
for (i in 1:nrow(SCold)) #could probably make loop short by indexing SCold$FV[i] like loops below
{
  if(SCold$Type[i]=="FSpC") #i will always be index of FV in SCold
  #Types of SCold are FspC = field visit or SCnd = logger SC no drift
    {
    #Extract CONTINUOUS values earlier and later than the field visit (i) for comparison
    #Perform QA checks on data type, time and site - need to compare correct values
    #Most of these ifelse populate values for i=1 and i=nrow(SCold), so there are not NAs 
    
    ###Type QA check prep
    if(i==1){ #if i-1 =0, we will index SCold$0 later on, which we don't want because it doesn't exist.   
      #Needs to be false.
      TypeE<-FALSE
    }
    else{
      TypeE<-SCold$Type[i-1] #extracting the type of sample (field or continuous) from early logger
    } 
    
    if(i+1>nrow(SCold)){
      TypeL<-FALSE
    }
    else{
      TypeL<-SCold$Type[i+1] #extracting the type of sample (field or continuous) from later logger
    }
    
    #i is location of field visit in SCold
    #This code creates a rows of data above (earlyT) and below (laterT) field visit.  
    #If that row can't be compared to field visit (wrong type, site, or time), fill with NAs
    
    FV<-SCold[i,] #pull out Field visit row
    ##QA on Type for values above and below field visit
    if(TypeE=="SCnd"){ #if type before field visit is continuous
      earlyT<-SCold[i-1,]
    }
    else{ #else it's a field visit and we don't want to compare FV to FV 
      earlyT<-SCold[1,] #creating a vector (value is not relevant)
      earlyT<-NA #will bind earlyT, laterT, and FV rows together later, so want NA vector to have same columns
    } 
    
    if(TypeL=="SCnd"){ #if type after field visit is continuous
      laterT<-SCold[i+1,] 
    }
    else{
      laterT<-SCold[1,] #creating a vector (value is not relevant)
      laterT<-NA
    }
    
    #Time QA check
    #time difference between logger before FV and after field visit
    timeEdiff<-ifelse(i-1<1, FALSE, 
                      as.numeric(difftime(SCold$DateTime[i], SCold$DateTime[i-1], units=c("mins")))) 
    #time difference between logger after FV and FV
    timeLdiff<-ifelse(i+1>nrow(SCold), FALSE, 
                      as.numeric(difftime(SCold$DateTime[i+1], SCold$DateTime[i], units=c("mins")))) #time difference between later logger and field visit
    
    ##Populate time differences in SCold for later compairisons and calculations
    SCold$TimeDiffE[i]<-timeEdiff
    SCold$TimeDiffL[i]<-timeLdiff
    
    if(timeEdiff>120){ #if time diffE is greater that 120 min, don't compare
      earlyT<-NA
    }
    if(timeLdiff>180){ #if time diffL is greater than 180 min, don't compare
      laterT<-NA
    }
    #Negative time differences will happen when sites change
    
    #Put all vectors in one 3-row df (same columns as SCold)
    extract<-rbind(earlyT, FV, laterT)
    NArow <- apply(extract, 1, function(x) all(is.na(x)))
    #remove rows with all NA's (Time or type doesn't match (NA's set above))
    extract<-extract[!NArow, ]
    
    #Site QA
    #extract is the dataframe of values before and after FV
    #keep rows that match site name of SCold[i] (the field visit)
    extractsite<-which(extract$Site==SCold$Site[i]) #which() returns the numeric vector location
    extract1<-extract[extractsite,] #pulls out those values
    extract1$SpCvalue<-as.numeric(extract1$SpCvalue)
    
    #create variable with number of rows in extract1, either 2 or 3
    match<-nrow(extract1)
    FVloc<-which(extract1$Type=="FSpC") #field visit location in extract
    CCloc<-which(extract1$Type=="SCnd") #continuous location in extract
    
    ###Calculate drift amount, keeping positive or negative, not absolute value
    if(match==2){ #there is only one logger and one field visit value
      #with only one continuous value, the order of values matters.  
      #Drift always needs to be FV-continuous
      SCold$Diff[i]<-as.numeric(extract1$SpCvalue[FVloc]-extract1$SpCvalue[CCloc])
      
      #could calculate percent difference add in USGS correction criteria here
    }
    
    if(match==3){ #then field visit [2] must be in middle of two continuous visits 
      ## Calculate Diff for continuous value before FV
      SCold$Diff[i]<-as.numeric(extract1$SpCvalue[2]-extract1$SpCvalue[1])
      
      ##Calculate Diff for continuous value after FV
      SCold$Diff2[i]<-as.numeric(extract1$SpCvalue[2]-extract1$SpCvalue[3])
      
    }
    if(match!=2 && match!=3) { #QA for only one row in extract, shouldn't happen
      SCold$Diff[i]<-"Error"
    }
  }
}

#creating a dataframe of field visits and the drift correction amounts
FieldVisit<-SCold[FVs,] 
FieldVisit$Diff<-as.numeric(FieldVisit$Diff)
#double check that diff is numeric
SCold$Diff<-as.numeric(SCold$Diff)

# 3. Calculate Drift Correction Variables ####
# 
# Correction algorithm:
#   ![](Z:/Monitoring Projects/ConductivityMonitors/DataAnalysis/QAdataFVandAquarius.DriftCorrectionCalculation.png)
# 
# CorrectedValue(t)=rawvalue(t)+AmountDriftTs+
#   (t-ts)*((AmountDrifte-AmountDrifts)/(TimeDrifte-TimeDrifts))
# 
# t=time of logger data point
# ts=time at start of correction
# Amount Drifte= Amount of Drift at end of correction
# Amount Drifts= Amount of Drift at start of correction
# TimeDrifte= Time at end of correction
# TimeDrifts= Time at start of correction
# 
# https://c.na32.content.force.com/servlet/servlet.FileDownload?file=00P3800000bYX13, sign into Aquarius help desk (not LAN)
# 
# Aquarius technical support states: 
#"The equation adjusts the signal by using linear interpolation of the 
#difference values, to calculate the difference value which applies at any given point. 
#It then adds that difference value to the raw value."

#Variables for Drift Correction####
#adding to dataframe
SCold$DriftTs<-NA #Amount Drift time start
SCold$DriftTe<-NA #Amount Drift time end
SCold$Ts<-SCold$DateTime[1] #Time at the start of correction, just pulling time format, value will be changed
SCold$Te<-SCold$DateTime[1] #Time at the end of correction, just pulling  format, value will be changed
SCold$CorPeriod<-NA
SCold$pDriftTs<-NA
SCold$pDriftTe<-NA

SColdFV<-which(SCold$Type=="FSpC")

for (i in 1:(length(SColdFV)-1)) #The number of correction periods will be # of field visits -1
{if((SColdFV[i+1]-SColdFV[i])!=1)#if there are not back to back field visits, continue.
{ #The index (correction period) start and end depends 
  #on if the FV is between two continuous values or if it's at 
  #the beginning or end of a dataset and next to only one
  #continuous value. We will first calculate 
  #everything as if it's not between two continous values, 
  #and adjust for continuous values later.
  
  #Correction Period (CorPeriod) starts with first continuous value after FV
  indexstart<-SColdFV[i]+1 #index of first continuous value after field visit in SCold.
  indexend<-SColdFV[i+1]-1 #last continuous value before next field visit. CorPeriod ends 
  
  #Assigning a unique number to each correction period
  SCold$CorPeriod[indexstart:indexend]<-i 
  
  #QA check on site, may not be needed, but not sure
  afterFV<-SCold$Site[SColdFV[i]]==SCold$Site[SColdFV[i+1]]
  
  #QA check on end drift value
  #using to evalute DriftTe, which occurs at corperiod end field visit (i+1)
  Diff2NA<-is.na(SCold$Diff[SColdFV[i+1]]) 
  
  #If Diff2NA is false or afterFV is false, can not perform drift corrections
  
  #Assign times  
  SCold$Ts[indexstart:indexend]<-SCold$DateTime[SColdFV[i]+1] #Time at start of correction
  SCold$Te[indexstart:indexend]<-SCold$DateTime[SColdFV[i+1]-1] #Time at end of correction
  
  #Assign drift at start of correction period (the period after field visit i)
  #Previous for loop establish a Diff and Diff2 value.  If there was only one continuous value to compare,
  #it was assigned to Diff.  If there were two, the earier one was Diff, the later was Diff2
  if(is.na(SCold$Diff2[SColdFV[i]])){ #if there is only one value to compare
    SCold$DriftTs[indexstart:indexend]<-SCold$Diff[SColdFV[i]] 
    SCold$pDriftTs[indexstart:indexend]<-round(abs(SCold$Diff[SColdFV[i]])/SCold$SpCvalue[SColdFV[i]]*100, digits=1)
  }
  else { #if field visit is between two continuous visits, 
    #saying correction starts with drift after field visit (Diff2) 
    #we will correct later if it needs to be value before field visit
    SCold$DriftTs[indexstart:indexend]<-SCold$Diff2[SColdFV[i]] 
    SCold$pDriftTs[indexstart:indexend]<-round(abs(SCold$Diff2[SColdFV[i]])/SCold$SpCvalue[SColdFV[i]]*100, digits=1)
  }
  #Assign drift at end of correction period
  if(afterFV && !Diff2NA){ #If next field visit site is not the same or Diff is NA, there will be no end drift value
    SCold$DriftTe[indexstart:indexend]<-SCold$Diff[SColdFV[i+1]] #Te will always be Diff from next field visit, pDiff2 doesn't apply
    SCold$pDriftTe[indexstart:indexend]<-round(abs(SCold$Diff[SColdFV[i+1]])/SCold$SpCvalue[SColdFV[i+1]]*100, digits=1)
    #SCold$USGScorE[indexstart:indexend]<-SCold$USGScor[SColdFV[i+1]]
  }
  else{
    SCold$DriftTe[indexstart:indexend]<-NA
    SCold$pDriftTe[indexstart:indexend]<-NA
       }
}
}

#### Preparing to adjust for FV between two continuous values
#pulling these out of SCold dataframe.  Indexing vectors is faster than 
#indexing dataframes in next for loop
timediffE<-SCold$TimeDiffE
timediffL<-SCold$TimeDiffL
SColdFVs<-SCold[SCold$Type=="FSpC",]

#creating dataframe of data that can not be drift corrected
nodrift<-SCold %>%
  filter(is.na(SCold$DriftTe)) %>%
  filter(Type=="SCnd")

#need to pull out first value of each corection period
uniquecor<-duplicated(nodrift$CorPeriod)
nodrift<-nodrift[!uniquecor,]

nodrift1<-nodrift %>%
  select(DateTime, Site, CorPeriod, Ts, Te)

## For field visits in between two logger visits, the above code will make each logger value before and after field visit 
# match the field visit value.  (So, the the slope of the SpC would be 0 for the hours bordering the field visit).  
# We want to keep the slope of the original dataset, so we need to adjust start and end times of these drift corrections.

#Fixing the slope issue
for (i in 1:(length(SColdFV)-1))
{ #below is deciding which value to correct to if the FV is between two logger values
  if(!is.na(SCold$Diff2[SColdFV[i]])) #If Diff2 is not NA, FV is between two continuous values
  { #We want the start and end of the correction period to be the continuous value closest in time to the field visit
    if(timediffE[SColdFV[i]]==timediffL[SColdFV[i]]){ #if the time is equal, we will correct to the larger SpC difference
      timediffE[SColdFV[i]]<-ifelse(SCold$Diff[SColdFV[i]]>SCold$Diff2[SColdFV[i]], timediffE[SColdFV[i]]-1, timediffE[SColdFV[i]]+1) 
      #The above line makes the ifelse statements below work if time is equal
    }
    
    if(timediffE[SColdFV[i]]<timediffL[SColdFV[i]]){ #if earlier time is smallest (closest) to FV
      #Need to change start time and start drift for all values in corection period after field visit  
      indexstart<-SColdFV[i]+1 #index of first continuous value after field visit in SCold. 
      indexend<-SColdFV[i+1]-1 #index of last continuous value before the next field visit
      
      SCold$Ts[indexstart:indexend]<-SCold$DateTime[SColdFV[i]-1] 
      SCold$DriftTs[indexstart:indexend]<-SCold$Diff[SColdFV[i]]
      SCold$pDriftTs[indexstart:indexend]<-round(abs(SCold$Diff[SColdFV[i]])/SCold$SpCvalue[SColdFV[i]]*100, digits=1)
      SCold$CorPeriod[indexstart:indexend]<-i
      } 
    if(timediffE[SColdFV[i]]>timediffL[SColdFV[i]]){ #if earlier time is larger,
      #Need to change end time and end drift for all values before field visit
      index2s<-SColdFV[i-1]+1 #index of first continuous value after previous field visit in SCold. 
      index2e<-SColdFV[i]-1 #index of last continuous value before current field visit
      
      SCold$Te[index2s:index2e]<-SCold$DateTime[SColdFV[i]+1]
      SCold$DriftTe[index2s:index2e]<-SCold$Diff2[SColdFV[i]]
      SCold$pDriftTe[index2s:index2e]<-round(abs(SCold$Diff2[SColdFV[i]])/SCold$SpCvalue[SColdFV[i]]*100, digits=1)
      SCold$CorPeriod[index2s:index2e]<-i
      }
  }
}

###Add Grade Ratings for Aquarius####
#In Aquarius
# Grades:
# 51 = Excellent
# 31 = Good
# 21 = Fair
# 11 = Poor
# -2 = Unpublishable
# 999 = Unable to drift correct

#Choosing to make Accuracy the lowest accuracy between start and end drift
SCold$Accuracy=as.numeric(ifelse(SCold$pDriftTs>SCold$pDriftTe, SCold$pDriftTs, SCold$pDriftTe))
#Assigning grade ratings
SCold$Grade=ifelse(SCold$Accuracy<=3.0, 51, 
                        ifelse(SCold$Accuracy>3&SCold$Accuracy<=10.0, 31,
                               ifelse(SCold$Accuracy>10.0&SCold$Accuracy<=15.0, 21,
                                      ifelse(SCold$Accuracy>15.0&SCold$Accuracy<30.0, 11,
                                             ifelse(SCold$Accuracy>30.0, -2, SCold$Accuracy)))))
#If no correction was performed, assign 999
Nocorrection<-which(is.na(SCold$Grade))                                         
NC<-which(SCold$Type[Nocorrection]=="SCnd")
SCold$Grade[Nocorrection[NC]]<-999

# #Assign Approval
# SCold$Approval<-ifelse(SCold$Grade==-2, -2, 3)

###4. Calculate Drift Corrected Specific Conductivity ####
#All exports are saved in bioproject "Z:/Monitoring Projects/Conductivity Monitors/Data Analysis/QAdataFVandAquarius"
SCold$TeTs<-as.numeric(difftime(SCold$Te, SCold$Ts, units="secs")) #in seconds, but need to convert from difftime format to numeric to do mathmatical operations
SCold$tTs<-as.numeric(difftime(SCold$DateTime,SCold$Ts, units="secs"))
SCold$SpCvalue<-as.numeric(SCold$SpCvalue)
SCold$DriftTs<-as.numeric(SCold$DriftTs) #don't know why these are not numerics already
SCold$DriftTe<-as.numeric(SCold$DriftTe)

#all variables are in dataframe, so just calculate new drift corrected value
SCnew<-SCold %>%
  mutate(SpCNew=SpCvalue+DriftTs+(tTs)*((DriftTe-DriftTs)/as.numeric(Te-Ts)))

#if it was not drift corrected, assign the old SpC value to SpCNew
SCnew$SpCNew<-ifelse(is.na(SCnew$SpCNew), SCnew$SpCvalue, SCnew$SpCNew)

#write.csv(SCnew, file="SCnew3-26-18.csv")


##Some QA Checks ####
#Field Visits that couldn't be compared (loggers died before FV, were out of water or lost)
SpCNA<-filter(SCnew, SCnew$Type=="FSpC")
SpCNA<-subset(SpCNA, is.na(SpCNA$Diff))

#visits where error increased in logger value after visit
test<-which(abs(SCFV$Diff2)>abs(SCFV$Diff))
test1<-SCFV[test,]
Diff2greater<- test1 %>% 
  filter(test1$Accuracy != test1$Accuracy2)

#write_xlsx(Diff2greater, "Times_Accuracy_Decreased_After_FV.xlsx", col_names = TRUE)

#write.csv(SCFV, file="SCFV.csv")

#Data Export####
### Export all sites in one file
SCnew1<- SCnew %>%
  select(DateTime, Site, SpCNew, Grade, Type) %>%
  filter(Type=="SCnd") %>% #remove field visits for export
  select(-Type)

#Export new continuous data for each site, will import into Aquarius
setwd("Z:/Desktop")

#write.csv(SCnew1, file="SCnew1All7-6-18.csv")

#They are named by Site and SpCnew, for example: FCDSpCnew.csv
for (i in 1:length(sites)){
  write.csv(subset(SCnew1, Site==sites[i]), paste(sites[i],"SpCfinal_7-06-18.csv",sep=""), row.names=F)
}


