#################
# NICU group care analysis
# Citation: Goldstein ND, Ellefson AM, Paul DA. Evaluating Individual Provider Performance under a Team Care Paradigm in the Neonatal Intensive Care Unit. Manuscript in preparation.
# 2/19/18 -- Neal Goldstein
#################


### FUNCTIONS ###

library(gmodels) #CrossTable
library(psych) #describe, describeBy
library(survival) #survival analysis


### READ DATA ###

load("NICU.2018-02-19.RData")


### SUBSET and CREATE COHORT ###

#no birthweight means not admitted to NICU
NICU = NICU[!is.na(NICU$Birthweight), ]

#remove no discharge date (data entry incomplete)
NICU = NICU[!is.na(NICU$Date_discharge_initial), ]

#limit data to Jan 2017 to Jun 2017
NICU = NICU[NICU$Date_admission>="2017-01-01" & NICU$Date_admission<="2017-06-30", ]

#limit to single admissions only
NICU = NICU[NICU$Admission_n==1, ]


### JOIN ATTENDING DATA ###

ID_oldlist = read.csv("NICU.2018-02-19.Identifiers.csv", stringsAsFactors=F)

oldIDs = NICU$ID
mrnlist = NA

for (i in 1:length(oldIDs))
{
  #get MRN
  if (!is.na(suppressWarnings(as.numeric(ID_oldlist$MRN[which(ID_oldlist$ID==oldIDs[i])]))))
  {
    mrnlist = c(mrnlist, as.numeric(ID_oldlist$MRN[which(ID_oldlist$ID==oldIDs[i])]))
  } else {
    mrnlist = c(mrnlist, ID_oldlist$MRN[which(ID_oldlist$ID==oldIDs[i])])
  }
}
rm(i)
mrnlist = mrnlist[-1]

#copy MRNs to clipboard
writeClipboard(paste(mrnlist[1:100], collapse=";"))
writeClipboard(paste(mrnlist[101:200], collapse=";"))
writeClipboard(paste(mrnlist[201:300], collapse=";"))
writeClipboard(paste(mrnlist[301:400], collapse=";"))
writeClipboard(paste(mrnlist[401:500], collapse=";"))
writeClipboard(paste(mrnlist[501:515], collapse=";"))

DPN = read.csv("Daily_progress_note_Jan_Jun2017.csv", as.is=T)

#get ID
DPN$ID = NA
for (i in 1:nrow(DPN))
{
  DPN$ID[i] = ID_oldlist$ID[ID_oldlist$MRN==DPN$MRN[i]]
}
rm(i)

#clean up
rm(ID_oldlist,mrnlist,oldIDs)
DPN$MRN = NULL

#save attending dataset
save.image(file="groupcare.RData")


### LOAD DATA FOR ANALYSIS AND SUBSET ###

load("groupcare.RData")

#for antibiotic d/c 48hrs
#NICU = NICU[!is.na(NICU$Antibiotic_start) & NICU$Antibiotic_start==1, ]


### CREATE DAY by DAY DATASETS ###

attending_list = c("XXXXX") #place names of providers here

#recode date
DPN$Date = as.Date(DPN$Date, format="%m/%d/%Y")

NICU_attending = data.frame("ID"=NA, "Date"=Sys.Date(), "LOS"=NA, "LOS_day"=NA, "Vent_day"=NA, "Abx_day"=NA, "Attending"=NA, stringsAsFactors=F)

for (i in 1:nrow(NICU))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")
  
  #NICU stay
  NICU_dates = seq(NICU$Date_admission[i],NICU$Date_discharge_initial[i],by="days")

  #retrieve the attending neonatologist and care indicators for each day
  for (j in 1:length(NICU_dates))
  {
    
    DPN_list = DPN[DPN$ID==NICU$ID[i] & DPN$Date==NICU_dates[j], ]

    #search for the attending(s)
    attending = unique(DPN_list$Verifying_Personnel[DPN_list$Verifying_Personnel %in% attending_list])
    
    #vent today
    if (NICU$Vent[i]==1) {
      #this technically should be DOB, but assuming admission is on first day of life
      vent = ifelse(NICU_dates[j]>=(NICU$Date_admission[i] + NICU$Vent_start[i] - 1) & NICU_dates[j]<=(NICU$Date_admission[i] + NICU$Vent_start[i] - 1 + NICU$Vent_length[i] - 1), 1, 0)
    } else {
      vent = NA
    }
    
    #antibiotic today
    if (NICU$Antibiotic[i]==1) {
      #this technically should be DOB, but assuming admission is on first day of life
      abx = ifelse(NICU_dates[j]>=(NICU$Date_admission[i] + NICU$Antibiotic_start[i] - 1) & NICU_dates[j]<=(NICU$Date_admission[i] + NICU$Antibiotic_start[i] - 1 + NICU$Antibiotic_length[i] - 1), 1, 0)
    } else {
      abx = NA
    }

    #if there is one or more attendings, add a record for the attending(s)
    if (length(attending)>0) {
      NICU_attending = rbind(NICU_attending,data.frame("ID"=NICU$ID[i], "Date"=NICU_dates[j], "LOS"=length(NICU_dates), "LOS_day"=j, "Vent_day"=vent, "Abx_day"=abx, "Attending"=attending, stringsAsFactors=F))
    } else {
      NICU_attending = rbind(NICU_attending,data.frame("ID"=NICU$ID[i], "Date"=NICU_dates[j], "LOS"=length(NICU_dates), "LOS_day"=j, "Vent_day"=vent, "Abx_day"=abx, "Attending"=NA, stringsAsFactors=F))
    }
    
  }
}
rm(i,j,NICU_dates,DPN_list,attending,vent,abx)
NICU_attending = NICU_attending[-1,]


### CREATE SURVIVAL DATASETS ###

#initialize a container for datasets
groupcare = list(NA)

#create provider specific datasets
for (i in 1:length(attending_list))
{
  cat("\n\n************** ","Observation: ",i," **************\n",sep="")

  #which attending is the focus of this analysis?
  provider = attending_list[i]

  NICU_surv = data.frame("ID"=NICU$ID, "LOS"=NICU$LOS, "Vent_length"=NICU$Vent_length, "Antibiotic_length"=NICU$Antibiotic_length, "Central_line_length"=NICU$Central_line_length, "Gestational_age"=NICU$Gestational_age, stringsAsFactors=F)
  #NICU_surv$Attending_any = NA
  #NICU_surv$Attending_start = NA
  #NICU_surv$Attending_48h = NA
  NICU_surv$Attending_prop = NA
  NICU_surv$Attending_weight = NA
  for (j in 1:nrow(NICU_surv))
  {
    #get list of attendings for the given patient
    provider_list = NICU_attending$Attending[NICU_attending$ID==NICU_surv$ID[j]]
    in_list = grep(provider, provider_list)
    
    # #attending for any portion of stay
    # NICU_surv$Attending_any[j] = ifelse(length(in_list)>0, 1, 0)
    # 
    # #admitting attending
    # NICU_surv$Attending_start[j] = ifelse(length(in_list)>0 & (1 %in% in_list), 1, 0)
    # 
    # #attending @48 hrs (for abx d/c metric)
    # NICU_surv$Attending_48h[j] = ifelse(length(in_list)>0 & ((2 %in% in_list) | (3 %in% in_list)), 1, 0)
    
    #attending for greatest proportion of stay
    NICU_surv$Attending_prop[j] = ifelse(length(in_list)>0 & (provider %in% names(which.max(table(provider_list)))), 1, 0)
    
    #create a weight corresponding to proportion of stay
    NICU_surv$Attending_weight[j] = ifelse(length(in_list)>0, prop.table(table(provider_list))[provider], 0)
    
  }

  #create an exposure category based on amounnt of care
  NICU_surv$Attending_care = ifelse(NICU_surv$Attending_prop==0 & NICU_surv$Attending_weight>0, 1, ifelse(NICU_surv$Attending_prop==0, 0, 2))
  
  #join into list
  tmp = list(NICU_surv)
  names(tmp) = provider
  groupcare = c(groupcare,tmp)

}
rm(i,j,in_list,tmp,NICU_surv,provider,provider_list)
groupcare[[1]] = NULL


### ANALYSIS ###

describe(NICU$Gestational_age)
describe(NICU$Birthweight)
sum(NICU$LOS)
describe(NICU$LOS); IQR(NICU$LOS)

CrossTable(NICU$Antibiotic)
CrossTable(NICU$Vent)
CrossTable(NICU$Central_line)

describe(NICU$Antibiotic_length)
describe(NICU$Vent_length)
describe(NICU$Central_line_length)

table(NICU_attending$Attending)
round(prop.table(table(NICU_attending$Attending))*100,0)

#aggregate(NICU_attending$Attending[NICU_attending$LOS==16], by=list(Infant=NICU_attending$ID[NICU_attending$LOS==16]), FUN=length)
IDs = unique(NICU_attending$ID[NICU_attending$LOS==16])
unique_providers = NA
for (i in 1:length(IDs)) {
  unique_providers = c(unique_providers, length(unique(NICU_attending$Attending[NICU_attending$ID==IDs[i]])))
}
mean(unique_providers,na.rm=T)
rm(i,IDs,unique_providers)

counts = table(NICU_attending$Attending[NICU_attending$LOS_day<=4], NICU_attending$LOS_day[NICU_attending$LOS_day<=4])
props = cbind(prop.table(counts[,1]), prop.table(counts[,2]), prop.table(counts[,3]), prop.table(counts[,4]))

barplot(props)

prop.table(table(NICU_attending$Attending[NICU_attending$LOS_day==1]))
prop.table(table(NICU_attending$Attending[NICU_attending$LOS_day==2]))
prop.table(table(NICU_attending$Attending[NICU_attending$LOS_day==3]))

boxplot(NICU_surv$LOS ~ NICU_surv$Attending_any)


### SURVIVAL ANALYSIS ###

# survobj = with(NICU_surv, Surv(Antibiotic_length))
# fit0 = survfit(survobj~1, data=NICU_surv)
# fit1 = survfit(survobj~Attending_any,data=NICU_surv)
# survdiff(survobj~Attending_any, data=NICU_surv) 
# plot(fit1)
# 
# #plot(fit1, xlab="LOS in Days", ylab="% Discharged", yscale=100, col=c("red","blue"), main="Differences in LOS by Provider") 
# 
# #adjusted Cox: https://cran.r-project.org/web/packages/survival/vignettes/adjcurve.pdf
# 
# #unweighted
# model = coxph(Surv(LOS)~strata(Attending_prop)+Gestational_age,data=NICU_surv)
# plot(survfit(model), xlab="LOS, days", yscale=100, col=c("red","blue"), lwd=2, main="Neonatology Length of Stay", sub="Jan 2017 to Jun 2017")
# legend("topright", title="Provider", c("Other attendings", provider), fill=c("red", "blue"), cex=1)
# 
# model = coxph(Surv(Vent_length)~strata(Attending_prop)+Gestational_age,data=NICU_surv)
# plot(survfit(model), xlab="Ventilation length, days", yscale=100, col=c("red","blue"), lwd=2, main="Neonatology Ventilator Use", sub="Jan 2017 to Jun 2017")
# legend("topright", title="Provider", c("Other attendings", provider), fill=c("red", "blue"), cex=1)
# 
# model = coxph(Surv(Antibiotic_length)~strata(Attending_prop)+Gestational_age,data=NICU_surv)
# plot(survfit(model), xlab="Antibiotic duration, days", ylab="Proportion on Antibiotics", yscale=100, col=c("red","blue"), lwd=2, main="Neonatology Antibiotic Use", sub="Jan 2017 to Jun 2017")
# legend("topright", title="Provider", c("Other attendings", provider), fill=c("red", "blue"), cex=1)
# 
# #plot(survfit(model, newdata=data.frame(Attending_prop=c(0,1), Gestational_age=c(mean(NICU_surv$Gestational_age[NICU_surv$Attending_prop==0],na.rm=T),mean(NICU_surv$Gestational_age[NICU_surv$Attending_prop==1],na.rm=T)))))
# 
# #weighted
# model = coxph(Surv(LOS)~strata(Attending_any)+Gestational_age,weights=Attending_weight,data=NICU_surv)
# #model = coxph(Surv(LOS)~strata(Attending_any)+Gestational_age,weights=Attending_weight,data=subset(NICU_surv,LOS<=7))
# plot(survfit(model), xlab="LOS, days", yscale=100, col=c("red","blue"), lwd=2, main="Neonatology Length of Stay", sub="Jan 2017 to Jun 2017")
# legend("topright", title="Provider", c("Other attendings", provider), fill=c("red", "blue"), cex=1)
# 
# #note that stratified cox models (assumes different baseline hazard) won't provide a global test, cannot estimate on stratified variables
# cox.zph(model)

#Length on ventilator
for (i in 1:length(groupcare))
{
  model = coxph(Surv(Vent_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[i]])
  plot(survfit(model), xlab="Ventilator days", yscale=100, col=c("black","blue","red"), lwd=2, main="Neonatology Group Care Comparison", sub="Jan 2017 to Jun 2017")
  legend("topright", title=names(groupcare)[i], legend=c("No care","Some care","Majority care"), fill=c("black","blue","red"), cex=1)
}
rm(i)

#Length on antibiotics
for (i in 1:length(groupcare))
{
  model = coxph(Surv(Antibiotic_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[i]])
  plot(survfit(model), xlab="Antibiotic days", yscale=100, col=c("black","blue","red"), lwd=2, main="Neonatology Group Care Comparison", sub="Jan 2017 to Jun 2017")
  legend("topright", title=names(groupcare)[i], legend=c("No care","Some care","Majority care"), fill=c("black","blue","red"), cex=1)
}
rm(i)

#Length with central line
for (i in 1:length(groupcare))
{
  model = coxph(Surv(Central_line_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[i]])
  plot(survfit(model), xlab="Central line days", yscale=100, col=c("black","blue","red"), lwd=2, main="Neonatology Group Care Comparison", sub="Jan 2017 to Jun 2017")
  legend("topright", title=names(groupcare)[i], legend=c("No care","Some care","Majority care"), fill=c("black","blue","red"), cex=1)
}
rm(i)

#protypical examples for paper
model = coxph(Surv(Antibiotic_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[6]])
#tiff("Figure2.tif",height=6,width=10,units='in',res=1200) 
plot(survfit(model), xlab="Antibiotic days", yscale=100, col=c("black","blue","red"), lwd=2)
legend("topright", title="Attending Neonatologist", legend=c("No care involvement","Partial care involvement","Majority care involvement"), fill=c("black","blue","red"), cex=0.8)
#dev.off() 
summary(coxph(Surv(Antibiotic_length)~as.factor(Attending_care)+Gestational_age+LOS,data=groupcare[[6]]))

model = coxph(Surv(Vent_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[4]])
#tiff("Figure3.tif",height=6,width=10,units='in',res=1200) 
plot(survfit(model), xlab="Ventilator days", yscale=100, col=c("black","blue","red"), lwd=2)
legend("topright", title="Attending Neonatologist", legend=c("No care involvement","Partial care involvement","Majority care involvement"), fill=c("black","blue","red"), cex=0.8)
#dev.off() 
summary(coxph(Surv(Vent_length)~as.factor(Attending_care)+Gestational_age+LOS,data=groupcare[[4]]))

model = coxph(Surv(Central_line_length)~strata(Attending_care)+Gestational_age+LOS,data=groupcare[[11]])
#tiff("Figure4.tif",height=6,width=10,units='in',res=1200) 
plot(survfit(model), xlab="Central line days", yscale=100, col=c("black","blue","red"), lwd=2)
legend("topright", title="Attending Neonatologist", legend=c("No care involvement","Partial care involvement","Majority care involvement"), fill=c("black","blue","red"), cex=0.8)
#dev.off() 
summary(coxph(Surv(Central_line_length)~as.factor(Attending_care)+Gestational_age+LOS,data=groupcare[[11]]))
