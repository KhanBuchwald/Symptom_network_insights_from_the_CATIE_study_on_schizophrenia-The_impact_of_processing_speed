## 1. Remove empty columns

library(missForest)
library(MASS)

## Read data dictionary
setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Auckland University of Technology/Study 3 Static Study/Data Files")
dd=read.csv("230704_Data_Dictionary.csv", header=T)

## List Catie Study txt files

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Orignal Data")
files=list.files()

## Selects filenames from datasets included in Data dictionary

files2=files[files%in%c("clgry01.txt","itaq01.txt","panss01.txt","neurobatt01.txt","qol01.txt","dai01.txt","cgis01.txt", "demo01.txt")]

## Remove .txt from filenames of datasets

files3=gsub(".txt", "", files2)

## Read files, order them and remove empty columns

for(i in c(1:length(files2))){
  setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Orignal Data")
  
  ## Read file
  dat0=read.delim(files2[i], header=T) 
  
  ## Remove repeat assessments
  
  dat=dat0[dat0$phase_ct=="Pre-Rand",]
  
  ##Remove duplicates
  
  dat1=dat[!duplicated(dat$subjectkey, fromLast = F),]
    
  ## Order dataset with earlier observations first
  dat2=dat1[order(dat1$subjectkey),]
  
  ## List empty columns
  empty_columns <- colSums(is.na(dat2) | dat2 == "") == nrow(dat2)
  
  ## Remove empty columns
  dat3=dat2[, !empty_columns]
  
  ## Write each dataset as CSV's without empty columns
  setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/CSV_files")
  write.csv(dat3, paste(files3[i], ".csv", sep="" ))
}

##Read Demographics

  setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Orignal Data")
  dat0=read.delim("demo01.txt", header=T) 
  
  ##Remove duplicates
  
  dat1=dat0[!duplicated(dat0$subjectkey, fromLast = F),]
  
  ## Order dataset with earlier observations first
  dat2=dat1[order(dat1$subjectkey),]
  
  ## List empty columns
  empty_columns <- colSums(is.na(dat2) | dat2 == "") == nrow(dat2)
  
  ## Remove empty columns
  dat3=dat2[, !empty_columns]
  
  ## Write each dataset as CSV's without empty columns
  setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/CSV_files")
  write.csv(dat3, "demo01.csv")


## Clear global environment
rm(list = ls())

## 2. Merge the datasets into one file and delete common variables

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/CSV_files")
files=list.files() ## list files

## Create dataset of key identifiers
part=c()
for (i in 1:length(files)){
  
  ## Read dataset
  dat=read.csv(files[i], header=T)

  ## Create vector of participant keys
  part=c(part, dat$subjectkey)
}
part=part[order(part)]
part1=part[!duplicated(part)]
part2=cbind.data.frame(subjectkey=part1, ParticipantNumber=seq(1, length(part1),1))

## Merge datasets

for (i in 1:length(files)){
  
  ## Read dataset
  
  dat=read.csv(files[i], header=T)

    ## Merge with Part  
  
  part2=merge(part2, dat, by="subjectkey", all.x=T)
  
}
  part3=part2[1:1460,]
 ## Write dataset
setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")
write.csv(part3, "240514_Merged_data.csv")


## Clear global environment
rm(list = ls())

## Prepare data dictionary

library(readxl)

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/CSV_files")

files=list.files()

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")
dat=read.csv("230707_Merged_data.csv")

n1=names(dat)[grep("\\.", names(dat))]
n2=gsub("\\..*","",n1)
n3=unique(n2)

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Auckland University of Technology/Study 3 Static Study/Data Files")

dd=as.data.frame(read_xlsx("230704_Data_Dictionary.xlsx", sheet = 1))
dd2=dd[dd$Variable%in%names(dat)|dd$Variable%in%n3,]
dd3=dd2[paste(dd2$Short.Name, ".csv", sep="")%in%files,]
write.csv(dd3, "240514_Variables_Of_Included_Assessments.csv")

## Select variables set missing to NA

## Clear global environment
rm(list = ls())
setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")
dat=as.data.frame(read.csv("240514_Merged_data.csv"))

meta=dat[,c(2,3)]
clgry=dat[,c(69)]
demo=dat[,c(101,103,109,113,117,125)]
dai=dat[,c(94)]
itaq=dat[,c(260)]
qol=dat[,c(492)]
pos=dat[,c(346:348)]
neurocog=dat[,c(333:337)] ## Using Raw Scores
cgi=dat[,c(40:42)]
dat2=cbind.data.frame(meta,demo,clgry,dai,itaq, neurocog,pos,cgi,qol)


## Change unknwon values to missing

dat2[dat2==""]=NA
dat2$race[dat2$race=="Unknown or not reported"]=NA
dat2$employ[dat2$employ==0]=NA

##Relevel variables

dat2$race[dat2$race!="White"&dat2$race!="Black or African American"]="Other"
dat2$das1ms[dat2$das1ms!="Married"]="Not Married/Widowed"
dat2$curreduccompleted[dat2$curreduccompleted=="Advanced degree completed [e.g. Ph.D., M"]="University degree"
dat2$curreduccompleted[dat2$curreduccompleted=="Advanced degree courses, not graduated ["]="University degree"
dat2$curreduccompleted[dat2$curreduccompleted=="College graduate"]="University degree"
dat2$curreduccompleted[dat2$curreduccompleted=="College graduate and some Master's level"]="University degree"
dat2$curreduccompleted[dat2$curreduccompleted=="Master's degree completed"]="University degree"
dat2$curreduccompleted[dat2$curreduccompleted=="Some college, did not graduate"]="GED/High school diploma"
dat2$curreduccompleted[dat2$curreduccompleted=="Community college or technical school de"]="GED/High school diploma"
dat2$curreduccompleted[dat2$curreduccompleted=="Did not complete high school"]="GED/High school diploma"
dat2$curreduccompleted[dat2$curreduccompleted=="GED/High school diploma"]="GED/High school diploma"

## Change class of variables

for (i in c(2,3,9:18,23)){
  dat2[,i]=as.numeric(dat2[,i])
}
for (i in c(1,4:8,20:22)){
  dat2[,i]=factor(dat2[,i], levels = rownames(table(dat2[,i])))
}

write.csv(dat2,"C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/240516_Before_Imputation.csv")


table(is.na(dat2))
dat3=dat2[rowSums(is.na(dat2))<(.5*(ncol(dat2)-2)),]

set.seed(1000)
dat4=cbind.data.frame(dat3[,c(1:2)], missForest(as.data.frame(dat3[,c(3:23)]))$ximp)

for(i in c(20:22)){
  dat4[,i]=as.numeric(dat4[,i])
}

## Change all sociodemographics to categorical Variables

dat4$age=dat4$interview_age/12
dat4$age[dat4$interview_age/12<30]="<30"
dat4$age[dat4$interview_age/12>29&dat4$interview_age/12<45]="30-45"
dat4$age[dat4$interview_age/12>44]="45+"
dat4$age=as.factor(dat4$age)
dat5=dat4[c(1:2,24,4:23)]

## Rename levels of employment

names(dat5)=c("subjectkey","ParticipantNumber","Age","Sex","Race",             
              "Marital Status","Education","Employment","CDSS","DAI",              
              "ITAQ","MATRICS Verbal","MATRICS Vigil","MATRICS Processing Speed","MATRICS Reasoning",         
              "MATRICS Memory","PANSS General","PANSS Negative","PANSS Positive","CGI Drug Use",             
              "CGI Alcohol Use","CGI Severity","QOL")

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")
write.csv(dat5, "240514_Data_RFA.csv")
