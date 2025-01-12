
setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")

dat0=read.csv("240514_Data_RFA.csv")
dat01=dat0[!duplicated(dat0$subjectkey),]
dat=cbind.data.frame(subjectkey=dat01$subjectkey, ParticipantNumber=dat01$ParticipantNumber)

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/CSV_files")

demo0=read.csv("demo01.csv",head=T)
Clin0=read.csv("panss01.csv", head=T)
Cog0=read.csv("neurobatt01.csv", head=T)

dat1=merge(dat, demo0, by="subjectkey",all.x=T)
dat2=merge(dat1, Clin0, by="subjectkey",all.x=T)
dat3=merge(dat2, Cog0, by="subjectkey",all.x=T)

## Demographics

table(dat3$sex)
round(prop.table(table(dat3$sex))*100,1)
table(is.na(dat3$sex))
365/1460*100
1059/1460*100


mean(as.numeric(as.character(dat3$interview_age))/12, na.rm=T)
sd(as.numeric(as.character(dat3$interview_age))/12, na.rm=T)
table(is.na(dat3$interview_age))

dat3$race[dat3$race=="Unknown or not reported"]=NA
dat3$race[dat3$race==""]=NA

table(dat3$race)
round(prop.table(table(dat3$race))*100,1)
table(is.na(dat3$race))
873/1460*100
512/1460*100
33/1460*100
8/1460*100
4/1460*100
26/1460*100

table(dat3$das1ms)
round(prop.table(table(dat3$das1ms))*100,1)
table(is.na(dat3$das1ms))
868/1460*100
301/1460*100
166/1460*100
89/1460*100
34/1460*100


mean(as.numeric(dat3$educ_yrs), na.rm=T)
sd(as.numeric(dat3$educ_yrs), na.rm=T)
table(is.na(as.numeric(dat3$educ_yrs)))

dat3$employ[dat3$employ=="0"]=NA

table(dat3$employ)
round(prop.table(table(dat3$employ))*100,1)
table(is.na(dat3$employ))
1216/1460*100
120/1460*100
97/1460*100

## Cognitive and clinical assessment scores

mean(dat3$panss_positive,na.rm=T)
median(dat3$panss_positive,na.rm=T)
sd(dat3$panss_positive,na.rm=T)
min(dat3$panss_positive,na.rm=T)
max(dat3$panss_positive,na.rm=T)
table(is.na(dat3$panss_positive))

mean(dat3$panss_negative,na.rm=T)
median(dat3$panss_negative,na.rm=T)
sd(dat3$panss_negative,na.rm=T)
min(dat3$panss_negative,na.rm=T)
max(dat3$panss_negative,na.rm=T)
table(is.na(dat3$panss_negative))

mean(dat3$panss_general,na.rm=T)
median(dat3$panss_general,na.rm=T)
sd(dat3$panss_general,na.rm=T)
min(dat3$panss_general,na.rm=T)
max(dat3$panss_general,na.rm=T)
table(is.na(dat3$panss_general))

## Other variables

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")

dat4=read.csv("240516_Before_Imputation.csv", header=T)
part1=dat3$subjectkey[dat3$subjectkey%in%dat4$subjectkey]
dat5=dat4[dat4$subjectkey%in%part1,]

### CDSS

dat6=dat5[!is.na(dat5$clgry),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$clgry,na.rm=T)
median(dat7$clgry,na.rm=T)
sd(dat7$clgry,na.rm=T)
min(dat7$clgry,na.rm=T)
max(dat7$clgry,na.rm=T)
table(is.na(dat7$clgry))

## dai

dat6=dat5[!is.na(dat5$dai),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$dai,na.rm=T)
median(dat7$dai,na.rm=T)
sd(dat7$dai,na.rm=T)
min(dat7$dai,na.rm=T)
max(dat7$dai,na.rm=T)
table(is.na(dat7$dai))

##itaq

dat6=dat5[!is.na(dat5$itaq),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$itaq)
median(dat7$itaq,na.rm=T)
sd(dat7$itaq,na.rm=T)
min(dat7$itaq,na.rm=T)
max(dat7$itaq,na.rm=T)
table(is.na(dat7$itaq))

## CGI drug use

dat6=dat5[!is.na(dat5$cs14),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$cs14)
median(dat7$cs14,na.rm=T)
sd(dat7$cs14,na.rm=T)
min(dat7$cs14,na.rm=T)
max(dat7$cs14,na.rm=T)
table(is.na(dat7$cs14))

## CGI alcohol

dat6=dat5[!is.na(dat5$cs15),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$cs15)
median(dat7$cs15,na.rm=T)
sd(dat7$cs15,na.rm=T)
min(dat7$cs15,na.rm=T)
max(dat7$cs15,na.rm=T)
table(is.na(dat7$cs15))

## CGI severity

dat6=dat5[!is.na(dat5$cs16),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$cs16)
median(dat7$cs16,na.rm=T)
sd(dat7$cs16,na.rm=T)
min(dat7$cs16,na.rm=T)
max(dat7$cs16,na.rm=T)
table(is.na(dat7$cs16))

## QOL

dat6=dat5[!is.na(dat5$qol),]
dat6=dat6[order(dat6$interview_age),]
dat7=dat6[!duplicated(dat6$subjectkey),]
mean(dat7$qol)
median(dat7$qol,na.rm=T)
sd(dat7$qol,na.rm=T)
min(dat7$qol,na.rm=T)
max(dat7$qol,na.rm=T)
table(is.na(dat7$qol))

## Cognition

##HVLT

mean(dat3$neur7a, na.rm=T)
median(dat3$neur7a,na.rm=T)
sd(dat3$neur7a,na.rm=T)
min(dat3$neur7a,na.rm=T)
max(dat3$neur7a,na.rm=T)
table(is.na(dat3$neur7a))

mean(dat3$neur7b, na.rm=T)
median(dat3$neur7b,na.rm=T)
sd(dat3$neur7b,na.rm=T)
min(dat3$neur7b,na.rm=T)
max(dat3$neur7b,na.rm=T)
table(is.na(dat3$neur7b))

mean(dat3$neur7c, na.rm=T)
median(dat3$neur7c,na.rm=T)
sd(dat3$neur7c,na.rm=T)
min(dat3$neur7c,na.rm=T)
max(dat3$neur7c,na.rm=T)
table(is.na(dat3$neur7c))

##CPT: d-Prime

mean(dat3$neur11a, na.rm=T)
median(dat3$neur11a,na.rm=T)
sd(dat3$neur11a,na.rm=T)
min(dat3$neur11a,na.rm=T)
max(dat3$neur11a,na.rm=T)
table(is.na(dat3$neur11a))

mean(dat3$neur11b, na.rm=T)
median(dat3$neur11b,na.rm=T)
sd(dat3$neur11b,na.rm=T)
min(dat3$neur11b,na.rm=T)
max(dat3$neur11b,na.rm=T)
table(is.na(dat3$neur11b))

mean(dat3$neur11c, na.rm=T)
median(dat3$neur11c,na.rm=T)
sd(dat3$neur11c,na.rm=T)
min(dat3$neur11c,na.rm=T)
max(dat3$neur11c,na.rm=T)
table(is.na(dat3$neur11c))

### COWAT FAS

mean(dat3$neur3a, na.rm=T)
median(dat3$neur3a,na.rm=T)
sd(dat3$neur3a,na.rm=T)
min(dat3$neur3a,na.rm=T)
max(dat3$neur3a,na.rm=T)
table(is.na(dat3$neur3a))

mean(dat3$neur3b, na.rm=T)
median(dat3$neur3b,na.rm=T)
sd(dat3$neur3b,na.rm=T)
min(dat3$neur3b,na.rm=T)
max(dat3$neur3b,na.rm=T)
table(is.na(dat3$neur3b))

mean(dat3$neur3c, na.rm=T)
median(dat3$neur3c,na.rm=T)
sd(dat3$neur3c,na.rm=T)
min(dat3$neur3c,na.rm=T)
max(dat3$neur3c,na.rm=T)
table(is.na(dat3$neur3c))

## COWAT Category

mean(dat3$neur4a, na.rm=T)
median(dat3$neur4a,na.rm=T)
sd(dat3$neur4a,na.rm=T)
min(dat3$neur4a,na.rm=T)
max(dat3$neur4a,na.rm=T)
table(is.na(dat3$neur4a))

mean(dat3$neur4b, na.rm=T)
median(dat3$neur4b,na.rm=T)
sd(dat3$neur4b,na.rm=T)
min(dat3$neur4b,na.rm=T)
max(dat3$neur4b,na.rm=T)
table(is.na(dat3$neur4b))

mean(dat3$neur4c, na.rm=T)
median(dat3$neur4c,na.rm=T)
sd(dat3$neur4c,na.rm=T)
min(dat3$neur4c,na.rm=T)
max(dat3$neur4c,na.rm=T)
table(is.na(dat3$neur4c))

## WAIS-R Digit Symbol

mean(dat3$neur9, na.rm=T)
median(dat3$neur9,na.rm=T)
sd(dat3$neur9,na.rm=T)
min(dat3$neur9,na.rm=T)
max(dat3$neur9,na.rm=T)
table(is.na(dat3$neur9))

##Grooved Pegboard

mean(dat3$neur10a, na.rm=T)
median(dat3$neur10a,na.rm=T)
sd(dat3$neur10a,na.rm=T)
min(dat3$neur10a,na.rm=T)
max(dat3$neur10a,na.rm=T)
table(is.na(dat3$neur10a))

mean(dat3$neur10b, na.rm=T)
median(dat3$neur10b,na.rm=T)
sd(dat3$neur10b,na.rm=T)
min(dat3$neur10b,na.rm=T)
max(dat3$neur10b,na.rm=T)
table(is.na(dat3$neur10b))

##Wisconsin Card sorting task

mean(dat3$neur13a, na.rm=T)
median(dat3$neur13a,na.rm=T)
sd(dat3$neur13a,na.rm=T)
min(dat3$neur13a,na.rm=T)
max(dat3$neur13a,na.rm=T)
table(is.na(dat3$neur13a))

mean(dat3$neur13b, na.rm=T)
median(dat3$neur13b,na.rm=T)
sd(dat3$neur13b,na.rm=T)
min(dat3$neur13b,na.rm=T)
max(dat3$neur13b,na.rm=T)
table(is.na(dat3$neur13b))

mean(dat3$neur13c, na.rm=T)
median(dat3$neur13c,na.rm=T)
sd(dat3$neur13c,na.rm=T)
min(dat3$neur13c,na.rm=T)
max(dat3$neur13c,na.rm=T)
table(is.na(dat3$neur13c))

##WISC-R mazes

mean(dat3$neur5, na.rm=T)
median(dat3$neur5,na.rm=T)
sd(dat3$neur5,na.rm=T)
min(dat3$neur5,na.rm=T)
max(dat3$neur5,na.rm=T)
table(is.na(dat3$neur5))

## Leter Number sequencing

mean(dat3$neur6, na.rm=T)
median(dat3$neur6,na.rm=T)
sd(dat3$neur6,na.rm=T)
min(dat3$neur6,na.rm=T)
max(dat3$neur6,na.rm=T)
table(is.na(dat3$neur6))

##Computerized test of visuaospatial working memory

mean(dat3$neur12a, na.rm=T)
median(dat3$neur12a,na.rm=T)
sd(dat3$neur12a,na.rm=T)
min(dat3$neur12a,na.rm=T)
max(dat3$neur12a,na.rm=T)
table(is.na(dat3$neur12a))

mean(dat3$neur12b, na.rm=T)
median(dat3$neur12b,na.rm=T)
sd(dat3$neur12b,na.rm=T)
min(dat3$neur12b,na.rm=T)
max(dat3$neur12b,na.rm=T)
table(is.na(dat3$neur12b))

mean(dat3$neur12c, na.rm=T)
median(dat3$neur12c,na.rm=T)
sd(dat3$neur12c,na.rm=T)
min(dat3$neur12c,na.rm=T)
max(dat3$neur12c,na.rm=T)
table(is.na(dat3$neur12c))


