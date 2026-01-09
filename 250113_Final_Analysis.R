library(bnlearn)
library(lavaan)
library(ggrepel)
library(Rgraphviz)
library(lavaanPlot)
library (stats)
library(igraph)
library(DiagrammeRsvg)
### Analysis Domains ###

## Read Dataset

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")

list.files()

dat=read.csv("240514_Data_RFA.csv")
dat1=dat[,2:ncol(dat)]

## Format Dataset

for(i in 1:8){
  dat1[,i]=as.factor(dat1[,i])
}

for(i in 9:23){
  dat1[,i]=as.numeric(dat1[,i])
}

## Execute Bayesian Networks

## Complete data

mod1=hc(dat1[,c(3:23)])

## Averaged network

set.seed(1000)
time1=Sys.time()
mod2=boot.strength(data=dat1[,c(3:23)], R=1000, algorithm = "hc")
Sys.time()-time1

setwd("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study")
save.image("240514_Afterbootstrapping.RData")

load("C:/Users/khanb/OneDrive/Personal Vault/CATIE/Static Study/240514_Afterbootstrapping.RData")

averaged.network(mod2)
mod2.1=mod2[mod2$strength>.5&mod2$direction>.5,]
mod2.2 = averaged.network(mod2.1)
mod2.3=cextend(mod2.2)

## Change BN model string to Lavaan syntax function

changestring=function(M1){
  filtered_string <- gsub("\\[[^\\|]+\\]", "", M1)
  step1 <- gsub("]\\[", "]\n[", filtered_string)
  step2 <- gsub("\\[|\\]", "", step1)
  step3 <- gsub("\\|", "~", step2)
  final_format <- gsub(":", "+", step3)
  return(final_format)
}

## Write SEM Models

mod1.sem=changestring(mod1)
mod2.sem=changestring(mod2.3)

## SEM on scaled variables

dat2=dat1
for(i in 9:23){
  dat2[,i]=scale(dat2[,i])
}

sys1=sem(model=mod1.sem, data=dat2[,c(3:23)], ordered = names(dat2)[3:8])
sys2=sem(model=mod2.sem, data=dat2[,c(3:23)], ordered = names(dat2)[3:8])

## Fit Statistics

fitmeasures(sys1)
fitmeasures(sys2)

summary(sys1, fit.measures = TRUE, rsquare = TRUE)
summary(sys2, fit.measures = TRUE, rsquare = TRUE)

BIC1=-2*logLik(mod1, dat2[,c(3:23)])+nparams(mod1,dat2[,c(3:23)])*log(nrow(dat))
BIC2=-2*logLik(mod2.3, dat2[,c(3:23)])+nparams(mod2.3,dat2[,c(3:23)])*log(nrow(dat))
AIC1=-2*logLik(mod1, dat2[,c(3:23)])+2*nparams(mod1,dat2[,c(3:23)])
AIC2=-2*logLik(mod2.3, dat2[,c(3:23)])+2*nparams(mod2.3,dat2[,c(3:23)])

## Adjusted p values

## Get coefficients

s1=summary(sys1)
s2=summary(sys2)

## create dataframe with holms p values 

p.vec1=cbind.data.frame(edge=c("chi", s1$pe$rhs[s1$pe$op=="~"],"chi", s2$pe$rhs[s2$pe$op=="~"]),edge2=c("chi", s1$pe$lhs[s1$pe$op=="~"],"chi", s2$pe$lhs[s2$pe$op=="~"]),p.orig=c(0, s1$pe$pvalue[s1$pe$op=="~"],0,s2$pe$pvalue[s2$pe$op=="~"]),holm=p.adjust(c(0, s1$pe$pvalue[s1$pe$op=="~"],0,s2$pe$pvalue[s2$pe$op=="~"]), method="holm"), mod=c(rep("mod1", 1+length(s1$pe$pvalue[s1$pe$op=="~"])), rep("mod2", 1+length(s2$pe$pvalue[s2$pe$op=="~"]))))
p.vec1

## plot SEM

embed_plot_pdf(lavaanPlot(model=sys1, coefs=T, stars=c("regress"), stand=T), "C:/Users/khanb/OneDrive/Documents C/Documents/University/Auckland University of Technology/Study 3 Static Study/Results/SEM_Complete.pdf")
embed_plot_pdf(lavaanPlot(model=sys2, coefs=T, stars=c("regress"), stand=T), "C:/Users/khanb/OneDrive/Documents C/Documents/University/Auckland University of Technology/Study 3 Static Study/Results/SEM_Averaged.pdf")

## Obtain required parameters

s1.1=s1$pe[s1$pe$op=="~",]
s2.1=s2$pe[s2$pe$op=="~",]

## Create edge list with parameters

s1.2=cbind.data.frame(from=s1.1$lhs, to=s1.1$rhs,estimate=s1.1$est, p=s1.1$pvalue)
s2.2=cbind.data.frame(from=s2.1$lhs, to=s2.1$rhs,estimate=s2.1$est, p=s2.1$pvalue)


## Define colours

s1.2$colour=ifelse(s1.2$estimate>0, "springgreen3", "firebrick3")
s2.2$colour=ifelse(s2.2$estimate>0, "springgreen3", "firebrick3")

## Set strings

s1.2$string=paste0(s1.2$to,"~", s1.2$from)
s2.2$string=paste0(s2.2$to,"~", s2.2$from)

## Construct plot

p1=graphviz.plot(mod1)
p2=graphviz.plot(mod2.3)
edge1=edgeRenderInfo(p1)
edge2=edgeRenderInfo(p2)

## Order rows

s1.3=s1.2[c(match(names(edge1$enamesFrom),s1.2$string)),]
s2.3=s2.2[c(match(names(edge2$enamesFrom),s2.2$string)),]

## Make changes to plots

for(i in 1:nrow(s1.3)){
edgeRenderInfo(p1)$col[i]=s1.3$colour[i]
}

for(i in 1:nrow(s2.3)){
  edgeRenderInfo(p2)$col[i]=s2.3$colour[i]
}

for(i in 1:nrow(s1.3)){
  edgeRenderInfo(p1)$lwd[i]=2
}

for(i in 1:nrow(s2.3)){
  edgeRenderInfo(p2)$lwd[i]=2
}

## render and save plots

svg("240515_Coloured_BN_Complete_Data.svg")
renderGraph(p1)
dev.off()

svg("240515_Coloured_BN_Averaged.svg")
renderGraph(p2)
dev.off()

## Specify parameters

param1=bn.fit(mod1, dat2[,c(3:23)])
param2=bn.fit(mod2.3, dat2[,c(3:23)])

## Obtain centrality mod1## Obtain centrality Statistics

close1=closeness(as.igraph(param1))
close1.1=(close1-mean(close1, na.rm=T))/sd(close1, na.rm=T)

close2=closeness(as.igraph(param2))
close2.1=(close2-mean(close2, na.rm=T))/sd(close2, na.rm=T)

between1=betweenness(as.igraph(param1))
between1.1=(between1-mean(between1, na.rm=T))/sd(between1, na.rm=T)

between2=betweenness(as.igraph(param2))
between2.1=(between2-mean(between2, na.rm=T))/sd(between2, na.rm=T)

degree1=degree(as.igraph(param1))
degree2=degree(as.igraph(param2))

Central=cbind.data.frame(M1.Close=close1.1, M1.between=between1.1, M1.degree=degree1,M2.Close=close2.1, M2.between=between2.1, M2.degree=degree2)
Central

## Probability queries

round(median(dat2$MATRICS.Processing.Speed),2)
round(median(dat2$MATRICS.Verbal),2)
round(median(dat2$MATRICS.Vigil),2)
round(median(dat2$MATRICS.Reasoning),2)
round(median(dat2$MATRICS.Memory),2)
round(median(dat2$PANSS.Negative),2)
round(median(dat2$PANSS.General),2)
round(median(dat2$CGI.Severity),2)
round(median(dat2$QOL),2)

round(median(dat2$DAI))
round(median(dat2$CDSS))
table(dat2$Employment)

set.seed(1000)

prob1.1=cpquery(param1, event=(MATRICS.Reasoning< median(dat2$MATRICS.Reasoning)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.2=cpquery(param1, event=(MATRICS.Memory< median(dat2$MATRICS.Memory)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.3=cpquery(param1, event=(MATRICS.Vigil< median(dat2$MATRICS.Vigil)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.4=cpquery(param1, event=(MATRICS.Verbal< median(dat2$MATRICS.Verbal)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.5=cpquery(param1, event=(PANSS.Negative> median(dat2$PANSS.Negative)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.6=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (MATRICS.Processing.Speed< median(dat2$MATRICS.Processing.Speed)))
prob1.7=cpquery(param1, event=(MATRICS.Memory< median(dat2$MATRICS.Memory)), evidence = (MATRICS.Reasoning< median(dat2$MATRICS.Reasoning)))
prob1.8=cpquery(param1, event=(MATRICS.Vigil< median(dat2$MATRICS.Vigil)), evidence = (MATRICS.Memory< median(dat2$MATRICS.Memory)))
prob1.9=cpquery(param1, event=(MATRICS.Verbal< median(dat2$MATRICS.Verbal)), evidence = (MATRICS.Memory< median(dat2$MATRICS.Memory)))
prob1.10=cpquery(param1, event=(PANSS.General> median(dat2$PANSS.General)), evidence = (MATRICS.Memory< median(dat2$MATRICS.Memory)))
prob1.11=cpquery(param1, event=(CGI.Severity> median(dat2$CGI.Severity)), evidence = (MATRICS.Memory< median(dat2$MATRICS.Memory)))

prob1.12=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (DAI< median(dat2$DAI)))
prob1.13=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (CGI.Severity> median(dat2$CGI.Severity)))
prob1.14=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (PANSS.Negative> median(dat2$PANSS.Negative)))
prob1.15=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (CDSS>median(dat2$CDSS)))
prob1.16=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (Employment==1))
prob1.17=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (Employment==2))
prob1.18=cpquery(param1, event=(QOL< median(dat2$QOL)), evidence = (Employment==3))

round(c(prob1.1,prob1.2,prob1.3,prob1.4,prob1.5,prob1.6,prob1.7,prob1.8,prob1.9,prob1.10,prob1.11,prob1.12,prob1.13,prob1.14,prob1.15,prob1.16,prob1.17,prob1.18),3)
