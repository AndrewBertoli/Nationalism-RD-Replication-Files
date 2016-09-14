# First read the data into R.

rcdata=read.csv("https://raw.githubusercontent.com/AndrewBertoli/World-Cup-Code/master/RegionalChampionshipData.csv",stringsAsFactors=FALSE)

# Now download these libraries

library(lmtest)
library(KernSmooth)
library(sandwich)
library(ggplot2)

# Next subset the data. There are three categories of countries: countries that fell outside the RD sample (Use=0), countries in the RD sample (Use=1), and countries that were not used because of overlap problems (Use=2). However, the results remain significant when the countries with (Use=2) that barely qualified are included in the sample. I also drop the Iran-North Korea (1988) Dyad, because Iran was an extreme outlier prior to qualification (Use=3).

rcdata.use=rcdata[rcdata$Use==1,]

t=rcdata.use[rcdata.use$Treat==1,]
c=rcdata.use[rcdata.use$Treat==0,]

sample=rbind(t,c)


# Balance Plot

pdf("BalancePlot2.pdf", width = 7, height = 8)
BalancePlot(Data=sample, Treat=sample$Treat, Title="Figure 5: Balance Plot for the Regional Championships", Covariates=c("Irst",'Milex','Milper','Tpop','Upop','BirthRate','DeathRate','InfantMortality','Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production', 'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous Regional Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE)
dev.off()


pdf("MIDComparison3.pdf")
tAGG=apply(t[,13:27], 2, sum)
cAGG=apply(c[,13:27], 2, sum)
Year=seq(-4,3,by=0.5)
tsdata=data.frame(c(Year,Year),c(tAGG,cAGG),c(rep(1,15),rep(0,15)))
colnames(tsdata)=c("Year","Aggression","Treat")
tsdata=data.frame(Year, tAGG, cAGG)
ggplot(tsdata, aes(x=Year)) + geom_line(aes(y=tAGG), colour="Blue", show_guide=TRUE) + geom_line(aes(y=cAGG) , colour="gray54", show_guide=TRUE) + geom_hline(yintercept=0) + geom_vline(xintercept=0, linetype=2) + annotate("text", x=-0.18, y=11, angle=90, label="Qualification", size=3.5) + geom_vline(xintercept=1.142466, linetype=2) + annotate("text", x=0.962466, y=11.6, angle=90, label="Last Tournament", size=3.5) + scale_x_continuous(breaks = c(-3.75,-3,-2.25,-1.5,-0.75,1.142466+3/4,1.142466+3/2,1.142466+9/4), labels = c(-5,-4,-3,-2,-1,1,2,3)) + labs(title="Figure 6. Comparing Aggression for the Regional Championships") + ylab("Militarized Interstate Disputes Initiated") + xlab("Years Since Qualification and the Last Tournament") + theme_bw() + geom_rect(aes(xmin = -3.9, xmax = -2, ymin = 11.17, ymax = 12.4),fill="white",colour="black")+ annotate("text", x = c(-3.65, -3.65, -3, -2.78), y = c(12.35, 11.829, 12.05, 11.55), label = c("_", "_", "Qualifiers", "Non-qualifiers"), colour=c("Blue", "gray54", "black", "black"), size=c(8, 8, 4, 4))
dev.off()


# Statistical Tests

t.test(t$AGG3YearsAfter-t$AGG3YearsBefore, c$AGG3YearsAfter-c$AGG3YearsBefore, paired=TRUE)

# Including the countries we dropped because of overlap(Use=2). (Note: the estimated treatment effect will have the opposite sign when t.test is written with a formula, which I do below to simplify the notation)

t.test(AGG3YearsAfter-AGG3YearsBefore~Treat, rbind(sample, rcdata[rcdata$Use==2,][abs(rcdata[rcdata$Use==2,]$PointsFromCutpoint)<1,]), paired=TRUE)



# Figures from Online Appendix

# Regional Championship Balance Plot (2 Point Window)

twopoints=rcdata[rcdata$Use%in%c(0,1) & abs(rcdata$PointsFromCutpoint)<=2,]

t.twopoints=twopoints[twopoints$Treat==1,]
c.twopoints=twopoints[twopoints$Treat==0,]
c.twopoints=c.twopoints[t.twopoints$Score>=5,]
t.twopoints=t.twopoints[t.twopoints$Score>=5,]

twopoints=rbind(t.twopoints, c.twopoints)


pdf("BalancePlotRC2Point.pdf", width = 7, height = 8)
BalancePlot(Data=twopoints, Treat=twopoints$Treat, Title="Figure 6a. Balance Plot for the Regional Championships (2-Point Window)", Title.Size=1, Covariates= c("Irst",'Milex','Milper', 'Tpop','Upop','BirthRate', 'DeathRate','InfantMortality', 'Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production', 'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE)
dev.off()


# Combined Sample with 2 Point Window

twopoints=twopoints[,-c(2:3,13:28)]

data=read.csv("WorldCupData.csv")

data.use=data[which(data$Use==1),]

data.use=data.use[,which(!colnames(data.use)%in%c("Rank", "NumberOfParticipants"))]

data.use=data.use[,-c(11:32)]

combined=rbind(data.use,twopoints)

pdf("BalancePlotCombined2Points.pdf", width = 7, height = 8)
BalancePlot(Data=combined, Treat=combined$Treat, Title="Figure 7a. Balance Plot for the Combined Sample (2-Point Window)", Title.Size=1, Covariates= c("Irst",'Milex','Milper', 'Tpop','Upop','BirthRate', 'DeathRate','InfantMortality', 'Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production', 'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE)
dev.off()


# By Sub-Groups

t.test(AGG3YearsAfter-AGG3YearsBefore ~ Treat, sample[sample$SoccerMostPopular==1,])

t.test(AGG3YearsAfter-AGG3YearsBefore ~ Treat, sample[sample$SoccerMostPopular==0,])

t.test(AGG3YearsAfter-AGG3YearsBefore ~ Treat, sample[sample$Democracy==1,])

t.test(AGG3YearsAfter-AGG3YearsBefore ~ Treat, sample[sample$Democracy==0,])



# Robustness checks

# Standard t-test on Just the Outcome

t.test(t$AGG3YearsAfter,c$AGG3YearsAfter,paired=TRUE)


# Wilcoxon signed rank test

wilcox.test(t$AGG3YearsAfter-t$AGG3YearsBefore,c$AGG3YearsAfter- c$AGG3YearsBefore,paired=TRUE)


# Permutation Inference (with mean as test statistic)

Permutation.Test(t$AGG3YearsAfter-t$AGG3YearsBefore,c$AGG3YearsAfter- c$AGG3YearsBefore, Paired=TRUE)


# Testing for an increase

t.test(c(with(t,AGG3YearsAfter-AGG3YearsBefore))>0,c(with(c,AGG3YearsAfter-AGG3YearsBefore))>0,paired=TRUE)

# Testing for a decrease

t.test(c(with(t,AGG3YearsAfter-AGG3YearsBefore))<0,c(with(c,AGG3YearsAfter-AGG3YearsBefore))<0,paired=TRUE)

# Linear Regression

sample=sample[abs(sample$PointsFromCutpoint)<=1,]

# No Controls

model=lm(AGG3YearsAfter - AGG3YearsBefore ~ Treat, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military Controls

model=lm(AGG3YearsAfter - AGG3YearsBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy + Tpop + Upop + CINC + EngagedCivilWar + EndedCivilWar + GreatPower + Alliances + USAlly, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military and Economic Controls

model=lm(AGG3YearsAfter - AGG3YearsBefore ~ Treat + LandArea + Irst + Milper + Milex + Energy + Tpop + Upop + CINC + EngagedCivilWar + EndedCivilWar + EntranceYear + GreatPower + Alliances + USAlly + Imports + Exports, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# Military, Economic, and Demographic Controls

model=lm(AGG3YearsAfter - AGG3YearsBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + Democracy + EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + Imports + Exports + MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# Linear Regression Controlling for Previous Outcome

# No Controls

model=lm(AGG3YearsAfter ~ Treat + AGG3YearsBefore, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military Controls

model=lm(AGG3YearsAfter ~ Treat + AGG3YearsBefore + LandArea + Irst + Milper + Milex+ Energy + Tpop + Upop + CINC + EngagedCivilWar + EndedCivilWar + GreatPower + Alliances + USAlly, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military and Economic Controls

model=lm(AGG3YearsAfter ~ Treat + AGG3YearsBefore + LandArea + Irst + Milper + Milex + Energy + Tpop + Upop + CINC + EngagedCivilWar + EndedCivilWar + EntranceYear + GreatPower + Alliances + USAlly + Imports + Exports, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# Military, Economic, and Demographic Controls

model=lm(AGG3YearsAfter ~ Treat + AGG3YearsBefore + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + Democracy + EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + Imports + Exports + MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Shifting the Minimum Score- Output is (Minimal Score, p-value, and N)

rcdata.use= rcdata[rcdata$Use%in%c(0,1),]

treatment=rcdata.use[rcdata.use$Treat==1,]
control=rcdata.use[rcdata.use$Treat==0,]
treatment=treatment[which(treatment$PointsFromCutpoint<=1),]
control=control[which(control$PointsFromCutpoint>=-1),]

for(i in 1:10){

control=control[which(treatment$Score>=i),]
treatment=treatment[which(treatment$Score>=i),]


test1=t.test(treatment$AGG3YearsAfter- treatment$AGG3YearsBefore,control$AGG3YearsAfter-control$AGG3YearsBefore,paired=TRUE)


print(c(i, test1$estimate,test1$p.value, length(treatment[,1])))

}


# Adjusting Window- Output is (Window Size, p-value, and N)


rcdata.use= rcdata[rcdata$Use%in%c(0,1),]

treatment=rcdata.use[rcdata.use$Treat==1,]
control=rcdata.use[rcdata.use$Treat==0,]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

for(i in 9:0){

treatment=treatment[which(treatment$PointsFromCutpoint<=i),]
control=control[which(control$PointsFromCutpoint>=-i),]

test1=t.test(treatment$AGG3YearsAfter- treatment$AGG3YearsBefore,control$AGG3YearsAfter- control$AGG3YearsBefore,paired=TRUE)


print(c(i, test1$estimate,test1$p.value,length(treatment[,1])))

}



# Continuity Graphs

qualifiers=rcdata[rcdata$Use%in%c(0,1)&rcdata$Treat==1,]
nonqualifiers=rcdata[rcdata$Use%in%c(0,1)&rcdata$Treat==0,]

nonqualifiers=nonqualifiers[qualifiers$Score>=5,]
qualifiers=qualifiers[qualifiers$Score>=5,]

# I set the ties at 0.0001 and -0.0001, instead of counting both the winners and losers as 0.  

nonqualifiers[nonqualifiers$PointsFromCutpoint==0,]$PointsFromCutpoint=-0.01
qualifiers[qualifiers$PointsFromCutpoint==0,]$PointsFromCutpoint=0.01

sample=rbind(qualifiers,nonqualifiers)

# You can compute the bandwidths with the function provided by Devin Caughey, available at: http://web.mit.edu/caughey/www/Site/Code.html


pdf("ContinuityGraphsRC.pdf", height=4.5, width=9)

op=par(mfrow=c(1,3), mar=c(4,3.9,3,0),oma=c(0,0,2,0))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsBefore,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Main="3 Years Before Qualification", xlab="Points Above/Below Cut-Point",ylab="Militarized Interstate Disputes Initiated",Bandwidth=11,Kernel="Triangular",Smoother="Local Linear", Shade.Color="blanchedalmond", Window=c(-2.5,2.5), NBoots=10000 , Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsAfter,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Main="3 Years After Qualification",xlab="Points Above/Below Cut-Point",ylab="",Bandwidth=11,Kernel="Triangular",Smoother="Local Linear", Shade.Color="blanchedalmond", Window=c(-2.5,2.5) , NBoots=10000 , Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsAfter-sample$AGG3YearsBefore,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Main="Change in Aggression", xlab="Points Above/Below Cut-Point",ylab="", Bandwidth=11,Kernel="Triangular",Smoother="Local Linear", Shade.Color="blanchedalmond", Window=c(-2.5,2.5), NBoots=10000 , Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5))

title("Figure 8a. Change in Aggression for the Regional Championships",outer=TRUE,cex.main=2)

dev.off()














# Combined Sample

# Balance-Plot

rcdata.use= rcdata[rcdata$Use==1,]

rcdata.use=rcdata.use[,which(!colnames(rcdata.use)%in%c("Day", "Month"))]
# Get’s rid of Month and Day of Qualification so the samples can be combined

rcdata.use=rcdata.use[,which(!colnames(rcdata.use)%in%c("Day", "Month"))]

rcdata.use=rcdata.use[,-(11:26)]

data=read.csv("WorldCupData.csv")

data.use=data[which(data$Use==1),]

data.use=data.use[,which(!colnames(data.use)%in%c("Rank", "NumberOfParticipants"))]

data.use=data.use[,-c(11:32)]

combined=rbind(data.use,rcdata.use)

pdf("BalancePlotCombined.pdf", width = 7, height = 8)
BalancePlot(Data=combined, Treat=combined$Treat, Title="Figure 9a. Balance Plot for Combined Sample"
, Covariates= c("Irst",'Milex','Milper', 'Tpop','Upop','BirthRate', 'DeathRate','InfantMortality', 'Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production', 'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE)
dev.off()




# Statistical Test for Combined Sample

data$AGGAfter=with(data,  AGGP.5 + AGGP1 + AGGP1.5 + AGGP2 + AGGP2.5)

data$AGGBefore=with(data, AGG0 + AGGM.5 + AGGM1 + AGGM1.5 + AGGM2)

sample=data[which(data$Use==1),]

c1=sample[which(sample$Treat==0),]
t1=sample[which(sample$Treat==1),]

rcdata.use= rcdata[rcdata$Use==1,]

t=rcdata.use[rcdata.use$Treat==1,]
c=rcdata.use[rcdata.use$Treat==0,]


t.test(c(t1$AGGAfter-t1$AGGBefore, t$AGG3YearsAfter-t$AGG3YearsBefore), c(c1$AGGAfter-c1$AGGBefore, c$AGG3YearsAfter-c$AGG3YearsBefore), paired=TRUE)



# Continuity Graph for combined sample

t1=data[data$Treat==1,c(1,2,4,5,8,10)]
t2=rcdata[rcdata$Use%in%c(0,1)&rcdata$Treat==1,c(1,4,6,7,10,12)]

qualifiers=rbind(t1,t2)

c1=data[data$Treat==0,c(1,2,4,5,8,10)]
c2=rcdata[rcdata$Use%in%c(0,1)&rcdata$Treat==0,c(1,4,6,7,10,12)]

nonqualifiers=rbind(c1,c2)

nonqualifiers=nonqualifiers[qualifiers$Score>=5,]
qualifiers=qualifiers[qualifiers$Score>=5,]

# I set the ties at 0.0001 and -0.0001, instead of counting both the winners and losers as 0.  

nonqualifiers[nonqualifiers$PointsFromCutpoint==0,]$PointsFromCutpoint=-0.0001
qualifiers[qualifiers$PointsFromCutpoint==0,]$PointsFromCutpoint=0.0001

sample=rbind(qualifiers,nonqualifiers)

pdf("ContinuityGraphsCombined.pdf", height=4.5, width=9)

op=par(mfrow=c(1,3), mar=c(4,3.9,3,0),oma=c(0,0,2,0))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsBefore,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Shade.Color="lightyellow", Main="3 Years Before Qualification", xlab="Points Above/Below Cut-Point",ylab="Militarized Interstate Disputes Initiated",Bandwidth=2.85621, Window=c(-2.5,2.5), Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5,3.5,4.5,5.5, 6.5,7.5,8.5,9.5, 10.5,11.5,12.5))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsAfter,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Shade.Color="lightyellow", Main="3 Years After Qualification",xlab="Points Above/Below Cut-Point",ylab="",Bandwidth=3.2316, Window=c(-2.5,2.5), Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5, 3.5,4.5,5.5,6.5, 7.5,8.5, 9.5,10.5,11.5,12.5))

RDPlot(X=sample$PointsFromCutpoint,Y=sample$AGG3YearsAfter-sample$AGG3YearsBefore,xlim=c(-3.7,3.7),ylim=c(-1.5,1.5), Shade.Color="lightyellow", Main="Change in Aggression", xlab="Points Above/Below Cut-Point",ylab="", Bandwidth=4.1937, Window=c(-2.5,2.5) , Breaks=c(-11.5,-10.5,-9.5,-8.5,-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5, 10.5,11.5,12.5))

title("Figure 10a. Change in Aggression for the Combined Sample",outer=TRUE,cex.main=2)

dev.off()
