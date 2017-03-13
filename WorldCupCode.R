# Replication code for "Nationalism and Conflict: Lessons from International Sports"

# Last Updated: March 13, 2017

# These are the required libraries

require(lmtest)
require(KernSmooth)
require(sandwich)
require(ggplot2)
require(rdrobust)
require(RCurl)
require(devtools)

# We begin by reading in the data, which includes all countries that were the last out or first in to the World Cup

data=read.csv("https://raw.githubusercontent.com/AndrewBertoli/World-Cup-Code/master/WorldCupData.csv",stringsAsFactors=FALSE)

# The variables in this dataset should be fairly clear. Treat (0,1) denotes whether the country went to the World Cup (1) 
# or stayed home (0). The AGG variables track the outcome of interest--disputes initiated--over time. Specifically, they 
# show how many disputes each country initiated in six month periods (Jan-Jun and Jul-Dec). AGG0 is the number of disputes 
# initiated in the six months prior to the World Cup year (Jul-Dec). AGGP.5 is the number initiated in the first six months 
# of the World Cup year (Jan-Jun), and AGGP1 is the number initiated in the last six months of the World Cup year (Jul-Dec). 
# The AGG variable continues to count up by six month periods--AGGP1.5, AGGP2, AGGP2.5...   Similarly, AGGM.5 is the number 
# of disputes initiated in the first half of the year before the World Cup year (Jan-Jun), and AGGM1 is the number initiated 
# in the six monts before that (Jul-Dec of the second year before the World Cup year). The AGGM variables continue counting 
# backwards. So for countries that went to the World Cup in the summer of 1990, AGGM1 is the number of disputes initiated 
# from Jul-Dec 1988, AGGM.5 refers to Jan-Jun 1989, AGG0 to Jul-Dec 1989, AGGP.5 to Jan-Jun 1990, AGGP1 to Jul-Dec 1990, and 
# so on. The REV variable works the same way, except it denotes revisionist disputes instead of initiated disputes. Some 
# political scientists believe that revisionist disputes are a better measure of aggression, but most use initiated disputes. 
# All other variables in the dataset should be fairly easy to interpret. For questions, feel free to email me at 
# bertoli@usc.edu.

# We can now subset to the countries that were within two points of making or missing qualification

sample=data[which(data$Use==1),]

# We can then divide these countries into the treatment and control groups

c=sample[which(sample$Treat==0),]
t=sample[which(sample$Treat==1),]

# We can now read in the functions that will make the figures from the paper. These include BalancePlot(), RDPlot(), 
# and PermutationTest()

source_url("https://raw.githubusercontent.com/AndrewBertoli/World-Cup-Code/master/WorldCupFunctions.R")


set.seed(0)

# Figure 1-Balance Plot

pdf("BalancePlot.pdf", width = 7, height = 5.1)
BalancePlot(Data=sample, Treat=sample$Treat, Title="Figure 1. Balance Between the Qualifiers and Non-qualifiers",
Covariates=c('Tpop','Upop','Imports','Exports','CINC',"GreatPower",'Democracy','EngagedCivilWar','EndedCivilWar',
'EntranceYear','BirthRate','InfantMortality','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular',
'PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Total Population', 'Urban Population',
'Imports', 'Exports', 'Material Power Score', 'Great Power Status', 'Level of Democracy', 'Engaged in Civil War', 
'Resolved Civil War', 'Year of State Formation', 'Birth Rate', 'Infant Mortality','Life Expectancy', 'Median Age', 
"Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  
'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), 
Shade.Color= "cadetblue2", Built.In.Tests=c("PermutationTest"), Point.Color= "black", Year.Covariates=c("EntranceYear"),
Paired=TRUE,na.rm=TRUE)
dev.off()



# Figure 2-Time Series

pdf("MIDComparison.pdf")
tAGG=apply(t[,11:31], 2, sum)
cAGG=apply(c[,11:31], 2, sum)
Year=seq(-5.5,4.5,by=0.5)
tsdata=data.frame(Year, tAGG, cAGG)
ggplot(tsdata, aes(x=Year)) + geom_line(aes(y=tAGG), colour="Blue", show_guide=TRUE) + geom_line(aes(y=cAGG) ,
colour="gray67", show_guide=TRUE) + geom_hline(yintercept=0) + geom_vline(xintercept=-0.5, linetype=2) + 
geom_vline(xintercept=0, linetype=2) + annotate("text", x=-0.68, y=12.2, angle=90, label="Qualification", size=4.2) + 
scale_x_continuous(breaks = c(-5.5,-4.5,-3.5,-2.5,-1.5,1,2,3,4), labels = c(-5,-4,-3,-2,-1,1,2,3,4)) + 
annotate("text", x=-0.2, y=12.2, angle=90, label="World Cup", size=4.2) + ylab("Militarized Interstate Disputes Initiated") +
xlab("Years Since Qualification and the World Cup") + theme_bw() + geom_rect(aes(xmin = -5.4, xmax = -3.1, ymin = 13.37, 
ymax = 14.9),fill="white",colour="black") + annotate("text", x = c(-5.15, -5.15, -4.4, -4.1), y = c(14.88, 14.19, 14.5, 13.8), 
label = c("_", "_", "Qualifiers", "Non-qualifiers"), colour=c("Blue", "gray67", "black", "black"), size=c(8, 8, 4, 4)) + 
theme(plot.title = element_text(lineheight=.8, face="bold",size=14.7),axis.title=element_text(size=13.7),
axis.text=element_text(size=11.7))
dev.off()



# Table 4-Post-treatment Statistical Tests

# I define AGG After as the number of militarized interstate disputes initiated in the period ranging from 
# qualification to the second year following the World Cup and AGG Before as the period of the same length prior 
# to qualification. I also use the same time intervals for revisionist disputes.

data$AGGAfter=with(data,  AGGP.5 + AGGP1 + AGGP1.5 + AGGP2 + AGGP2.5)

data$AGGBefore=with(data, AGG0 + AGGM.5 + AGGM1 + AGGM1.5 + AGGM2)

data$REVAfter=with(data,  REVP.5 + REVP1 + REVP1.5 + REVP2 + REVP2.5)

data$REVBefore=with(data, REV0 + REVM.5 + REVM1 + REVM1.5 + REVM2)

sample=data[which(data$Use==1),]

c=sample[which(sample$Treat==0),]
t=sample[which(sample$Treat==1),]


# Dif-in-dif t-test

t.test(AGGAfter-AGGBefore~Treat,sample,paired=TRUE)
PermutationTest(AGGAfter-AGGBefore~Treat,sample, Paired=TRUE,Output="full")

# Countries Where Soccer Is the Most Popular Sport
t.test(AGGAfter-AGGBefore ~ Treat, sample[sample$SoccerMostPopular==1,])
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[sample$SoccerMostPopular==1,],Output="full")
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[t$SoccerMostPopular==1&c$SoccerMostPopular==1,],Output="full")

# To keep the paired structure of the data, you can subset to the pairs where soccer was the most popopular sport 
# for both countries
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[t$SoccerMostPopular==1&c$SoccerMostPopular==1,],Paired=TRUE,Output="full")

# Countries Where Soccer Is Not the Most Popular Sport

t.test(AGGAfter-AGGBefore ~ Treat, sample[sample$SoccerMostPopular==0,])
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[sample$SoccerMostPopular==0,],Output="full")

# Great Powers

t.test(AGGAfter-AGGBefore~Treat, sample[sample$GreatPower==1,])
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[sample$GreatPower==1,],Output="full")

# Minor Powers

t.test(AGGAfter-AGGBefore~Treat, sample[sample$GreatPower==0,])
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[sample$GreatPower==0,],Output="full")

# Countries that Qualified or Missed By 1 point or Less

t.test(AGGAfter-AGGBefore~Treat,sample[abs(sample$PointsFromCutpoint)<=1,],paired=TRUE)
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[abs(sample$PointsFromCutpoint)<=1,],Output="full")


# Countries that Qualified or Missed By 3 points or Less-We must go back to data to add the countries that were 3 points away

treatment=data[which(data$Treat==1),]
control=data[which(data$Treat==0),]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

no.window=rbind(treatment,control)

t.test(AGGAfter-AGGBefore~Treat,no.window[abs(no.window$PointsFromCutpoint)<=3,],paired=TRUE)
PermutationTest(AGGAfter-AGGBefore ~ Treat,no.window[abs(no.window$PointsFromCutpoint)<=3,],Output="full")

# Removing Ties

t.test(AGGAfter-AGGBefore~Treat,sample[sample$PointsFromCutpoint!=0,],paired=TRUE)
PermutationTest(AGGAfter-AGGBefore~Treat,sample[sample$PointsFromCutpoint!=0,], Paired=TRUE,Output="full")


# Linear Regression with Robust Standard Errors

model=lm(AGGAfter - AGGBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + 
Democracy + EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + 
Imports + Exports + MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + 
SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# t-test on Post-Treatment outcome alone (not Difference-in-Differences)

t.test(AGGAfter~Treat,sample,paired=TRUE)
PermutationTest(AGGAfter~Treat,sample, Paired=TRUE,Output="full")

# Wilcoxon signed-rank test

wilcox.test(t$AGGAfter-t$AGGBefore,c$AGGAfter-c$AGGBefore,paired=TRUE)

# Dummy for an increase

t.test(c(with(t,AGGAfter-AGGBefore))>0,c(with(c,AGGAfter-AGGBefore))>0,paired=TRUE)
PermutationTest(t$AGGAfter-t$AGGBefore>0, c$AGGAfter-c$AGGBefore>0, Paired=TRUE,Output="full")

# Removing the US and Soviet Union

t.test(AGGAfter-AGGBefore~Treat, sample[!sample$Country%in%c("United States","Soviet Union"),])
PermutationTest(AGGAfter-AGGBefore ~ Treat,sample[!sample$Country%in%c("United States","Soviet Union"),],Output="full")



# Revisionist Disputes

t.test(REVAfter-REVBefore~Treat,sample,paired=TRUE)
PermutationTest(REVAfter-REVBefore~Treat,sample, Paired=TRUE,Output="full")

# Disputes That Involved the Use of Force

t.test(ForceAGGAfter-ForceAGGBefore~Treat,sample,paired=TRUE)
PermutationTest(ForceAGGAfter-ForceAGGBefore~Treat,sample, Paired=TRUE,Output="full")

# Disputes That Involved a Direct Attack, a Clash, or the Start of Interstate War

t.test(AttackAGGAfter-AttackAGGBefore~Treat,sample,paired=TRUE)
PermutationTest(AttackAGGAfter-AttackAGGBefore~Treat,sample, Paired=TRUE,Output="full")

# Changes in Military Participation

# Since we have data on the total population size of countries, we can calculate the percentage of the population 
# that participated in the military the year before and the year after the World Cup. We can then do a difference-in-
# differences t-test.  


sample$MilperIncrease=with(sample,MilperPercentAfter-Milper/Tpop)
sample$MilperIncreasePercent=with(sample,MilperPercentAfter/(Milper/Tpop))
t.test(MilperIncrease~Treat,sample)
t.test(MilperIncrease~Treat,sample,paired=TRUE)$p.value
mean(t$Milper/t$Tpop,na.rm=TRUE)
mean(c$Milper/c$Tpop,na.rm=TRUE)
mean(t$MilperPercentAfter,na.rm=TRUE)
mean(c$MilperPercentAfter,na.rm=TRUE)
PermutationTest(MilperIncrease~Treat,sample, Paired=TRUE,Output="full",na.rm=TRUE)







# Figure 3-Continuity Graphs

# I set the ties at 0.01 and -0.01, instead of counting both the winners and losers as 0.  

no.window[no.window$PointsFromCutpoint==0&no.window$Treat==0,]$PointsFromCutpoint=-0.01
no.window[no.window$PointsFromCutpoint==0&no.window$Treat==1,]$PointsFromCutpoint=0.01

# Because the score is highly discrete, the normal bandwidth procedures do not work on this data. I set the bandwidths 
# at 4, although the results are significant for any bandwidth greater than or equal to 1.

pdf("WCCont.pdf", height=4.5, width=9.5)

op=par(mfrow=c(1,3), mar=c(4,4.5,3,0),oma=c(0,1,2,1))

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGBefore,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), Main="Aggression Before", 
xlab="Points Above/Below Cut-Point",ylab="Militarized Interstate Disputes Initiated",Kernel="Triangular", 
Smoother="Local Linear",Bandwidth=10, Window=c(-2.5,2.5),NBoots=10000,cex.main=1.8,cex.lab=1.5)

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGAfter,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), Main="Aggression After",
xlab="Points Above/Below Cut-Point", ylab="",Kernel="Triangular", Smoother="Local Linear", Bandwidth=10, Window=c(-2.5,2.5),
NBoots=10000,cex.main=1.8,cex.lab=1.5,yaxt='n')

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGAfter-no.window$AGGBefore,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), 
Main="Change in Aggression", xlab="Points Above/Below Cut-Point", ylab="", Kernel="Triangular", Smoother="Local Linear", 
Bandwidth=10, Window=c(-2.5,2.5),NBoots=10000,cex.main=1.8,cex.lab=1.5,yaxt='n')

title("Figure 3. Change in Aggression for the World Cup",outer=TRUE,cex.main=2.5)

dev.off()






# Conflicts between countries that played against each other

dyads=read.csv("https://raw.githubusercontent.com/AndrewBertoli/Nationalism-RD-Replication-Files/master/WorldCupDyads.csv",
stringsAsFactors=FALSE)

t.test(dyads$DisputesAfter,dyads$DisputesBefore,paired=TRUE)

sum(dyads$DisputesBefore)
sum(dyads$DisputesAfter)

sum(dyads$DisputesBefore>0)
sum(dyads$DisputesAfter>0)


# Tracking the total number of all other disputes over this period

# You can download the correlates of war data from http://www.correlatesofwar.org/data-sets/MIDs

disp=read.csv("MIDB_4.0.csv",stringsAsFactors=FALSE)

Year=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)

disp=disputes

disputes_with_wc_part=c()

for(i in 1:length(Year)){
index1=disp[c(which(disp$StYear>Year[i]-1&disp$StYear<Year[i]+1),which(disp$StYear==Year[i]-2&disp$StMon>=6),
which(disp$StYear==Year[i]+2&disp$StMon<6)),]$DispNum3			
index2=disp[disp$StAbb%in%c(data[data$Year==Year[i],]$Country1,data[data$Year==Year[i],]$Country2),]$DispNum3		
disputes_with_wc_part=c(disputes_with_wc_part,intersect(index1,index2))}

disp=disp[-which(disp$DispNum3%in%disputes_with_wc_part),]

Month=6
Day=1
	
allprevdisp=disp[c(
	which(disp$StYear%in%Year&disp$StMon==Month&disp$StDay<=Day),
	which(disp$StYear%in%Year&disp$StMon<Month),
	which(disp$StYear%in%(Year-1)),
	which(disp$StYear%in%(Year-2)&disp$StMon>Month),
	which(disp$StYear%in%(Year-2)&disp$StMon==Month&disp$StDay>Day)	
	),]		

num_all_prev=length(unique(allprevdisp$DispNum3))

alldisp=disp[c(
	which(disp$StYear%in%Year&disp$StMon==Month&disp$StDay>Day),
	which(disp$StYear%in%Year&disp$StMon>=Month),
	which(disp$StYear%in%(Year+1)),
	which(disp$StYear%in%(Year+2)&disp$StMon<Month),
	which(disp$StYear%in%(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		

num_all=length(unique(alldisp$DispNum3))

num_all/num_all_prev

# Number of all disputes increased by 5.1%.









# Senegal 2002 data

nmc=read.csv("NMC_v4_0(1).csv",stringsAsFactors=FALSE)

# The code belwo will print the year, change in military expenditures, and change in military personel 

for(i in 1990:2006){
year=nmc[nmc$stateabb=="SEN"&nmc$year==i,c("milex","milper")]
nextyear=nmc[nmc$stateabb=="SEN"&nmc$year==(i+1),c("milex","milper")]
change=round(as.numeric(nextyear/year)*100-100,0)
print(c(i,change[1],change[2]))}






# Justifying the two-point regression discontinuity window

# You can copy and paste the RD window selection functions (rdwinselect.R and rdlocrand_fun.R) from this site:
# https://sites.google.com/site/rdpackages/rdlocrand/r

treatment=data[which(data$Treat==1),]
control=data[which(data$Treat==0),]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

treatment$PointsFromCutpoint[treatment$PointsFromCutpoint==0]=0.001
control$PointsFromCutpoint[control$PointsFromCutpoint==0]=-0.001

no.window=rbind(treatment,control)

X=no.window[,c('Tpop','Upop','Imports','Exports','CINC',"GreatPower",'Democracy','EngagedCivilWar','EndedCivilWar',
'EntranceYear','BirthRate','InfantMortality','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular',
'PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore')]

rdwinselect(R=no.window$PointsFromCutpoint,X=X,obsmin=50)







# Figures in the online appendix


# Expanded Blance Plot

pdf("BalancePlotExpanded.pdf", width = 7, height = 8)
BalancePlot(Data=sample, Treat=sample$Treat, Title="Figure 1a. Balance Between the Qualifiers and Non-qualifiers", 
Covariates=c("Irst",'Milex','Milper','Tpop','Upop','BirthRate','DeathRate','InfantMortality','Energy','Imports',
'Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar','EntranceYear','SexRatio',
'LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear','AGGYearBefore','AGG3YearsBefore',
'AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production', 'Military Expenditures', 'Military Personnel', 
'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 'Energy Production', 
'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 
'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", 
"U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 
'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", 
Built.In.Tests=c("PermutationTest"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE,na.rm=TRUE)
dev.off()


# Balance Plot-No Great Powers

pdf("BalancePlotNoGP.pdf", width = 7, height = 8)
BalancePlot(Data= sample[!sample$Country%in%c("United States","Soviet Union"),], Treat= sample[!sample$Country%in%
c("United States","Soviet Union"),]$Treat, Title="Figure 2a. Balance After Dropping the U.S. and Soviet Union", 
Covariates= c("Irst", 'Milex','Milper', 'Tpop','Upop', 'BirthRate','DeathRate', 'InfantMortality',
'Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar',
'EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular',
'PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production',
'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 
'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 
'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 
'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 
'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 
'MIDs Initiated in the 5 Years Before'), Shade.Color="cadetblue2", Built.In.Tests= c("PermutationTest"), Point.Color="black", 
Year.Covariates= c("EntranceYear"), Paired=FALSE)
dev.off()


# Balance Plot for 1 point sample

pdf("BalancePlot1Point.pdf", width = 7, height = 8)
BalancePlot(Data= sample[abs(sample$PointsFromCutpoint)<=1, ], Treat= sample[abs(sample$PointsFromCutpoint)<=1, ]$Treat, 
Title="Figure 3a. Balance for the One-Point Window", Covariates= c("Irst",'Milex','Milper','Tpop','Upop','BirthRate',
'DeathRate','InfantMortality','Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar',
'EndedCivilWar','EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear',
'AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production','Military Expenditures', 
'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 'Infant Mortality', 
'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 'Level of Democracy', 'Great Power Status', 
'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 'Sex Ratio', 'Life Expectancy', 'Median Age', 
"Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  
'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), 
Shade.Color="cadetblue2", Built.In.Tests= c("PermutationTest"), Point.Color="black", Year.Covariates= c("EntranceYear"), 
Paired=TRUE,na.rm=TRUE)
dev.off()



# Linear Regression

# No Controls

model=lm(AGGAfter - AGGBefore ~ Treat, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)

# Military Controls

model=lm(AGGAfter - AGGBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy + Tpop + Upop + CINC + 
EngagedCivilWar + EndedCivilWar + GreatPower + Alliances + USAlly, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military and Economic Controls

model=lm(AGGAfter - AGGBefore ~ Treat + LandArea + Irst + Milper + Milex + Energy + Tpop + Upop + CINC + 
EngagedCivilWar + EndedCivilWar + EntranceYear + GreatPower + Alliances + USAlly + Imports + Exports, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# Military, Economic, and Demographic Controls

model=lm(AGGAfter - AGGBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + 
Democracy + EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + 
Imports + Exports + MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + 
SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Linear Regression Controlling for Previous Outcome

# No Controls

model=lm(AGGAfter ~ Treat + AGGBefore, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military Controls

model=lm(AGGAfter ~ Treat + AGGBefore + LandArea + Irst + Milper + Milex+ Energy + Tpop + Upop + CINC + EngagedCivilWar 
+ EndedCivilWar + GreatPower + Alliances + USAlly, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)



# Military and Economic Controls

model=lm(AGGAfter ~ Treat + AGGBefore + LandArea + Irst + Milper + Milex + Energy + Tpop + Upop + CINC + EngagedCivilWar + 
EndedCivilWar + EntranceYear + GreatPower + Alliances + USAlly + Imports + Exports, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)


# Military, Economic, and Demographic Controls

model=lm(AGGAfter ~ Treat + AGGBefore + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + Democracy + 
EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + Imports + Exports + 
MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)




# Shifting Window- Output is (Window Size, estimate, p-value, SE, and N)

treatment=data[which(data$Treat==1),]
control=data[which(data$Treat==0),]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

for(i in 9:0){

treatment=treatment[which(treatment$PointsFromCutpoint<=i),]
control=control[which(control$PointsFromCutpoint>=-i),]

test1=PermutationTest(treatment$AGGAfter- treatment$AGGBefore,control$AGGAfter- control$AGGBefore,Paired=TRUE,Output="full")


print(c(i, test1))

}



# Shifting Minimal Score- Output is (Minimal Score, estimate, p-value, SE, and N)

treatment=data[which(data$Treat==1),]
treatment=treatment[which(treatment$PointsFromCutpoint<=2),]
control=data[which(data$Treat==0),]
control=control[which(control$PointsFromCutpoint>=-2),]

for(i in 1:10){

control=control[which(treatment$Score>=i),]
treatment=treatment[which(treatment$Score>=i),]


test1=PermutationTest(treatment$AGGAfter- treatment$AGGBefore,control$AGGAfter-control$AGGBefore,Paired=TRUE,Output="full")


print(c(i, test1))

}




# Adjusting the time interval

# Changing the Period Before

for(i in 0:7){
test=PermutationTest(t$AGGAfter-5*rowMeans(t[21:(21-i)]), c$AGGAfter-5*rowMeans(c[21:(21-i)]),Paired=TRUE,Output="full")	
print(c(0.5*(i+1),test))	
}

# Changing the Period After

for(i in 0:7){
test=PermutationTest(5*rowMeans(t[22:(22+i)])- t$AGGBefore, 5*rowMeans(c[22:(22+i)])- c$AGGBefore,Paired=TRUE,Output="full")	
print(c(0.5*(i+1),test))	
}




# Revisionist Disputes

# Changing the Period Before

for(i in 0:7){
test=PermutationTest(t$REVAfter-5*rowMeans(t[43:(43-i)]), c$REVAfter-5*rowMeans(c[43:(43-i)]),Paired=TRUE,Output="full")	
print(c(0.5*(i+1),test))	
}

# Changing the Period After

for(i in 0:7){
test=PermutationTest(5*rowMeans(t[44:(44+i)])- t$REVBefore, 5*rowMeans(c[44:(44+i)])- c$REVBefore,Paired=TRUE,Output="full")	
print(c(0.5*(i+1),test))	
}






# Changing the Cut-point

no.window2=no.window

# I put the ties at -1 and 1 points away from the cutpoint for this part. Not doing this makes the number alignment tricky
no.window2$PointsFromCutpoint[no.window2$PointsFromCutpoint==0&no.window2$Treat==1]=1
no.window2$PointsFromCutpoint[no.window2$PointsFromCutpoint==0&no.window2$Treat==0]=0

Upper=c(-4,-3,-2,-1,1,2,3,4,5,6,7)
Lower=c(-7,-6,-5,-4,-3,-2,-1,1,2,3,4)
Cutpoint=c(-5.5,-4.5,-3.5,-2.5,-1.5,0,1.5,2.5,3.5,4.5,5.5)

for(i in 1:length(Cutpoint)){
no.window3=no.window2
no.window3$Treat=as.numeric(no.window3$PointsFromCutpoint>Cutpoint[i])
output=PermutationTest(AGGAfter-AGGBefore~Treat,no.window3[no.window3$PointsFromCutpoint<=Upper[i]&
no.window3$PointsFromCutpoint>=Lower[i],],Output="full")
print(c(Cutpoint[i],output))}



# Possible SUTVA Violation-Looking at random samples that do not have repeated states

estimatevector=rep(0,1000)
ns=rep(0,1000)
pvalues=rep(0,1000)
for(i in 1:1000){
order=sample(1:71,71)
torder=t[order,]
corder=c[order,]
newsample=rbind(torder[1,],corder[1,])
for(j in 2:71){
	if(torder[j,]$Country %in% newsample$Country){x=1}
	if(corder[j,]$Country %in% newsample$Country){x=1} 
	else{newsample=rbind(newsample,torder[j,],corder[j,])}}
newt=newsample[which(newsample$Treat==1),]	
newc=newsample[which(newsample$Treat==0),]
estimatevector[i]=t.test(newt$AGGAfter- newt$AGGBefore,newc$AGGAfter- newc$AGGBefore,paired=TRUE)$estimate
pvalues[i]= Permutation.Test(newt$AGGAfter- newt$AGGBefore,newc$AGGAfter- newc$AGGBefore,Paired=TRUE)
ns[i]=dim(newt)[1]}
pdf("SUTVA.pdf",height=4,width=7)
ggplot()+geom_histogram(aes(x=estimatevector),origin=-0.1,binwidth=0.02,fill="cornflowerblue")+xlab("Estimate")+
ylab("Frequency")+ xlim(-0.22,0.7)+ geom_vline(xintercept=mean(t$AGGAfter-t$AGGBefore-(c$AGGAfter-c$AGGBefore)),col="black") + 
geom_vline(xintercept=mean(estimatevector),col="red") + theme_bw() + geom_rect(aes(xmin = -0.2, xmax = 0.18, ymin = 70, 
ymax = 85),fill="white",colour="black")+ annotate("text", x = c(-0.17, -0.17, -0.005, 0.0028), y = c(85.1, 79.3, 81, 75), 
label = c("_", "_", "Average of the Estimates", "Estimate from Full Sample"), colour=c("red", "black", "black", "black"), 
size=c(8, 8, 4, 4)) + theme(axis.title=element_text(size=13.7),axis.text=element_text(size=11.7))
dev.off()

 

length(which(pvalues<0.05))/length(pvalues)

min(ns)

max(ns)


# Performance of Qualifiers at the World Cup

pdf("WCRankings.pdf", height=4.3)
ggplot(data=t,aes(x=(t$Rank-1)/(t$NumberOfParticipants-1)))+xlab("Rank")+ylab("Frequency")+
labs(title="Figure 5a. Rankings of the Qualifiers at the World Cup",size=2.5)+geom_density(adjust=0.25,fill="cornflowerblue") 
+ scale_x_continuous(limits=c(0,1),breaks = c(0.0,1), labels = c("First Place","Last Place"))+ theme_bw() + 
theme(axis.title=element_text(size=13.7),axis.text=element_text(size=10.7))
dev.off()


# Treatment Effect Comparison

# These treatment effects were taken from the papers listed in Section 1.8 of the online appendix

names=c("Revolution","Arms Transfer","Neutrality Pact","Offensive Alliance","    Leader from Rebel Group","World Cup",
"Leader from Military","Autocratic Regime Change")

treatment.effects=c(74.29,59.6,57,47,43,33.3,24.6,24)
position=1:length(treatment.effects)

teffects=data.frame(position,treatment.effects)

pdf("TEComp.pdf")
ggplot(teffects, aes(position, treatment.effects)) + geom_point()  + theme_bw() + scale_x_continuous(breaks = position, 
labels = names) + xlab("Variable") + ylab("Increase in Probability of Dispute\nInitiation (from Baseline)") + 
scale_y_continuous(breaks = seq(0,100,by=20), labels = c("0%","20%","40%","60%","80%","100%"),limits=c(0,105))+
geom_hline(yintercept=0)+ labs(title="Figure 6a. Comparing the Effect of the World\nCup to Other Estimated Treatment Effects")+ 
theme(plot.title = element_text(lineheight=.8, face="bold",size=18), axis.text.x  = element_text(angle=90, vjust=0.5, hjust=1, 
size=12), axis.title= element_text(size=13, face="bold"))+geom_point(data=subset(teffects,treatment.effects==33.3),colour="red")

dev.off()



# Further Analysis

# Testing Which Countries Have the Largest Impact on the Outcomes

sample=data[which(data$Use==1),]

c=sample[which(sample$Treat==0),]
t=sample[which(sample$Treat==1),]


p=rep(0,length(unique(sample$Country)))
est=rep(0,length(unique(sample$Country)))

for(i in 1:length(unique(sample$Country))){
samplex=sample[-which(sample$Country==unique(sample$Country)[i]),]
p[i]=PermutationTest(AGGAfter-AGGBefore~Treat,samplex)
est[i]=t.test(AGGAfter-AGGBefore~Treat,samplex)$est[2]-t.test(AGGAfter-AGGBefore~Treat,samplex)$est[1]		
}

unique(sample$Country)[order(-p)]
unique(sample$Country)[order(est)]



# Dropping Countries

countries=unique(sample$Country)
p=rep(0,length(countries))
est=rep(0,length(countries))
se=rep(0,length(countries))

for(i in 1:length(countries)){
new.sample=sample[sample$Country!=countries[i],]	
out=t.test(AGGAfter-AGGBefore~Treat,new.sample)
p[i]=PermutationTest(AGGAfter-AGGBefore~Treat,new.sample)
est[i]=out$estimate[2]-out$estimate[1]	
se[i]=abs(out$conf.int[1]-out$conf.int[2])/(2*1.96)
}

cbind(countries[order(est)],est[order(est)],se[order(est)],p[order(est)])
