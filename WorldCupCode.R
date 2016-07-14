# Replication code for "Nationalism and Interstate Conflict: A Regression Discontinuity Analysis"

# Last Updated: Dec 22, 2015

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

# We can now subset to the countries that were within two points of making or missing qualification

sample=data[which(data$Use==1),]

# We can then divide these countries into the treatment and control groups

c=sample[which(sample$Treat==0),]
t=sample[which(sample$Treat==1),]

# We can now read in the functions that will make the figures from the paper. These include BalancePlot(), RDPlot(), and PermutationTest()

source_url("https://raw.githubusercontent.com/AndrewBertoli/World-Cup-Code/master/WorldCupFunctions.R")



# Figure 1-Balance Plot

pdf("BalancePlot.pdf", width = 7, height = 5)
BalancePlot(Data=sample, Treat=sample$Treat, Title="Figure 1. Balance Between the Qualifiers and Non-qualifiers", 
Covariates=c('Tpop','Upop','Imports','Exports','CINC','Democracy','EngagedCivilWar','EndedCivilWar','EntranceYear',
'BirthRate','InfantMortality','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular','PrevAppear',
'AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Total Population', 'Urban Population',
'Imports', 'Exports', 'Material Power Score', 'Level of Democracy', 'Engaged in Civil War', 'Resolved Civil War', 
'Year of State Formation', 'Birth Rate', 'Infant Mortality','Life Expectancy', 'Median Age', "Number of Alliances", 
"U.S. Ally", "Soccer Most Popular Sport", 'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 
'MIDs Initiated in the 3 Years Before', 'MIDs Initiated in the 5 Years Before'), Shade.Color= "cadetblue2", 
Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE,na.rm=TRUE)
dev.off()



# Figure 2-Time Series

pdf("MIDComparison.pdf")
tAGG=apply(t[,11:31], 2, sum)
cAGG=apply(c[,11:31], 2, sum)
Year=seq(-5.5,4.5,by=0.5)
tsdata=data.frame(c(Year,Year),c(tAGG,cAGG),c(rep(1,20),rep(0,20)))
colnames(tsdata)=c("Year","Aggression","Treat")
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

# I define AGG After as the number of militarized interstate disputes initiated in the period ranging from qualification to the second year following the World Cup and AGG Before as the period of the same length prior to qualification. I also use the same time intervals for revisionist disputes.

data$AGGAfter=with(data,  AGGP.5 + AGGP1 + AGGP1.5 + AGGP2 + AGGP2.5)

data$AGGBefore=with(data, AGG0 + AGGM.5 + AGGM1 + AGGM1.5 + AGGM2)

data$REVAfter=with(data,  REVP.5 + REVP1 + REVP1.5 + REVP2 + REVP2.5)

data$REVBefore=with(data, REV0 + REVM.5 + REVM1 + REVM1.5 + REVM2)

sample=data[which(data$Use==1),]

c=sample[which(sample$Treat==0),]
t=sample[which(sample$Treat==1),]


# Dif-in-dif t-test

t.test(AGGAfter-AGGBefore~Treat,sample,paired=TRUE)


# Countries Where Soccer Is the Most Popular Sport

t.test(AGGAfter-AGGBefore ~ Treat, sample[sample$SoccerMostPopular==1,])


# Countries Where Soccer Is Not the Most Popular Sport

t.test(AGGAfter-AGGBefore ~ Treat, sample[sample$SoccerMostPopular==0,])


# Countries that Qualified or Missed By 1 point or Less

t.test(AGGAfter-AGGBefore~Treat,sample[abs(sample$PointsFromCutpoint)<=1,],paired=TRUE)

# Countries that Qualified or Missed By 3 points or Less-We must go back to data to add the countries that were 3 points away

treatment=data[which(data$Treat==1),]
control=data[which(data$Treat==0),]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

no.window=rbind(treatment,control)

t.test(AGGAfter-AGGBefore~Treat,no.window[abs(no.window$PointsFromCutpoint)<=3,],paired=TRUE)

# Removing Ties

t.test(AGGAfter-AGGBefore~Treat,sample[sample$PointsFromCutpoint!=0,],paired=TRUE)



# Linear Regression with Robust Standard Errors

model=lm(AGGAfter - AGGBefore ~ Treat + LandArea + Irst + Milper + Milex+ Energy +Tpop + Upop + CINC + 
Democracy + EngagedCivilWar + EndedCivilWar + PrevAppear + GreatPower + Alliances + USAlly + EntranceYear + 
Imports + Exports + MedianAge + LifeExpectancy + InfantMortality + SexRatio + DeathRate + BirthRate + 
SoccerMostPopular, sample)

model$newse<-vcovHC(model,type="HC1")
coeftest(model,model$newse)

# Permutation Inference (with mean as test statistic)

PermutationTest(t$AGGAfter-t$AGGBefore, c$AGGAfter-c$AGGBefore, Simulations=500000, Paired=TRUE)

# t-test on Post-Treatment outcome alone (not Difference-in-Differences)

t.test(AGGAfter~Treat,sample,paired=TRUE)


# Wilcoxon signed-rank test

wilcox.test(t$AGGAfter-t$AGGBefore,c$AGGAfter-c$AGGBefore,paired=TRUE)

# Dummy for an increase

t.test(c(with(t,AGGAfter-AGGBefore))>0,c(with(c,AGGAfter-AGGBefore))>0,paired=TRUE)

# Excluding Great Powers

t.test(AGGAfter-AGGBefore~Treat, sample[sample$GreatPower==0,])


# Revisionist Disputes

t.test(REVAfter-REVBefore~Treat,sample,paired=TRUE)

# Disputes That Involved the Use of Force

t.test(ForceAGGAfter-ForceAGGBefore~Treat,sample,paired=TRUE)

# Disputes That Involved a Direct Attack, a Clash, or the Start of Interstate War

t.test(AttackAGGAfter-AttackAGGBefore~Treat,sample,paired=TRUE)






# Figure 3-Continuity Graphs

# I set the ties at 0.0001 and -0.0001, instead of counting both the winners and losers as 0.  

no.window[no.window$PointsFromCutpoint==0&no.window$Treat==0,]$PointsFromCutpoint=-0.1
no.window[no.window$PointsFromCutpoint==0&no.window$Treat==1,]$PointsFromCutpoint=0.1

# Because the score is highly discrete, the normal bandwidth procedures do not work on this data. I set the bandwidths at 4, although the results are significant for any bandwidth greater than or equal to 1.

pdf("WCCont.pdf", height=4.5, width=9.5)

op=par(mfrow=c(1,3), mar=c(4,4.5,3,0),oma=c(0,1,2,1))

bw=rdbwselect(no.window$AGGBefore,no.window$PointsFromCutpoint,bwselect="IK")[[3]][1]

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGBefore,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), Main="Aggression Before", 
xlab="Points Above/Below Cut-Point",ylab="Militarized Interstate Disputes Initiated",Kernel="Triangular",Bandwidth=bw, 
Window=c(-2.5,2.5),NBoots=10000,cex.main=1.8,cex.lab=1.5)

bw=rdbwselect(no.window$AGGAfter,no.window$PointsFromCutpoint,bwselect="IK")[[3]][1]

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGAfter,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), Main="Aggression After",
xlab="Points Above/Below Cut-Point", ylab="",Kernel="Triangular", Bandwidth=bw, Window=c(-2.5,2.5),NBoots=10000,cex.main=1.8,
cex.lab=1.5,yaxt='n')

bw=rdbwselect(no.window$AGGAfter-no.window$AGGBefore,no.window$PointsFromCutpoint,bwselect="IK")[[3]][1]

RDPlot(X=no.window$PointsFromCutpoint,Y=no.window$AGGAfter-no.window$AGGBefore,xlim=c(-3.6,3.6),ylim=c(-1.5,1.5), 
Main="Change in Aggression", xlab="Points Above/Below Cut-Point", ylab="", Kernel="Triangular", Bandwidth=bw, 
Window=c(-2.5,2.5),NBoots=10000,cex.main=1.8,cex.lab=1.5,yaxt='n')

title("Figure 3. Change in Aggression for the World Cup",outer=TRUE,cex.main=2.5)

dev.off()



# Conflicts between countries that played against each other

dyads=read.csv("https://raw.githubusercontent.com/AndrewBertoli/Nationalism-RD-Replication-Files/master/Dyads.csv",stringsAsFactors=FALSE)

t.test(Disputes-PreviousDisputes~Played,dyads)
t.test(Disputes>0~Played,dyads)

Played_Disputes=mean(dyads$Disputes[dyads$Played==1]>0)
Played_PrevDisputes=mean(dyads$PreviousDisputes[dyads$Played==1]>0)

Other_Disputes=mean(dyads$Disputes[dyads$Played==0]>0)
Other_PrevDisputes=mean(dyads$PreviousDisputes[dyads$Played==0]>0)

pdf("WCDyadsGraph.pdf",height=4.2, width=7)
ggplot() + geom_line(aes(y=c(Played_PrevDisputes,Played_Disputes),x=c(0,1)), colour="Blue", show_guide=TRUE) + 
geom_line(aes(y=c(Other_PrevDisputes,Other_Disputes),x=c(0,1)) , colour="gray44", show_guide=TRUE) + 
scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + 
scale_y_continuous(breaks = c(0.37,0.38,0.39,.4,.41,.42,.43), labels = c("37%","38%","39%","40%","41%","42%","43%"), 
limits=c(0.37,0.43)) + labs(title="Figure 4: Conflict Between Countries That Played at the World Cup") + xlab("Time Period") 
+ ylab("Probability of Military Dispute")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=15,
hjust=.97),axis.title = element_text(size=14),axis.text = element_text(size=11.5))+annotate("text", x=0.52, y=0.408, angle=22.4,
label="Participants That Played Each Other",color="blue", size=5)+ annotate("text", x=0.6, y=0.38402, angle=-8.1, 
label="Participants That Did Not Play Each Other",color="gray44", size=5) + annotate("text", x=1, y=0.4281, label="0.424*",
color="black", size=4.5)+ annotate("text", x=1, y=0.3728, label="0.376",color="black", size=4.5) + annotate("text", x=0, 
y=0.393, label="0.390",color="black", size=4.5)+ annotate("text", x=0, y=0.3812, label="0.384",color="black", size=4.5)
dev.off()



# Changes in Military Participation and Military Expenditures

# Since we have data on the total population size of countries, we can calculate the percentage of the population 
# that participated in the military the year before and the year after the World Cup. We can then do a difference-in-differences 
# t-test.  

t.test(MilperPercentAfter-Milper/Tpop>0~Treat,sample)

t.test(MilperPercentAfter-Milper/Tpop>0~Treat,sample,paired=TRUE)$p.value


# Since we don't have reliable GDP data, we can't do the same test for military expenditures. So I just look at the 
# percentage that military expenditures increased between the year before and year after the World Cup.

t.test(MilexAfter/Milex~Treat,sample[sample$Milex>0,])




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
Built.In.Tests=c("T.Test"), Point.Color= "black", Year.Covariates=c("EntranceYear"),Paired=TRUE,na.rm=TRUE)
dev.off()


# Balance Plot-No Great Powers

pdf("BalancePlotNoGP.pdf", width = 7, height = 8)
BalancePlot(Data= sample[sample$GreatPower==0,], Treat= sample[sample$GreatPower==0, ]$Treat, 
Title="Figure 2a. Balance After Dropping the U.S. and Soviet Union", 
Covariates= c("Irst", 'Milex','Milper', 'Tpop','Upop', 'BirthRate','DeathRate', 'InfantMortality',
'Energy','Imports','Exports','LandArea','CINC','Democracy','GreatPower','EngagedCivilWar','EndedCivilWar',
'EntranceYear','SexRatio','LifeExpectancy','MedianAge','Alliances','USAlly','SoccerMostPopular',
'PrevAppear','AGGYearBefore','AGG3YearsBefore','AGG5YearsBefore'), Names.To.Print=c('Iron and Steel Production',
'Military Expenditures', 'Military Personnel', 'Total Population', 'Urban Population', 'Birth Rate', 'Death Rate', 
'Infant Mortality', 'Energy Production', 'Imports', 'Exports', 'Land Area', 'Material Power Score', 
'Level of Democracy', 'Great Power Status', 'Engaged in Civil War', 'Resolved Civil War', 'Year of State Formation', 
'Sex Ratio', 'Life Expectancy', 'Median Age', "Number of Alliances", "U.S. Ally", "Soccer Most Popular Sport", 
'Appearance at Previous World Cup',  'MIDs Initiated in the Year Before', 'MIDs Initiated in the 3 Years Before', 
'MIDs Initiated in the 5 Years Before'), Shade.Color="cadetblue2", Built.In.Tests= c("T.Test"), Point.Color="black", 
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
Shade.Color="cadetblue2", Built.In.Tests= c("T.Test"), Point.Color="black", Year.Covariates= c("EntranceYear"), 
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




# Shifting Window- Output is (Window Size, p-value, and N)

treatment=data[which(data$Treat==1),]
control=data[which(data$Treat==0),]
control=control[which(treatment$Score>=5),]
treatment=treatment[which(treatment$Score>=5),]

for(i in 9:0){

treatment=treatment[which(treatment$PointsFromCutpoint<=i),]
control=control[which(control$PointsFromCutpoint>=-i),]

test1=t.test(treatment$AGGAfter- treatment$AGGBefore,control$AGGAfter- control$AGGBefore,paired=TRUE)


print(c(i, test1$estimate,test1$p.value,length(treatment[,1])))

}



# Shifting Minimal Score- Output is (Minimal Score, p-value, and N)

treatment=data[which(data$Treat==1),]
treatment=treatment[which(treatment$PointsFromCutpoint<=2),]
control=data[which(data$Treat==0),]
control=control[which(control$PointsFromCutpoint>=-2),]

for(i in 1:10){

control=control[which(treatment$Score>=i),]
treatment=treatment[which(treatment$Score>=i),]


test1=t.test(treatment$AGGAfter- treatment$AGGBefore,control$AGGAfter-control$AGGBefore,paired=TRUE)


print(c(i, test1$estimate,test1$p.value, length(treatment[,1])))

}




# Adjusting the time interval

# Changing the Period After

for(i in 0:7){
test=t.test(5*rowMeans(t[23:(23+i)])- t$AGGBefore, 5*rowMeans(c[23:(23+i)])- c$AGGBefore,paired=TRUE)	
print(c(0.5*(i+1),test$estimate,test$p.value))	
}

# Changing the Period Before

for(i in 0:7){
test=t.test(t$AGGAfter-5*rowMeans(t[22:(22-i)]), c$AGGAfter-5*rowMeans(c[22:(22-i)]),paired=TRUE)	
print(c(0.5*(i+1),test$estimate,test$p.value))	
}



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
pvalues[i]= t.test(newt$AGGAfter- newt$AGGBefore,newc$AGGAfter- newc$AGGBefore,paired=TRUE)$p.value
ns[i]=dim(newt)[1]}
pdf("SUTVA.pdf",height=4,width=7)
ggplot()+geom_histogram(aes(x=estimatevector),origin=-0.1,binwidth=0.02,fill="cornflowerblue")+xlab("Estimate")+ylab("Frequency")+ 
xlim(-0.22,0.7)+ geom_vline(xintercept=mean(t$AGGAfter-t$AGGBefore-(c$AGGAfter-c$AGGBefore)),col="black") + 
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

names=c("Revolution","Arms Transfer","Neutrality Pact","Offensive Alliance","    Leader from Rebel Group",
"Leader Backed By Military","World Cup","Autocratic Regime Change","Leader from Military")

treatment.effects=c(74.29,59.6,57,47,43,41,33.3,24,18.5)
position=1:length(treatment.effects)

teffects=data.frame(position,treatment.effects)

pdf("TEComp.pdf")
ggplot(teffects[-7,], aes(position, treatment.effects)) + geom_point()  + theme_bw() + scale_x_continuous(breaks = position, 
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
p[i]=t.test(AGGAfter-AGGBefore~Treat,samplex)$p.value
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
p[i]=out$p.value	
est[i]=out$estimate[2]-out$estimate[1]	
se[i]=abs(out$conf.int[1]-out$conf.int[2])/(2*1.96)
}

cbind(countries[order(est)],est[order(est)],se[order(est)],p[order(est)])
