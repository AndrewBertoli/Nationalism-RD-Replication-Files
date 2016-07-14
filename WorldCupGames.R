library(ggplot2)

setwd("/Users/andrewbertoli/Dropbox/WCNaturalExperiment")
data=read.csv("WorldCupGames.csv",stringsAsFactors=FALSE)

disputes=read.csv("MIDB_4.0.csv",stringsAsFactors=FALSE)

# disputes=disputes[disputes$Orig==1,]

source("NameChange.R")

mids=function(v){

disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]

Year=as.numeric(v[1])
Month=as.numeric(v[13])
Day=as.numeric(v[12])	
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay>=Day),
	which(disp$StYear==Year&disp$StMon>Month),
	which(disp$StYear==(Year+1)),
	which(disp$StYear==(Year+2)&disp$StMon<Month),
	which(disp$StYear==(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}

outcome=apply(data,1,mids)







p.mids=function(v){

Year=as.numeric(v[1])
Month=as.numeric(v[13])
Day=as.numeric(v[12])	

disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay<=Day),
	which(disp$StYear==Year&disp$StMon<Month),
	which(disp$StYear==(Year-1)),
	which(disp$StYear==(Year-2)&disp$StMon>Month),
	which(disp$StYear==(Year-2)&disp$StMon==Month&disp$StDay>Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}

prevoutcome=apply(data,1,p.mids)








years=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)

month=rep(6,19)

other.pairs=matrix(0,nrow=1,ncol=3)
for(k in 1:length(years)){

countries=unique(c(data[data$Year==years[k],]$Country1,data[data$Year==years[k],]$Country2))

new.pairs=t(combn(countries,2))
new.pairs=cbind(years[k],new.pairs)
other.pairs=rbind(other.pairs,new.pairs)	
}

other.pairs=other.pairs[-1,]


for(i in 1:nrow(data)){
other.pairs=other.pairs[-c(which(other.pairs[,2]==data$Country1[i]&other.pairs[,3]==data$Country2[i]&as.numeric(other.pairs[,1])==data$Year[i]),which(other.pairs[,2]==data$Country2[i]&other.pairs[,3]==data$Country1[i]&as.numeric(other.pairs[,1])==data$Year[i])),]}



mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1	

disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay>=Day),
	which(disp$StYear==Year&disp$StMon>Month),
	which(disp$StYear==(Year+1)),
	which(disp$StYear==(Year+2)&disp$StMon<Month),
	which(disp$StYear==(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}


all.mids=apply(other.pairs,1,mids)

p.mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1
	
disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay<=Day),
	which(disp$StYear==Year&disp$StMon<Month),
	which(disp$StYear==(Year-1)),
	which(disp$StYear==(Year-2)&disp$StMon>Month),
	which(disp$StYear==(Year-2)&disp$StMon==Month&disp$StDay>Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}





all.pmids=apply(other.pairs,1,p.mids)


t=sum(outcome>0)
c=sum(all.mids>0)
pt=sum(prevoutcome>0)
pc=sum(all.pmids>0)

sum(outcome)/sum(prevoutcome)

t/pt

c/pc

t.test(outcome>0,all.mids>0)


Year=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)
Month=6
Day=1

disp=disputes	
allprevdisp=disp[c(
	which(disp$StYear%in%Year&disp$StMon==Month&disp$StDay<=Day),
	which(disp$StYear%in%Year&disp$StMon<Month),
	which(disp$StYear%in%(Year-1)),
	which(disp$StYear%in%(Year-2)&disp$StMon>Month),
	which(disp$StYear%in%(Year-2)&disp$StMon==Month&disp$StDay>Day)	
	),]		

num_all_prev=nrow(allprevdisp)-sum(all.pmids)
alldisp=disp[c(
	which(disp$StYear%in%Year&disp$StMon==Month&disp$StDay>Day),
	which(disp$StYear%in%Year&disp$StMon>=Month),
	which(disp$StYear%in%(Year+1)),
	which(disp$StYear%in%(Year+2)&disp$StMon<Month),
	which(disp$StYear%in%(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		

num_all=nrow(alldisp)-sum(all.mids)
















Year=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)

disp=disputes

disputes_with_wc_part=c()

for(i in 1:length(Year)){
index1=disp[c(which(disp$StYear>Year[i]-1&disp$StYear<Year[i]+1),which(disp$StYear==Year[i]-2&disp$StMon>=6),which(disp$StYear==Year[i]+2&disp$StMon<6)),]$DispNum3			
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

num_all_prev=num_all_prev-sum(prevoutcome)







-sum(all.pmids)-sum(prevoutcome)


alldisp=disp[c(
	which(disp$StYear%in%Year&disp$StMon==Month&disp$StDay>Day),
	which(disp$StYear%in%Year&disp$StMon>=Month),
	which(disp$StYear%in%(Year+1)),
	which(disp$StYear%in%(Year+2)&disp$StMon<Month),
	which(disp$StYear%in%(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		



num_all=length(unique(alldisp$DispNum3))

num_all=num_all-sum(outcome)

num_all/num_all_prev

# Number of all disputes increased by 6.2%.


num_all=nrow(alldisp)-sum(all.mids)-sum(outcome)

num_all/num_all_prev

num_all=nrow(alldisp>0)-sum(all.mids>0)-sum(outcome>0)



setwd("/Users/andrewbertoli/Desktop/Conquest/MIDs")

pdf("WCDyadsGraph.pdf",height=4, width=7.2)
ggplot() + geom_line(aes(y=c(sum(prevoutcome),sum(outcome)),x=c(0,1)), colour="Blue", show_guide=TRUE) + geom_line(aes(y=c(sum(prevoutcome>0),sum(outcome>0)),x=c(0,1)) , colour="Blue", show_guide=TRUE) + scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + ylim(30,60) + xlab("Time Period") + ylab("")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=15,hjust=.97),axis.title = element_text(size=14),axis.text = element_text(size=11.5))+


 annotate("text", x=0.49, y=49.2, angle=24, label="Total Militarized Interstate Dipsutes",color="Blue", size=5)+ annotate("text", x=0.51, y=42, angle=14.8, label="Pairs with at Least One Dispute",color="blue", size=5) + 
 
 annotate("text", x=1, y=60, label="59",color="black", size=4.5)+ annotate("text", x=1, y=45.8, label="47",color="black", size=4.5) + annotate("text", x=0, y=33, label="34",color="black", size=4.5)+ annotate("text", x=0, y=38.2, label="37",color="black", size=4.5)
dev.off()




pdf("WCGamesGraph.pdf",height=4, width=7.2)
ggplot() + geom_line(aes(y=c(sum(prevoutcome),sum(outcome)),x=c(0,1)), colour="Blue", show_guide=TRUE) + geom_line(aes(y=c(sum(prevoutcome>0),sum(outcome>0)),x=c(0,1)) , colour="Blue", show_guide=TRUE) + scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + ylim(30,60) + xlab("Time Period") + ylab("")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=15,hjust=.97),axis.title = element_text(size=14),axis.text = element_text(size=11.5))+


 annotate("text", x=0.49, y=49.2, angle=24, label="Total Militarized Interstate Dipsutes",color="Blue", size=5)+ annotate("text", x=0.51, y=42, angle=14.8, label="Pairs with at Least One Dispute",color="blue", size=5) + 
 
 annotate("text", x=1, y=60, label="59",color="black", size=4.5)+ annotate("text", x=1, y=45.8, label="47",color="black", size=4.5) + annotate("text", x=0, y=33, label="34",color="black", size=4.5)+ annotate("text", x=0, y=38.2, label="37",color="black", size=4.5)
dev.off()








pdf("WCDyadsGraphSlides.pdf",height=4.2, width=5.5)
ggplot() + geom_line(aes(y=c(sum(prevoutcome),sum(outcome)),x=c(0,1)), colour="Blue", show_guide=TRUE) + geom_line(aes(y=c(sum(prevoutcome>0),sum(outcome>0)),x=c(0,1)) , colour="Blue", show_guide=TRUE) + scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + ylim(30,60) + labs(title="Figure 4: Conflict Between Countries That Played at the World Cup") + xlab("Time Period") + ylab("")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=15,hjust=.97),axis.title = element_text(size=14),axis.text = element_text(size=11.5))+


 annotate("text", x=0.49, y=49.2, angle=24, label="Total Militarized Interstate Dipsutes",color="blue", size=5)+ annotate("text", x=0.51, y=42, angle=14.8, label="Pairs with at Least One Dispute",color="Blue", size=5) + 
 
 annotate("text", x=1, y=60, label="59",color="black", size=4.5)+ annotate("text", x=1, y=46, label="47",color="black", size=4.5) + annotate("text", x=0, y=33, label="34",color="black", size=4.5)+ annotate("text", x=0, y=38.2, label="37",color="black", size=4.5)
dev.off()




# All countries that went to the World Cup at some point


years=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)

month=rep(6,19)

other.pairs=matrix(0,nrow=1,ncol=3)
for(k in 1:length(years)){

countries=unique(c(data$Country1,data$Country2))

new.pairs=t(combn(countries,2))
new.pairs=cbind(years[k],new.pairs)
other.pairs=rbind(other.pairs,new.pairs)	
}

other.pairs=other.pairs[-1,]


mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1	

disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay>=Day),
	which(disp$StYear==Year&disp$StMon>Month),
	which(disp$StYear==(Year+1)),
	which(disp$StYear==(Year+2)&disp$StMon<Month),
	which(disp$StYear==(Year+2)&disp$StMon==Month&disp$StDay<Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}


all.mids=apply(other.pairs,1,mids)

p.mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1
	
disp=disputes[disputes$DispNum3%in%disputes[disputes$StAbb==v[2],]$DispNum3&disputes$DispNum3%in%disputes[disputes$StAbb==v[3],]$DispNum3&disputes$StAbb%in%v[2:3],]
	
disp=disp[c(
	which(disp$StYear==Year&disp$StMon==Month&disp$StDay<=Day),
	which(disp$StYear==Year&disp$StMon<Month),
	which(disp$StYear==(Year-1)),
	which(disp$StYear==(Year-2)&disp$StMon>Month),
	which(disp$StYear==(Year-2)&disp$StMon==Month&disp$StDay>Day)	
	),]		
a=which(disp$StAbb%in%c(v[2])&disp$SideA==1)
b=which(disp$StAbb%in%c(v[3])&disp$SideA==0)
c=which(disp$StAbb%in%c(v[2])&disp$SideA==0)
d=which(disp$StAbb%in%c(v[3])&disp$SideA==1)

dispute.numbers=c(unique(disp[a,]$DispNum3,disp[b,]$DispNum3),unique(disp[c,]$DispNum3,disp[d,]$DispNum3))

return(length(dispute.numbers))

}





all.pmids=apply(other.pairs,1,p.mids)


t=sum(outcome>0)
c=sum(all.mids>0)
pt=sum(prevoutcome>0)
pc=sum(all.pmids>0)

sum(outcome)/sum(prevoutcome)

t/pt

c/pc

t.test(outcome>0,all.mids>0)




























setwd("/Users/andrewbertoli/Downloads")
disputes=read.csv("dyadmid20.csv",stringsAsFactors=FALSE)



setwd("/Users/andrewbertoli/Dropbox/WCNaturalExperiment")
source("NameChange2.R")

mids=function(v){

disp1=disputes[disputes$NAMEA==as.character(v[2])&disputes$NAMEB==as.character(v[3]),]
disp2=disputes[disputes$NAMEA==as.character(v[3])&disputes$NAMEB==as.character(v[2]),]

disp=rbind(disp1,disp2)

Year=as.numeric(v[1])
Month=as.numeric(v[13])
Day=as.numeric(v[12])	
	
disp=disp[c(
	which(disp$STRTYR==Year&disp$STRTMNTH==Month&disp$STRTDAY>=Day),
	which(disp$STRTYR==Year&disp$STRTMNTH>Month),
	which(disp$STRTYR==(Year+1)),
	which(disp$STRTYR==(Year+2)&disp$STRTMNTH<Month),
	which(disp$STRTYR==(Year+2)&disp$STRTMNTH==Month&disp$STRTDAY<Day)	
	),]		

return(nrow(disp))

}

outcome=apply(data,1,mids)







p.mids=function(v){

Year=as.numeric(v[1])
Month=as.numeric(v[13])
Day=as.numeric(v[12])	

disp1=disputes[disputes$NAMEA==as.character(v[2])&disputes$NAMEB==as.character(v[3]),]
disp2=disputes[disputes$NAMEA==as.character(v[3])&disputes$NAMEB==as.character(v[2]),]

disp=rbind(disp1,disp2)
	
disp=disp[c(
	which(disp$STRTYR==Year&disp$STRTMNTH==Month&disp$STRTDAY<=Day),
	which(disp$STRTYR==Year&disp$STRTMNTH<Month),
	which(disp$STRTYR==(Year-1)),
	which(disp$STRTYR==(Year-2)&disp$STRTMNTH>Month),
	which(disp$STRTYR==(Year-2)&disp$STRTMNTH==Month&disp$STRTDAY>Day)	
	),]		

return(nrow(disp))

}

prevoutcome=apply(data,1,p.mids)








years=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010)

month=rep(6,19)

other.pairs=matrix(0,nrow=1,ncol=3)
for(k in 1:length(years)){

countries=unique(c(data[data$Year==years[k],]$Country1,data[data$Year==years[k],]$Country2))

new.pairs=t(combn(countries,2))
new.pairs=cbind(years[k],new.pairs)
other.pairs=rbind(other.pairs,new.pairs)	
}

other.pairs=other.pairs[-1,]


for(i in 1:nrow(data)){
other.pairs=other.pairs[-c(which(other.pairs[,2]==data$Country1[i]&other.pairs[,3]==data$Country2[i]&as.numeric(other.pairs[,1])==data$Year[i]),which(other.pairs[,2]==data$Country2[i]&other.pairs[,3]==data$Country1[i]&as.numeric(other.pairs[,1])==data$Year[i])),]}



mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1	

disp1=disputes[disputes$NAMEA==as.character(v[2])&disputes$NAMEB==as.character(v[3]),]
disp2=disputes[disputes$NAMEA==as.character(v[3])&disputes$NAMEB==as.character(v[2]),]

disp=rbind(disp1,disp2)
	
disp=disp[c(
	which(disp$STRTYR==Year&disp$STRTMNTH==Month&disp$STRTDAY>=Day),
	which(disp$STRTYR==Year&disp$STRTMNTH>Month),
	which(disp$STRTYR==(Year+1)),
	which(disp$STRTYR==(Year+2)&disp$STRTMNTH<Month),
	which(disp$STRTYR==(Year+2)&disp$STRTMNTH==Month&disp$STRTDAY<Day)	
	),]		

return(nrow(disp))

}


all.mids=apply(other.pairs,1,mids)

p.mids=function(v){

Year=as.numeric(v[1])
Month=6
Day=1
	
disp1=disputes[disputes$NAMEA==as.character(v[2])&disputes$NAMEB==as.character(v[3]),]
disp2=disputes[disputes$NAMEA==as.character(v[3])&disputes$NAMEB==as.character(v[2]),]

disp=rbind(disp1,disp2)
	
disp=disp[c(
	which(disp$STRTYR==Year&disp$STRTMNTH==Month&disp$STRTDAY<=Day),
	which(disp$STRTYR==Year&disp$STRTMNTH<Month),
	which(disp$STRTYR==(Year-1)),
	which(disp$STRTYR==(Year-2)&disp$STRTMNTH>Month),
	which(disp$STRTYR==(Year-2)&disp$STRTMNTH==Month&disp$STRTDAY>Day)	
	),]		

return(nrow(disp))

}





all.pmids=apply(other.pairs,1,p.mids)


t=mean(outcome>0)
c=mean(all.mids>0)
pt=mean(prevoutcome>0)
pc=mean(all.pmids>0)

t.test(outcome>0,all.mids>0)






ts_t_outcomes=rep(NA,20)
ts_c_outcomes=rep(NA,20)

ts_t_outcomes[20]=t
ts_c_outcomes[20]=c

ts_t_outcomes[19]=pt
ts_c_outcomes[19]=pc

for(i in 18:1){
disputes$StYear=disputes$StYear+2
ts_t_outcomes[i]=mean(apply(data,1,p.mids))
ts_c_outcomes[i]=mean(apply(other.pairs,1,p.mids))
}

pdf("WCDyads.pdf")
ggplot() + geom_line(aes(y=c(pt,t),x=c(0,1)), colour="Blue", show_guide=TRUE) + geom_line(aes(y=c(pc,c),x=c(0,1)) , colour="gray44", show_guide=TRUE) + scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + scale_y_continuous(breaks = c(0.35,0.4,0.45), labels = c("35%","40%","45%"), limits=c(0.35,0.45)) + labs(title="Figure 4: Conflict Between Countries That Played at the World Cup") + xlab("Time Period") + ylab("Probability of Militarized Interstate Dispute")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=14),axis.title = element_text(size=14),axis.text = element_text(size=12.5))+


 annotate("text", x=0.5, y=0.4065, angle=24.9, label="Participants That Played Each Other",color="blue", size=4.5)+ annotate("text", x=0.56, y=0.384, angle=-9.1, label="Participants That Did Not Play Each Other",color="gray44", size=4.5) + 
 
 annotate("text", x=1, y=0.427, label="0.424*",color="black", size=4.5)+ annotate("text", x=1, y=0.3738, label="0.376",color="black", size=4.5) + annotate("text", x=0, y=0.392, label="0.390",color="black", size=4.5)+ annotate("text", x=1, y=0.3738, label="0.376",color="black", size=4.5) + annotate("text", x=0, y=0.3822, label="0.384",color="black", size=4.5)
dev.off()




setwd("/Users/andrewbertoli/Desktop/Conquest/MIDs")

pdf("WCDyadsGraph.pdf",height=4.2, width=7)
ggplot() + geom_line(aes(y=c(pt,t),x=c(0,1)), colour="Blue", show_guide=TRUE) + geom_line(aes(y=c(pc,c),x=c(0,1)) , colour="gray44", show_guide=TRUE) + scale_x_continuous(breaks = c(0,1), labels = c("Two Years Before","Two Years After"),limits=c(-0.07,1.07)) + scale_y_continuous(breaks = c(0.37,0.38,0.39,.4,.41,.42,.43), labels = c("37%","38%","39%","40%","41%","42%","43%"), limits=c(0.37,0.43)) + labs(title="Figure 4: Conflict Between Countries That Played at the World Cup") + xlab("Time Period") + ylab("Probability of Military Dispute")+ theme_bw() + theme(plot.title = element_text(lineheight=.8, face="bold",size=15,hjust=.97),axis.title = element_text(size=14),axis.text = element_text(size=11.5))+


 annotate("text", x=0.52, y=0.408, angle=22.4, label="Participants That Played Each Other",color="blue", size=5)+ annotate("text", x=0.6, y=0.38402, angle=-8.1, label="Participants That Did Not Play Each Other",color="gray44", size=5) + 
 
 annotate("text", x=1, y=0.429, label="0.424*",color="black", size=4.5)+ annotate("text", x=1, y=0.3728, label="0.376",color="black", size=4.5) + annotate("text", x=0, y=0.393, label="0.390",color="black", size=4.5)+ annotate("text", x=0, y=0.3812, label="0.384",color="black", size=4.5)
dev.off()




other.pairs$GroupStage=NA;other.pairs$Round1=NA;other.pairs$Round2=NA;other.pairs$Round3=NA;other.pairs$Round4=NA;other.pairs$Rounds=NA;other.pairs$Score1=NA;other.pairs$Score2=NA;other.pairs$Day=NA;other.pairs$Month=NA;

dyads=rbind(data,other.pairs)
dyads=cbind(dyads[,1:3],NA,dyads[,4:ncol(dyads)])
colnames(dyads)[4]="Played"
dyads$Played[is.na(dyads$GroupStage)==TRUE]=0
dyads$Played[is.na(dyads$GroupStage)==FALSE]=1






contiguous=read.csv("ContiguousDyads.csv")

contiguous$State1Abb=gsub("AAB","Antigua & Barbuda", contiguous$State1Abb)
contiguous$State1Abb=gsub("AFG","Afghanistan", contiguous$State1Abb)
contiguous$State1Abb=gsub("ALB","Albania", contiguous$State1Abb)
contiguous$State1Abb=gsub("ALG","Algeria", contiguous$State1Abb)
contiguous$State1Abb=gsub("AND","Andorra", contiguous$State1Abb)
contiguous$State1Abb=gsub("ANG","Angola", contiguous$State1Abb)
contiguous$State1Abb=gsub("ARG","Argentina", contiguous$State1Abb)
contiguous$State1Abb=gsub("ARM","Armenia", contiguous$State1Abb)
contiguous$State1Abb=gsub("AUL","Australia", contiguous$State1Abb)
contiguous$State1Abb=gsub("AUS","Austria", contiguous$State1Abb)
contiguous$State1Abb=gsub("AZE ","Azerbaijan", contiguous$State1Abb)
contiguous$State1Abb=gsub("BAH","Bahrain", contiguous$State1Abb)
contiguous$State1Abb=gsub("BAR","Barbados", contiguous$State1Abb)
contiguous$State1Abb=gsub("BEL","Belgium", contiguous$State1Abb)
contiguous$State1Abb=gsub("BEN","Benin", contiguous$State1Abb)
contiguous$State1Abb=gsub("BFO","Burkina Faso", contiguous$State1Abb)
contiguous$State1Abb=gsub("BHM ","Bahamas", contiguous$State1Abb)
contiguous$State1Abb=gsub("BHU","Bhutan", contiguous$State1Abb)
contiguous$State1Abb=gsub("BLR","Belarus", contiguous$State1Abb)
contiguous$State1Abb=gsub("BLZ","Belize", contiguous$State1Abb)
contiguous$State1Abb=gsub("BNG ","Bangladesh", contiguous$State1Abb)
contiguous$State1Abb=gsub("BOL","Bolivia", contiguous$State1Abb)
contiguous$State1Abb=gsub("BOS","Bosnia-Herzegovina", contiguous$State1Abb)
contiguous$State1Abb=gsub("BOT","Botswana", contiguous$State1Abb)
contiguous$State1Abb=gsub("BRA","Brazil", contiguous$State1Abb)
contiguous$State1Abb=gsub("BRU","Brunei", contiguous$State1Abb)
contiguous$State1Abb=gsub("BUI","Burundi", contiguous$State1Abb)
contiguous$State1Abb=gsub("BUL","Bulgaria", contiguous$State1Abb)
contiguous$State1Abb=gsub("CAM","Cambodia", contiguous$State1Abb)
contiguous$State1Abb=gsub("CAN","Canada", contiguous$State1Abb)
contiguous$State1Abb=gsub("CAO","Cameroon", contiguous$State1Abb)
contiguous$State1Abb=gsub("CAP","Cape Verde", contiguous$State1Abb)
contiguous$State1Abb=gsub("CDI","Ivory Coast", contiguous$State1Abb)
contiguous$State1Abb=gsub("CEN ","Central African Republic", contiguous$State1Abb)
contiguous$State1Abb=gsub("CHA","Chad", contiguous$State1Abb)
contiguous$State1Abb=gsub("CHL","Chile", contiguous$State1Abb)
contiguous$State1Abb=gsub("CHN","China", contiguous$State1Abb)
contiguous$State1Abb=gsub("COL","Colombia", contiguous$State1Abb)
contiguous$State1Abb=gsub("COM","Comoros", contiguous$State1Abb)
contiguous$State1Abb=gsub("CON","Congo", contiguous$State1Abb)
contiguous$State1Abb=gsub("COS","Costa Rica", contiguous$State1Abb)
contiguous$State1Abb=gsub("CRO","Croatia", contiguous$State1Abb)
contiguous$State1Abb=gsub("CUB","Cuba", contiguous$State1Abb)
contiguous$State1Abb=gsub("CYP","Cyprus", contiguous$State1Abb)
contiguous$State1Abb=gsub("CZR","Czech Republic", contiguous$State1Abb)
contiguous$State1Abb=gsub("CZE","Czechoslovakia", contiguous$State1Abb)
contiguous$State1Abb=gsub("DEN","Denmark", contiguous$State1Abb)
contiguous$State1Abb=gsub("DJI","Djibouti", contiguous$State1Abb)
contiguous$State1Abb=gsub("DMA","Dominica", contiguous$State1Abb)
contiguous$State1Abb=gsub("DOM","Dominican Republic", contiguous$State1Abb)
# contiguous$State1Abb=gsub("DRC ","Democratic Republic of the Congo", contiguous$State1Abb)
contiguous$State1Abb=gsub("ECU","Ecuador", contiguous$State1Abb)
contiguous$State1Abb=gsub("EGY","Egypt", contiguous$State1Abb)
contiguous$State1Abb=gsub("EQG","Equatorial Guine", contiguous$State1Abb)
contiguous$State1Abb=gsub("ERI","Eritrea", contiguous$State1Abb)
contiguous$State1Abb=gsub("EST","Estonia", contiguous$State1Abb)
contiguous$State1Abb=gsub("ETH","Ethiopia", contiguous$State1Abb)
contiguous$State1Abb=gsub("FIJ","Fiji", contiguous$State1Abb)
contiguous$State1Abb=gsub("FIN","Finland", contiguous$State1Abb)
contiguous$State1Abb=gsub("FRN","France", contiguous$State1Abb)
contiguous$State1Abb=gsub("FSM","Federated States of Micronesia", contiguous$State1Abb)
contiguous$State1Abb=gsub("GAB","Gabon", contiguous$State1Abb)
contiguous$State1Abb=gsub("GAM","Gambia", contiguous$State1Abb)
contiguous$State1Abb=gsub("GHA","Ghana", contiguous$State1Abb)
contiguous$State1Abb=gsub("GMY","Germany", contiguous$State1Abb)
contiguous$State1Abb=gsub("GDR","East Germany", contiguous$State1Abb)
contiguous$State1Abb=gsub("GFR","West Germany", contiguous$State1Abb)
contiguous$State1Abb=gsub("GNB","Guin-Bissau", contiguous$State1Abb)
contiguous$State1Abb=gsub("GRC","Greece", contiguous$State1Abb)
contiguous$State1Abb=gsub("GRG","Georgia", contiguous$State1Abb)
contiguous$State1Abb=gsub("GRN","Grenada", contiguous$State1Abb)
contiguous$State1Abb=gsub("GUA","Guatemala", contiguous$State1Abb)
contiguous$State1Abb=gsub("GUI","Guinea", contiguous$State1Abb)
contiguous$State1Abb=gsub("GUY","Guyana", contiguous$State1Abb)
contiguous$State1Abb=gsub("HAI","Haiti", contiguous$State1Abb)
contiguous$State1Abb=gsub("HON","Honduras", contiguous$State1Abb)
contiguous$State1Abb=gsub("HUN","Hungary", contiguous$State1Abb)
contiguous$State1Abb=gsub("ICE","Iceland", contiguous$State1Abb)
contiguous$State1Abb=gsub("IND","India", contiguous$State1Abb)
contiguous$State1Abb=gsub("INS","Indonesia", contiguous$State1Abb)
contiguous$State1Abb=gsub("IRE","Ireland", contiguous$State1Abb)
contiguous$State1Abb=gsub("IRN","Iran", contiguous$State1Abb)
contiguous$State1Abb=gsub("IRQ","Iraq", contiguous$State1Abb)
contiguous$State1Abb=gsub("ISR","Israel", contiguous$State1Abb)
contiguous$State1Abb=gsub("ITA","Italy", contiguous$State1Abb)
contiguous$State1Abb=gsub("JAM","Jamaica", contiguous$State1Abb)
contiguous$State1Abb=gsub("JOR","Jordan", contiguous$State1Abb)
contiguous$State1Abb=gsub("JPN","Japan", contiguous$State1Abb)
contiguous$State1Abb=gsub("KEN","Kenya", contiguous$State1Abb)
contiguous$State1Abb=gsub("KIR","Kiribati", contiguous$State1Abb)
contiguous$State1Abb=gsub("KUW","Kuwait", contiguous$State1Abb)
contiguous$State1Abb=gsub("KYR","Kyrgyz Republic", contiguous$State1Abb)
contiguous$State1Abb=gsub("KZK","Kazakhstan", contiguous$State1Abb)
contiguous$State1Abb=gsub("LAO","Laos", contiguous$State1Abb)
contiguous$State1Abb=gsub("LAT","Latvia", contiguous$State1Abb)
contiguous$State1Abb=gsub("LBR","Liberia", contiguous$State1Abb)
contiguous$State1Abb=gsub("LEB","Lebanon", contiguous$State1Abb)
contiguous$State1Abb=gsub("LES","Lesotho", contiguous$State1Abb)
contiguous$State1Abb=gsub("LIB","Libya", contiguous$State1Abb)
contiguous$State1Abb=gsub("LIE","Liechtenstein", contiguous$State1Abb)
contiguous$State1Abb=gsub("LIT","Lithuania", contiguous$State1Abb)
contiguous$State1Abb=gsub("LUX","Luxembourg", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAA","Mauritania", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAC","Macedonia", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAD","Maldive Islands", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAG","Madagascar", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAL","Malaysia", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAS","Mauritius", contiguous$State1Abb)
contiguous$State1Abb=gsub("MAW","Malawi", contiguous$State1Abb)
contiguous$State1Abb=gsub("MEX","Mexico", contiguous$State1Abb)
contiguous$State1Abb=gsub("MLD","Moldova", contiguous$State1Abb)
contiguous$State1Abb=gsub("MLI","Mali", contiguous$State1Abb)
contiguous$State1Abb=gsub("MLT","Malta", contiguous$State1Abb)
contiguous$State1Abb=gsub("MNC","Monaco", contiguous$State1Abb)
contiguous$State1Abb=gsub("MON","Mongolia", contiguous$State1Abb)
contiguous$State1Abb=gsub("MOR","Morocco", contiguous$State1Abb)
contiguous$State1Abb=gsub("MSI","Marshall Islands", contiguous$State1Abb)
contiguous$State1Abb=gsub("MYA","Burma", contiguous$State1Abb)
contiguous$State1Abb=gsub("MZM","Mozambique", contiguous$State1Abb)
contiguous$State1Abb=gsub("NAM","Namibia", contiguous$State1Abb)
contiguous$State1Abb=gsub("NAU","Nauru", contiguous$State1Abb)
contiguous$State1Abb=gsub("NEP","Nepal", contiguous$State1Abb)
contiguous$State1Abb=gsub("NEW","New Zealand", contiguous$State1Abb)
contiguous$State1Abb=gsub("NIC","Nicaragua", contiguous$State1Abb)
contiguous$State1Abb=gsub("NIG","Nigeria", contiguous$State1Abb)
contiguous$State1Abb=gsub("NIR","Niger", contiguous$State1Abb)
contiguous$State1Abb=gsub("NOR","Norway", contiguous$State1Abb)
contiguous$State1Abb=gsub("NTH","Netherlands", contiguous$State1Abb)
contiguous$State1Abb=gsub("OMA","Oman", contiguous$State1Abb)
contiguous$State1Abb=gsub("PAK","Pakistan", contiguous$State1Abb)
contiguous$State1Abb=gsub("PAL","Palau", contiguous$State1Abb)
contiguous$State1Abb=gsub("PAN","Panama", contiguous$State1Abb)
contiguous$State1Abb=gsub("PAR","Paraguay", contiguous$State1Abb)
contiguous$State1Abb=gsub("PER","Peru", contiguous$State1Abb)
contiguous$State1Abb=gsub("PHI","Philippines", contiguous$State1Abb)
contiguous$State1Abb=gsub("PNG","Papua New Guin", contiguous$State1Abb)
contiguous$State1Abb=gsub("POL","Poland", contiguous$State1Abb)
contiguous$State1Abb=gsub("POR","Portugal", contiguous$State1Abb)
contiguous$State1Abb=gsub("PRK","North Korea", contiguous$State1Abb)
contiguous$State1Abb=gsub("QAT","Qatar", contiguous$State1Abb)
contiguous$State1Abb=gsub("ROK","South Korea", contiguous$State1Abb)
contiguous$State1Abb=gsub("ROM","Romania", contiguous$State1Abb)
contiguous$State1Abb=gsub("RUM","Romania", contiguous$State1Abb)
contiguous$State1Abb=gsub("RUS","Russia", contiguous$State1Abb)
contiguous$State1Abb=gsub("RWA","Rwanda", contiguous$State1Abb)
contiguous$State1Abb=gsub("SAF","South Africa", contiguous$State1Abb)
contiguous$State1Abb=gsub("SAL","El Salvador", contiguous$State1Abb)
contiguous$State1Abb=gsub("SAU","Saudi Arabia", contiguous$State1Abb)
contiguous$State1Abb=gsub("SEN","Senegal", contiguous$State1Abb)
contiguous$State1Abb=gsub("SEY","Seychelles", contiguous$State1Abb)
contiguous$State1Abb=gsub("SIE","Sierra Leone", contiguous$State1Abb)
contiguous$State1Abb=gsub("SIN","Singapore", contiguous$State1Abb)
contiguous$State1Abb=gsub("SKN","St. Kitts-Nevis", contiguous$State1Abb)
contiguous$State1Abb=gsub("SLO","Slovakia", contiguous$State1Abb)
contiguous$State1Abb=gsub("SLU","St. Lucia", contiguous$State1Abb)
contiguous$State1Abb=gsub("SLV","Slovenia", contiguous$State1Abb)
contiguous$State1Abb=gsub("SNM","San Marino", contiguous$State1Abb)
contiguous$State1Abb=gsub("SOL","Solomon Islands", contiguous$State1Abb)
contiguous$State1Abb=gsub("SOM","Somalia", contiguous$State1Abb)
contiguous$State1Abb=gsub("SPN","Spain", contiguous$State1Abb)
contiguous$State1Abb=gsub("SRI","Sri Lanka", contiguous$State1Abb)
contiguous$State1Abb=gsub("STP","Sao Tome-Principe", contiguous$State1Abb)
contiguous$State1Abb=gsub("SUD","Sudan", contiguous$State1Abb)
contiguous$State1Abb=gsub("SUR","Suriname", contiguous$State1Abb)
contiguous$State1Abb=gsub("SVG","St. Vincent and the Grenadines", contiguous$State1Abb)
contiguous$State1Abb=gsub("SWA","Swaziland", contiguous$State1Abb)
contiguous$State1Abb=gsub("SWD","Sweden", contiguous$State1Abb)
contiguous$State1Abb=gsub("SWZ","Switzerland", contiguous$State1Abb)
contiguous$State1Abb=gsub("SYR","Syria", contiguous$State1Abb)
contiguous$State1Abb=gsub("TAJ","Tajikistan", contiguous$State1Abb)
contiguous$State1Abb=gsub("TAW","Taiwan", contiguous$State1Abb)
contiguous$State1Abb=gsub("TAZ","Tanzania", contiguous$State1Abb)
contiguous$State1Abb=gsub("THI","Thailand", contiguous$State1Abb)
contiguous$State1Abb=gsub("TKM","Turkmenistan", contiguous$State1Abb)
contiguous$State1Abb=gsub("TOG","Togo", contiguous$State1Abb)
contiguous$State1Abb=gsub("TON","Tonga", contiguous$State1Abb)
contiguous$State1Abb=gsub("TRI","Trinidad", contiguous$State1Abb)
contiguous$State1Abb=gsub("TUN","Tunisia", contiguous$State1Abb)
contiguous$State1Abb=gsub("TUR","Turkey", contiguous$State1Abb)
contiguous$State1Abb=gsub("TUV","Tuvalu", contiguous$State1Abb)
contiguous$State1Abb=gsub("UAE","UAE", contiguous$State1Abb)
contiguous$State1Abb=gsub("UGA","Uganda", contiguous$State1Abb)
contiguous$State1Abb=gsub("UKG","United Kingdom", contiguous$State1Abb)
contiguous$State1Abb=gsub("UKR","Ukraine", contiguous$State1Abb)
contiguous$State1Abb=gsub("URU","Uruguay", contiguous$State1Abb)
contiguous$State1Abb=gsub("USR","Soviet Union", contiguous$State1Abb)
contiguous$State1Abb=gsub("USA","United States", contiguous$State1Abb)
contiguous$State1Abb=gsub("UZB","Uzbekistan", contiguous$State1Abb)
contiguous$State1Abb=gsub("VAN","Vanuatu", contiguous$State1Abb)
contiguous$State1Abb=gsub("VEN","Venezuela", contiguous$State1Abb)
contiguous$State1Abb=gsub("VTM","Vietnam", contiguous$State1Abb)
contiguous$State1Abb=gsub("WSM","Western Samoa", contiguous$State1Abb)
contiguous$State1Abb=gsub("YEM","Yemen", contiguous$State1Abb)
contiguous$State1Abb=gsub("YAR","NorthYemen", contiguous$State1Abb)
contiguous$State1Abb=gsub("YUG","Yugoslavia", contiguous$State1Abb)
contiguous$State1Abb=gsub("ZAM","Zambia", contiguous$State1Abb)
contiguous$State1Abb=gsub("ZIM","Zimbabwe", contiguous$State1Abb)
contiguous$State1Abb=gsub("ZAI","Zaire", contiguous$State1Abb)
contiguous$State1Abb=gsub("DRC","Zaire", contiguous$State1Abb)

contiguous$State2Abb=gsub("AAB","Antigua & Barbuda", contiguous$State2Abb)
contiguous$State2Abb=gsub("AFG","Afghanistan", contiguous$State2Abb)
contiguous$State2Abb=gsub("ALB","Albania", contiguous$State2Abb)
contiguous$State2Abb=gsub("ALG","Algeria", contiguous$State2Abb)
contiguous$State2Abb=gsub("AND","Andorra", contiguous$State2Abb)
contiguous$State2Abb=gsub("ANG","Angola", contiguous$State2Abb)
contiguous$State2Abb=gsub("ARG","Argentina", contiguous$State2Abb)
contiguous$State2Abb=gsub("ARM","Armenia", contiguous$State2Abb)
contiguous$State2Abb=gsub("AUL","Australia", contiguous$State2Abb)
contiguous$State2Abb=gsub("AUS","Austria", contiguous$State2Abb)
contiguous$State2Abb=gsub("AZE ","Azerbaijan", contiguous$State2Abb)
contiguous$State2Abb=gsub("BAH","Bahrain", contiguous$State2Abb)
contiguous$State2Abb=gsub("BAR","Barbados", contiguous$State2Abb)
contiguous$State2Abb=gsub("BEL","Belgium", contiguous$State2Abb)
contiguous$State2Abb=gsub("BEN","Benin", contiguous$State2Abb)
contiguous$State2Abb=gsub("BFO","Burkina Faso", contiguous$State2Abb)
contiguous$State2Abb=gsub("BHM ","Bahamas", contiguous$State2Abb)
contiguous$State2Abb=gsub("BHU","Bhutan", contiguous$State2Abb)
contiguous$State2Abb=gsub("BLR","Belarus", contiguous$State2Abb)
contiguous$State2Abb=gsub("BLZ","Belize", contiguous$State2Abb)
contiguous$State2Abb=gsub("BNG ","Bangladesh", contiguous$State2Abb)
contiguous$State2Abb=gsub("BOL","Bolivia", contiguous$State2Abb)
contiguous$State2Abb=gsub("BOS","Bosnia-Herzegovina", contiguous$State2Abb)
contiguous$State2Abb=gsub("BOT","Botswana", contiguous$State2Abb)
contiguous$State2Abb=gsub("BRA","Brazil", contiguous$State2Abb)
contiguous$State2Abb=gsub("BRU","Brunei", contiguous$State2Abb)
contiguous$State2Abb=gsub("BUI","Burundi", contiguous$State2Abb)
contiguous$State2Abb=gsub("BUL","Bulgaria", contiguous$State2Abb)
contiguous$State2Abb=gsub("CAM","Cambodia", contiguous$State2Abb)
contiguous$State2Abb=gsub("CAN","Canada", contiguous$State2Abb)
contiguous$State2Abb=gsub("CAO","Cameroon", contiguous$State2Abb)
contiguous$State2Abb=gsub("CAP","Cape Verde", contiguous$State2Abb)
contiguous$State2Abb=gsub("CDI","Ivory Coast", contiguous$State2Abb)
contiguous$State2Abb=gsub("CEN ","Central African Republic", contiguous$State2Abb)
contiguous$State2Abb=gsub("CHA","Chad", contiguous$State2Abb)
contiguous$State2Abb=gsub("CHL","Chile", contiguous$State2Abb)
contiguous$State2Abb=gsub("CHN","China", contiguous$State2Abb)
contiguous$State2Abb=gsub("COL","Colombia", contiguous$State2Abb)
contiguous$State2Abb=gsub("COM","Comoros", contiguous$State2Abb)
contiguous$State2Abb=gsub("CON","Congo", contiguous$State2Abb)
contiguous$State2Abb=gsub("COS","Costa Rica", contiguous$State2Abb)
contiguous$State2Abb=gsub("CRO","Croatia", contiguous$State2Abb)
contiguous$State2Abb=gsub("CUB","Cuba", contiguous$State2Abb)
contiguous$State2Abb=gsub("CYP","Cyprus", contiguous$State2Abb)
contiguous$State2Abb=gsub("CZR","Czech Republic", contiguous$State2Abb)
contiguous$State2Abb=gsub("CZE","Czechoslovakia", contiguous$State2Abb)
contiguous$State2Abb=gsub("DEN","Denmark", contiguous$State2Abb)
contiguous$State2Abb=gsub("DJI","Djibouti", contiguous$State2Abb)
contiguous$State2Abb=gsub("DMA","Dominica", contiguous$State2Abb)
contiguous$State2Abb=gsub("DOM","Dominican Republic", contiguous$State2Abb)
# contiguous$State2Abb=gsub("DRC ","Democratic Republic of the Congo", contiguous$State2Abb)
contiguous$State2Abb=gsub("ECU","Ecuador", contiguous$State2Abb)
contiguous$State2Abb=gsub("EGY","Egypt", contiguous$State2Abb)
contiguous$State2Abb=gsub("EQG","Equatorial Guine", contiguous$State2Abb)
contiguous$State2Abb=gsub("ERI","Eritrea", contiguous$State2Abb)
contiguous$State2Abb=gsub("EST","Estonia", contiguous$State2Abb)
contiguous$State2Abb=gsub("ETH","Ethiopia", contiguous$State2Abb)
contiguous$State2Abb=gsub("FIJ","Fiji", contiguous$State2Abb)
contiguous$State2Abb=gsub("FIN","Finland", contiguous$State2Abb)
contiguous$State2Abb=gsub("FRN","France", contiguous$State2Abb)
contiguous$State2Abb=gsub("FSM","Federated States of Micronesia", contiguous$State2Abb)
contiguous$State2Abb=gsub("GAB","Gabon", contiguous$State2Abb)
contiguous$State2Abb=gsub("GAM","Gambia", contiguous$State2Abb)
contiguous$State2Abb=gsub("GHA","Ghana", contiguous$State2Abb)
contiguous$State2Abb=gsub("GMY","Germany", contiguous$State2Abb)
contiguous$State2Abb=gsub("GDR","East Germany", contiguous$State2Abb)
contiguous$State2Abb=gsub("GFR","West Germany", contiguous$State2Abb)
contiguous$State2Abb=gsub("GNB","Guin-Bissau", contiguous$State2Abb)
contiguous$State2Abb=gsub("GRC","Greece", contiguous$State2Abb)
contiguous$State2Abb=gsub("GRG","Georgia", contiguous$State2Abb)
contiguous$State2Abb=gsub("GRN","Grenada", contiguous$State2Abb)
contiguous$State2Abb=gsub("GUA","Guatemala", contiguous$State2Abb)
contiguous$State2Abb=gsub("GUI","Guinea", contiguous$State2Abb)
contiguous$State2Abb=gsub("GUY","Guyana", contiguous$State2Abb)
contiguous$State2Abb=gsub("HAI","Haiti", contiguous$State2Abb)
contiguous$State2Abb=gsub("HON","Honduras", contiguous$State2Abb)
contiguous$State2Abb=gsub("HUN","Hungary", contiguous$State2Abb)
contiguous$State2Abb=gsub("ICE","Iceland", contiguous$State2Abb)
contiguous$State2Abb=gsub("IND","India", contiguous$State2Abb)
contiguous$State2Abb=gsub("INS","Indonesia", contiguous$State2Abb)
contiguous$State2Abb=gsub("IRE","Ireland", contiguous$State2Abb)
contiguous$State2Abb=gsub("IRN","Iran", contiguous$State2Abb)
contiguous$State2Abb=gsub("IRQ","Iraq", contiguous$State2Abb)
contiguous$State2Abb=gsub("ISR","Israel", contiguous$State2Abb)
contiguous$State2Abb=gsub("ITA","Italy", contiguous$State2Abb)
contiguous$State2Abb=gsub("JAM","Jamaica", contiguous$State2Abb)
contiguous$State2Abb=gsub("JOR","Jordan", contiguous$State2Abb)
contiguous$State2Abb=gsub("JPN","Japan", contiguous$State2Abb)
contiguous$State2Abb=gsub("KEN","Kenya", contiguous$State2Abb)
contiguous$State2Abb=gsub("KIR","Kiribati", contiguous$State2Abb)
contiguous$State2Abb=gsub("KUW","Kuwait", contiguous$State2Abb)
contiguous$State2Abb=gsub("KYR","Kyrgyz Republic", contiguous$State2Abb)
contiguous$State2Abb=gsub("KZK","Kazakhstan", contiguous$State2Abb)
contiguous$State2Abb=gsub("LAO","Laos", contiguous$State2Abb)
contiguous$State2Abb=gsub("LAT","Latvia", contiguous$State2Abb)
contiguous$State2Abb=gsub("LBR","Liberia", contiguous$State2Abb)
contiguous$State2Abb=gsub("LEB","Lebanon", contiguous$State2Abb)
contiguous$State2Abb=gsub("LES","Lesotho", contiguous$State2Abb)
contiguous$State2Abb=gsub("LIB","Libya", contiguous$State2Abb)
contiguous$State2Abb=gsub("LIE","Liechtenstein", contiguous$State2Abb)
contiguous$State2Abb=gsub("LIT","Lithuania", contiguous$State2Abb)
contiguous$State2Abb=gsub("LUX","Luxembourg", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAA","Mauritania", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAC","Macedonia", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAD","Maldive Islands", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAG","Madagascar", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAL","Malaysia", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAS","Mauritius", contiguous$State2Abb)
contiguous$State2Abb=gsub("MAW","Malawi", contiguous$State2Abb)
contiguous$State2Abb=gsub("MEX","Mexico", contiguous$State2Abb)
contiguous$State2Abb=gsub("MLD","Moldova", contiguous$State2Abb)
contiguous$State2Abb=gsub("MLI","Mali", contiguous$State2Abb)
contiguous$State2Abb=gsub("MLT","Malta", contiguous$State2Abb)
contiguous$State2Abb=gsub("MNC","Monaco", contiguous$State2Abb)
contiguous$State2Abb=gsub("MON","Mongolia", contiguous$State2Abb)
contiguous$State2Abb=gsub("MOR","Morocco", contiguous$State2Abb)
contiguous$State2Abb=gsub("MSI","Marshall Islands", contiguous$State2Abb)
contiguous$State2Abb=gsub("MYA","Burma", contiguous$State2Abb)
contiguous$State2Abb=gsub("MZM","Mozambique", contiguous$State2Abb)
contiguous$State2Abb=gsub("NAM","Namibia", contiguous$State2Abb)
contiguous$State2Abb=gsub("NAU","Nauru", contiguous$State2Abb)
contiguous$State2Abb=gsub("NEP","Nepal", contiguous$State2Abb)
contiguous$State2Abb=gsub("NEW","New Zealand", contiguous$State2Abb)
contiguous$State2Abb=gsub("NIC","Nicaragua", contiguous$State2Abb)
contiguous$State2Abb=gsub("NIG","Nigeria", contiguous$State2Abb)
contiguous$State2Abb=gsub("NIR","Niger", contiguous$State2Abb)
contiguous$State2Abb=gsub("NOR","Norway", contiguous$State2Abb)
contiguous$State2Abb=gsub("NTH","Netherlands", contiguous$State2Abb)
contiguous$State2Abb=gsub("OMA","Oman", contiguous$State2Abb)
contiguous$State2Abb=gsub("PAK","Pakistan", contiguous$State2Abb)
contiguous$State2Abb=gsub("PAL","Palau", contiguous$State2Abb)
contiguous$State2Abb=gsub("PAN","Panama", contiguous$State2Abb)
contiguous$State2Abb=gsub("PAR","Paraguay", contiguous$State2Abb)
contiguous$State2Abb=gsub("PER","Peru", contiguous$State2Abb)
contiguous$State2Abb=gsub("PHI","Philippines", contiguous$State2Abb)
contiguous$State2Abb=gsub("PNG","Papua New Guin", contiguous$State2Abb)
contiguous$State2Abb=gsub("POL","Poland", contiguous$State2Abb)
contiguous$State2Abb=gsub("POR","Portugal", contiguous$State2Abb)
contiguous$State2Abb=gsub("PRK","North Korea", contiguous$State2Abb)
contiguous$State2Abb=gsub("QAT","Qatar", contiguous$State2Abb)
contiguous$State2Abb=gsub("ROK","South Korea", contiguous$State2Abb)
contiguous$State2Abb=gsub("ROM","Romania", contiguous$State2Abb)
contiguous$State2Abb=gsub("RUM","Romania", contiguous$State2Abb)
contiguous$State2Abb=gsub("RUS","Russia", contiguous$State2Abb)
contiguous$State2Abb=gsub("RWA","Rwanda", contiguous$State2Abb)
contiguous$State2Abb=gsub("SAF","South Africa", contiguous$State2Abb)
contiguous$State2Abb=gsub("SAL","El Salvador", contiguous$State2Abb)
contiguous$State2Abb=gsub("SAU","Saudi Arabia", contiguous$State2Abb)
contiguous$State2Abb=gsub("SEN","Senegal", contiguous$State2Abb)
contiguous$State2Abb=gsub("SEY","Seychelles", contiguous$State2Abb)
contiguous$State2Abb=gsub("SIE","Sierra Leone", contiguous$State2Abb)
contiguous$State2Abb=gsub("SIN","Singapore", contiguous$State2Abb)
contiguous$State2Abb=gsub("SKN","St. Kitts-Nevis", contiguous$State2Abb)
contiguous$State2Abb=gsub("SLO","Slovakia", contiguous$State2Abb)
contiguous$State2Abb=gsub("SLU","St. Lucia", contiguous$State2Abb)
contiguous$State2Abb=gsub("SLV","Slovenia", contiguous$State2Abb)
contiguous$State2Abb=gsub("SNM","San Marino", contiguous$State2Abb)
contiguous$State2Abb=gsub("SOL","Solomon Islands", contiguous$State2Abb)
contiguous$State2Abb=gsub("SOM","Somalia", contiguous$State2Abb)
contiguous$State2Abb=gsub("SPN","Spain", contiguous$State2Abb)
contiguous$State2Abb=gsub("SRI","Sri Lanka", contiguous$State2Abb)
contiguous$State2Abb=gsub("STP","Sao Tome-Principe", contiguous$State2Abb)
contiguous$State2Abb=gsub("SUD","Sudan", contiguous$State2Abb)
contiguous$State2Abb=gsub("SUR","Suriname", contiguous$State2Abb)
contiguous$State2Abb=gsub("SVG","St. Vincent and the Grenadines", contiguous$State2Abb)
contiguous$State2Abb=gsub("SWA","Swaziland", contiguous$State2Abb)
contiguous$State2Abb=gsub("SWD","Sweden", contiguous$State2Abb)
contiguous$State2Abb=gsub("SWZ","Switzerland", contiguous$State2Abb)
contiguous$State2Abb=gsub("SYR","Syria", contiguous$State2Abb)
contiguous$State2Abb=gsub("TAJ","Tajikistan", contiguous$State2Abb)
contiguous$State2Abb=gsub("TAW","Taiwan", contiguous$State2Abb)
contiguous$State2Abb=gsub("TAZ","Tanzania", contiguous$State2Abb)
contiguous$State2Abb=gsub("THI","Thailand", contiguous$State2Abb)
contiguous$State2Abb=gsub("TKM","Turkmenistan", contiguous$State2Abb)
contiguous$State2Abb=gsub("TOG","Togo", contiguous$State2Abb)
contiguous$State2Abb=gsub("TON","Tonga", contiguous$State2Abb)
contiguous$State2Abb=gsub("TRI","Trinidad", contiguous$State2Abb)
contiguous$State2Abb=gsub("TUN","Tunisia", contiguous$State2Abb)
contiguous$State2Abb=gsub("TUR","Turkey", contiguous$State2Abb)
contiguous$State2Abb=gsub("TUV","Tuvalu", contiguous$State2Abb)
contiguous$State2Abb=gsub("UAE","UAE", contiguous$State2Abb)
contiguous$State2Abb=gsub("UGA","Uganda", contiguous$State2Abb)
contiguous$State2Abb=gsub("UKG","United Kingdom", contiguous$State2Abb)
contiguous$State2Abb=gsub("UKR","Ukraine", contiguous$State2Abb)
contiguous$State2Abb=gsub("URU","Uruguay", contiguous$State2Abb)
contiguous$State2Abb=gsub("USR","Soviet Union", contiguous$State2Abb)
contiguous$State2Abb=gsub("USA","USA", contiguous$State2Abb)
contiguous$State2Abb=gsub("UZB","Uzbekistan", contiguous$State2Abb)
contiguous$State2Abb=gsub("VAN","Vanuatu", contiguous$State2Abb)
contiguous$State2Abb=gsub("VEN","Venezuela", contiguous$State2Abb)
contiguous$State2Abb=gsub("VTM","Vietnam", contiguous$State2Abb)
contiguous$State2Abb=gsub("WSM","Western Samoa", contiguous$State2Abb)
contiguous$State2Abb=gsub("YEM","Yemen", contiguous$State2Abb)
contiguous$State2Abb=gsub("YAR","NorthYemen", contiguous$State2Abb)
contiguous$State2Abb=gsub("YUG","Yugoslavia", contiguous$State2Abb)
contiguous$State2Abb=gsub("ZAM","Zambia", contiguous$State2Abb)
contiguous$State2Abb=gsub("ZIM","Zimbabwe", contiguous$State2Abb)
contiguous$State2Abb=gsub("ZAI","Zaire", contiguous$State2Abb)
contiguous$State2Abb=gsub("DRC","Zaire", contiguous$State2Abb)

con=function(v){
a=contiguous[c(which(contiguous$State1Abb==v[2]),which(contiguous$State2Abb==v[2])),]
a=a[c(which(a$State1Abb==v[3]),which(a$State2Abb==v[3])),]
if(dim(a)[1]==0){return(0)}
if(dim(a)[1]==2){return(1)}
}


mean(unlist(apply(data,1,con)))

mean(unlist(apply(other.pairs,1,con)))
