### install and load packages as needed ###
install.packages('lme4')
install.packages('stargazer')
install.packages('lmerTest')
install.packages('multcomp')
install.packages('openxlsx')

library('lme4')
library('stargazer')
library('lmerTest')
library('multcomp')
library("openxlsx")


### set the path to the folder containing the data files ###
setwd('Data')
a<-read.table("Behavioural task.txt", header=TRUE, sep='\t')
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

######## FIGURE 1 #############
#### compliance rates with/without control questions ###
# calculate mean compliance without and with control questions
means<-rep(0,2)
Ns<-rep(0,2)
for (controlQ in 0:1){
	b<-subset(a, a$control_questions==controlQ)
	means[controlQ+1]<-1-mean(b$violate1)
	Ns[controlQ+1]<-nrow(b)
}

### show proportions of compliance (with associated sample sizes)
means
Ns

### windsorize extremely slow responses (with payoff 0)
a$move1<-ifelse(a$move1<20000, a$move1, 20000)
# check responses to traffic light signal (after 12 seconds)
hist(a$move1)


### DESCRIPTIVES (TABLES S1 AND S2) #######
stargazer(a, type="html", out="Table S1.html")



#### ROBUSTNESS CHECKS FOR THESE EFFECTS of BASELINE COMPLIANCE ####
### Nottingham students
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 2, startRow = 1, colNames = TRUE)
a$move1<-ifelse(a$move1<20000, a$move1, 20000)
# check responses to traffic light signal (after 12 seconds)
hist(a$move1)
a$comply1<-ifelse(a$move1<12000,0,1)
mean(a$comply1)

##### do the same for the abstract 'cross' task (Fig. 1B,D). NB: signal not after 12 seconds, but after 8)
### DO vs DONT is for a separate study; here we focus on DONT in parallel to the traffic light rules
## which states 'DONT move before it turns green'

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 7, startRow = 1, colNames = TRUE)
a<-subset(a, a$DOorDONT=='DONT')
a$move1<-ifelse(a$move1<20000, a$move1, 20000)
hist(a$move1)
a$comply1<-ifelse(a$move1<8100,0,1)
mean(a$comply1)



##### FIT REGRESSION MODELS TO BEHAVIOURAL DATA ####
### read the Traffic Lights task dataset (again); variable a was overwritten above :)
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
a$comply1<-ifelse(a$move1<8100,0,1)

### we only had males and females (no 'other' category) 
a$male<-ifelse(a$gender=='male', 1,0)


### some questionnaires were conducted with the same participants in a later, dedicated session (not right after the task) 
### the responses there take priority over the responses right after the task
### this is because items measuring constructs like 'patience' could have been influenced
### by task behaviour

## big 5
a$risk_taking<-ifelse(is.na(a$riskTakingQ), a$riskTaking, a$riskTakingQ)
a$extraversion<-ifelse(is.na(a$ExtraversionQ), a$Extraversion, a$ExtraversionQ)
a$agreeableness<-ifelse(is.na(a$AgreeablenessQ), a$Agreeableness, a$AgreeablenessQ)
a$conscientiousness<-ifelse(is.na(a$ConscientiousnessQ), a$Conscientiousness, a$ConscientiousnessQ)
a$emotional_stability<-ifelse(is.na(a$Emotional_stabilityQ), a$Emotional_stability, a$Emotional_stabilityQ)
a$openness<-ifelse(is.na(a$OpennessQ), a$Openness, a$OpennessQ)

# conditional cooperation
a$otherc<-ifelse(is.na(a$otherCQ), a$otherC, a$otherCQ)
a$otherd<-ifelse(is.na(a$otherDQ), a$otherD, a$otherDQ)

# GASP (guilt and shame proneness)
a$guilt_neg<-ifelse(is.na(a$guilt_negQ), a$guilt_neg, a$guilt_negQ)
a$guilt_repair<-ifelse(is.na(a$guilt_repairQ), a$guilt_repair, a$guilt_repairQ)
a$shame_neg<-ifelse(is.na(a$shame_negQ), a$shame_neg, a$shame_negQ)
a$shame_withdraw<-ifelse(is.na(a$shame_withdrawQ), a$shame_withdraw, a$shame_withdrawQ)

# see who is a conditional cooperator and who is not
a$condCoop<-0;
for (i in 1:nrow(a)){
	if(a$otherc[i]=='C' && a$otherd[i]=='D') a$condCoop[i]<-1
}

### fit exploratory model to see if any of the variables predicts compliance in the solo task
m1linear<-lm(comply1 ~ age + male + 
	patience + risk_taking + 
	guilt_neg + guilt_repair + shame_neg + shame_withdraw + 
	extraversion + agreeableness + conscientiousness + emotional_stability + openness + 
	 condCoop + 
	control_questions, data=a)

### logistic model (robustness test)
m1logistic<-glm(comply1 ~ age + male + 
	patience + risk_taking + 
	guilt_neg + guilt_repair + shame_neg + shame_withdraw + 
	extraversion + agreeableness + conscientiousness + emotional_stability + openness + 
	 condCoop + 
	control_questions, family='binomial', data=a)


### output HTML file #######
stargazer(m1linear,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
# report=('vcsp'),
	out="Table S2.html")
### output HTML file #######

stargazer(m1logistic, m1,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
# report=('vcsp'),
	out="Table S2_robustness.html")


### Data shown in Figure 2 (social expectations and conditional compliance) ###

### normative expectations 
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 3, startRow = 1, colNames = TRUE)
### 
## calculate the distribution of appropriateness ratings in absence of peers
b<-subset(a, a$numberPeers==0)

## bookkeeping
freqV<-rep(0,4)
freqC<-rep(0,4)

for (i in 1:4){
	freqV[i]<-length(which(b$treatmentViolateRating==i))
	freqC[i]<-length(which(b$treatmentComplyRating==i))
}
freqV<-freqV/sum(freqV)
freqC<-freqC/sum(freqC)

### cumulative distributions (y values in Fig. 2A)
csV<-c(0,cumsum(freqV))
csC<-c(0,cumsum(freqC))

### do the plotting itself ###
cols<-c('#b94a48','#b74d86', '#6780d8', '#50b47b')
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, yaxs='i')

par(plt=c(0.1,0.3, 0.15, 0.95))
plot.new()
plot(0, type='n', xlim=c(0,3), ylim=c(0,1), xlab='', ylab='', axes=F)
axis(1, at=c(-10,10), labels=FALSE)
axis(2, at=0:5/5, labels=FALSE)


for (i in 1:4){
	thisCol<-cols[i]
#	if (i==2 || i==3) thisCol<-adjustColor(thisCol, alpha=0.6)
	rect(0.3, csC[i], 1.2, csC[i+1], col=thisCol, lwd=3)
	rect(1.8, csV[i], 2.7, csV[i+1], col=thisCol, lwd=3)
	
	if (i<3) {
		rect(0.3, csC[i], 1.2, csC[i+1], col='black',lwd=1.5, density=10)
		rect(1.8, csV[i], 2.7, csV[i+1], col='black', lwd=1.5, density=10)
				
	}
}

cols<-c("#ff9653","#7f63da","#4a4e00")

### plot empirical expectations (descriptive norms); Fig. 2B ###
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 4, startRow = 1, colNames = TRUE)
### 
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, yaxs='i')

par(plt=c(0.35,0.47, 0.15, 0.95), new=T)
plot.new()
plot(0, type='n', xlim=c(0,2), ylim=c(0,1), xlab='', ylab='', axes=F)

axis(1, at=c(-10,10))
axis(2, labels=FALSE)
cols<-c("#ff9653","#7f63da","#4a4e00")

### plot a bar chart based on the summary of the data (stored in variable s)
s<-summary(a$estimateComply/100)
arrows(1,s[1],1,s[6], lwd=3, code=0)
rect(0.5,s[2],1.5,s[5], col=cols[1], lwd=3)
arrows(0.5, s[3], 1.5, s[3], lwd=4, code=0)
arrows(0.5, s[4], 1.5, s[4], lwd=3, lty=3, code=0)

### find the mode empirical expectation
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mo<-Mode(a$estimateComply/100)

### plot preferences for compliance conditional on normative expectations ###
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 5, startRow = 1, colNames = TRUE)
### create graph ###
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1)
par(plt=c(0.52,0.74, 0.15, 0.95), new=T)
plot.new()
plot(0, type='n', xlim=c(0,100), ylim=c(0,1), xlab='', ylab='', axes=F)
axis(1, labels=FALSE)
axis(2, labels=FALSE)

### plot the data points
xs<-c()
ms<-c(); Ns<-c()
for (i in 1:5){
	x<- ((i-1)*20+10);
	xs<-c(xs,x)
	y<- 1-a[,3+i];
	
	m<-mean(y)
	ms<-c(ms,m)
	n<-length(y)
	Ns<-c(Ns,n)
	se<-sqrt(m*(1-m)/n) 
	arrows(x,m-se,x,m+se,col='black', lwd=3, code=0)
	points(x,m,cex=2,pch=16,col=cols[3])
}
ms
Ns
lines(xs,ms, col=cols[3], lwd=3)


### statistics table S3 (column 1)
### create a table with 1 decision on each row (to fit a lmer)
b<-matrix(nrow=0, ncol=5)
for (i in 1:nrow(a)){
	male<-ifelse(a$gender[i]=='male',1,0)
	for (j in 1:5){
		newRow<-c(a$session[i]*100000 + a$playerNr[i], ((j-1)*20+10)/100, 1-a[i,3+j],male,a$age[i])
		b<-rbind(b, newRow)
	}
}
b<-data.frame(b)
names(b)<-c('participant', 'condition','comply','male','age')
rownames(b)<-c()


### model 1 in Table S3
m1_normative<-lmer(comply~condition+age+male + (1|participant), data=b)
data_normative<-b


### summarize individual strategy profiles ###

### plot preferences for compliance conditional on empirical expectations (Fig. 2D) ###
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 6, startRow = 1, colNames = TRUE)

### create plot ###
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, new=TRUE)
par(plt=c(0.78,1, 0.15, 0.95))
plot.new()
plot(0, type='n', xlim=c(0,100), ylim=c(0,1), xlab='', ylab='', axes=F)

axis(1, labels=FALSE)
axis(2, labels=FALSE)
# plot data points

xs<-c()
ms<-c(); Ns<-c()
for (i in 1:5){
	x<- 100 - ((i-1)*20+10);
	xs<-c(xs,x)
	y<- 1-a[,3+i]; #plot compliance, not violation
	
	m<-mean(y)
	ms<-c(ms,m)
	n<-length(y)
	Ns<-c(Ns,n)
	se<-sqrt(m*(1-m)/n)

	
	arrows(x,(m-se),x,(m+se),col='black', lwd=3, code=0)
	points(x,m,cex=2,pch=15,col=cols[2])
}
lines(xs,ms, col=cols[2], lwd=3)



b<-matrix(nrow=0, ncol=5)
for (i in 1:nrow(a)){

	male<-ifelse(a$gender[i]=='male', 1,0)
	for (j in 1:5){
		newRow<-c(a$session[i]*1000 + a$playerNr[i], (100-((j-1)*20+10))/100, 1-a[i,3+j],male,a$age[i])
		b<-rbind(b, newRow)
	}
}
b<-data.frame(b)
names(b)<-c('participant', 'condition','comply','male','age')
rownames(b)<-c()

### model 2 in Table S3
m1_empirical<-lmer(comply~condition+age+male + (1|participant), data=b)
data_empirical<-b

### combined model (model 3 in Table S3)
data_normative$treatment<-0
data_empirical$treatment<-1

b<-rbind(data_normative, data_empirical)


m1_combined<-lmer(comply ~ treatment * condition + age + male + (1|participant), data=b)

### for stargazer output, we need to change the model object class
class(m1_normative) <- "lmerMod"
class(m1_empirical) <- "lmerMod"
class(m1_combined) <- "lmerMod"
### this is Table S3 in full
stargazer(m1_normative, m1_empirical, m1_combined, type="html", ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE, star.cutoffs=c(0.05,0.01,0.001), out="Table S3.html")



a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 5, startRow = 1, colNames = TRUE)
## check which participants have a monotonic profile
a$monotonic<-1;

for (i in 1:nrow(a)){
	for (j in 4:7){
		if (a[i,j] < a[i,j+1]) a$monotonic[i]<-0
	}
}
a$monotonic
a$sumViol<-99
for (i in 1:nrow(a)){
	a$sumViol[i]<-sum(a[i,4:8])
}
length(which(a$sumViol==5))/nrow(a)

distr<-c();
b<-subset(a, a$monotonic==1)
for (i in 5:0){
	distr<-c(distr, length(which(b$sumViol==i))/nrow(a))
}
distr<-c(distr, (nrow(a)-nrow(b))/nrow(a))

cumDist_normative<-c(0, cumsum(distr))



a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 6, startRow = 1, colNames = TRUE)
## check which participants have a monotonic profile
a$monotonic<-1;

for (i in 1:nrow(a)){
	for (j in 4:7){
		if (a[i,j] > a[i,j+1]) a$monotonic[i]<-0
	}
}
a$monotonic
a$sumViol<-99
for (i in 1:nrow(a)){
	a$sumViol[i]<-sum(a[i,4:8])
}
length(which(a$sumViol==5))/nrow(a)

distr<-c();
b<-subset(a, a$monotonic==1)
for (i in 5:0){
	distr<-c(distr, length(which(b$sumViol==i))/nrow(a))
}
distr<-c(distr, (nrow(a)-nrow(b))/nrow(a))

cumDist_empirical<-c(0, cumsum(distr))

cumDist<-cbind(cumDist_normative, cumDist_empirical)

dev.off()

par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, yaxs='i')
plot(0, type='n', xlab='', ylab='', axes=FALSE, xlim=c(1,5), ylim=c(0,1))
axis(1, at=c(-1,10), tck=FALSE)
axis(2, at=0:5/5)
cols<-c('#999C47', rep('#9771BC',4), '#B35A61', 'grey90')
alphs<-c(1,1,0.8, 0.6, 0.4, 1, 1)
colCnt<-1;

for (tr in 1:2){
	for (i in 1:7){
		rect(-0.4+tr*2,cumDist[i,tr],0.4+tr*2, cumDist[i+1,tr], col=adjustcolor(cols[i], alpha=alphs[i]))
		if (i>1 && i < 6) rect(-0.4+tr*2,cumDist[i,tr],0.4+tr*2, cumDist[i+1,tr], density=20, border='black', lwd=1)
	}
}


##################### PEER EFFECTS iteration 2 #################

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

a$move1<-ifelse(a$move1<20000, a$move1, 20000)
a$move2<-ifelse(a$move2<20000, a$move2, 20000)
a$move3<-ifelse(a$move3<20000, a$move3, 20000)
a$comply1<-ifelse(a$move1<12000,0,1)
a$comply2<-ifelse(a$move2<12000,0,1)
a$comply3<-ifelse(a$move3<12000,0,1)


### Figure 3B (treatment differences in compliance relative to no-peers baseline)

### we need a weighted average across with and without control questions
### first calculate no-peers baseline compliance
b<-subset(a, a$control_questions==0)
d<-subset(b, b$numberOfPeers==0)
peers0viol_noControlQ<-mean(d$violate2)


b<-subset(a, a$control_questions==1)
d<-subset(b, b$numberOfPeers==0)
peers0viol_yesControlQ<-mean(d$violate2)

n<-c();
for (i in 1:28)	n<-c(n, length(which(a$treatment==i)))
n

### set colours for bars (higher vs lower than no-peers baseline)
cols<-c("#ca5572", "#6097ce")

require('png')
angel<-readPNG('angel.png')
demon<-readPNG('demon.png')

w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1, cex.lab=1, lend=1, plt=c(0,1,0,1))
plot(0, type='n', ylim=c(-0.45, 0.12), xlim=c(0,32)) 

### the grid system below is a bit warped
### the logic is to plot a bar for each of the 27 treatments, sorted by number of peers and then compliance level
xStart<-1;
for (peers in 1:6){	
	# add box with grid
	x1<-xStart - 0.75; x2<-xStart + peers + 0.75;
	y1<- -0.25; y2<-0.12;
	rect(x1,y1,x2,y2)	

	#add gridlines
#	for (i in -2:1) arrows(x1, i/10,x2,i/10, code=0, col='grey50')
	# create pseudoaxis
	if (peers>=1) {
		for (i in -2:1){
			arrows(x1-0.2, i/10,x1,i/10, code=0, col='grey50')
			if (i==0) arrows(x1-0.2, i/10,x1,i/10, code=0, col='black', lwd=2)
			if (peers==1) text(x1-0.1, i/10, format(i/10,nsmall=1), pos=2, cex=1.5)
		}
	}

	#add bars reflecting data
	par(plt=c(0,1,0,1), new=T)
	a0<-subset(a, a$numberOfPeers==peers)
	## jumper indicates the number of peers jumping the red light
	for (jumper in 0:peers){
		b<-subset(a0, a0$peersViolate==jumper)
		
		# compare with baseline compliance WITHOUT control questions 
		b0<-subset(b, b$control_questions==0)		
		n0<-nrow(b0)
		y0<-peers0viol_noControlQ - mean(b0$violate2) 

		# compare with baseline compliance WITH control questions 
		b1<-subset(b, b$control_questions==1)		
		n1<-nrow(b1)
		y1<-peers0viol_yesControlQ - mean(b1$violate2) 
		
		# calculate a weighted mean
		y<- n0/(n0+n1) * y0 + n1/(n0+n1) * y1
	
		# plot the mean
		xpos<-xStart+peers-jumper
		colour<-cols[1]
		if (y > 0) colour<-cols[2]
		w<-0.4
		rect(xpos - w, 0, xpos + w, y, col='white')		
		rect(xpos - w, 0, xpos + w, y, col=adjustcolor(colour, alpha=0.85), border='black')

		nFollow<-peers-jumper
		nViolate<-jumper
		
		# add a cartoon version reflecting the treatment (angels and demons created in powerpoint)
		y<--0.3; f<-0;
		while(f < nFollow){
			rasterImage(angel,xpos-w/1.5, y, xpos + w/1.5, y+0.03)
			y<-y-0.03
			f<-f+1;
		}
		v<-0;
		while(v < nViolate){
			rasterImage(demon,xpos-w/1.5, y, xpos + w/1.5, y+0.03)
			y<-y-0.03
			v<-v+1;
		}

	}	
	
	# draw the 0 line (should be done after the bars)
	arrows(x1, 0,x2,0, code=0, col='black', lwd=2)
	
	## add axis labels ##
	xStart<-xStart+peers+2;
}

##### as a supplementary figure, plot absolute compliance levels with and without control questions

w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1, cex.lab=1, lend=1, plt=c(0,1,0,1))
plot(0, type='n', ylim=c(-0.25, 1), xlim=c(0,32)) 

b<-subset(a, a$numberOfPeers==0)
c<-subset(b, b$control_questions==0)
baseline0<-mean(c$comply2)
c<-subset(b, b$control_questions==1)
baseline1<-mean(c$comply2)

### the grid system below is a bit warped
### the logic is to plot a bar for each of the 27 treatments, sorted by number of peers and then compliance level
xStart<-1;
for (peers in 1:6){	
	# add box with grid
	x1<-xStart - 0.75; x2<-xStart + peers + 0.75;
	y1<- 0; y2<-1;
	rect(x1,y1,x2,y2)
	arrows(x1,baseline0,x2,baseline0,code=0,lty=2)
	arrows(x1,baseline1,x2,baseline1,code=0,lty=1)

	#add gridlines
#	for (i in -2:1) arrows(x1, i/10,x2,i/10, code=0, col='grey50')
	# create pseudoaxis
	if (peers>=1) {
		for (i in 0:5){
			arrows(x1-0.2, i/5,x1,i/5, code=0, col='grey50')
			if (i==0) arrows(x1-0.2, i/5,x1,i/5, code=0, col='black', lwd=2)
			if (peers==1) text(x1-0.1, i/5, format(i/5,nsmall=1), pos=2, cex=1.5)
		}
	}

	#add bars reflecting data
	par(plt=c(0,1,0,1), new=T)
	a0<-subset(a, a$numberOfPeers==peers)
	## jumper indicates the number of peers jumping the red light
	for (jumper in 0:peers){
		b<-subset(a0, a0$peersViolate==jumper)

		b0<-subset(b, b$control_questions==0)		
		b1<-subset(b, b$control_questions==1)		
	
		m0<-mean(b0$comply2)
		m1<-mean(b1$comply2) 
		
		# calculate a weighted mean
		y<- n0/(n0+n1) * y0 + n1/(n0+n1) * y1
	
		# plot the mean
		xpos<-xStart+peers-jumper
		colour0<-cols[1]
		colour1<-cols[1]
		if (m0 > baseline0) colour0<-cols[2]
		if (m1 > baseline1) colour1<-cols[2]
		w<-0.4
		arrows(xpos, m0, xpos, m1, col='black', code=0, lwd=1.5)

		points(xpos, m0, pch=16, cex=1.5, col='white', lwd=1.5)
		points(xpos, m0, pch=1, cex=1.5, col='black', lwd=1.5)
		points(xpos, m1, pch=16, cex=1.5, col='black')

		nFollow<-peers-jumper
		nViolate<-jumper
		
		# add a cartoon version reflecting the treatment (angels and demons created in powerpoint)
		y<--0.05; f<-0;
		while(f < nFollow){
			rasterImage(angel,xpos-w/1.5, y, xpos + w/1.5, y+0.03)
			y<-y-0.03
			f<-f+1;
		}
		v<-0;
		while(v < nViolate){
			rasterImage(demon,xpos-w/1.5, y, xpos + w/1.5, y+0.03)
			y<-y-0.03
			v<-v+1;
		}

	}	
	
	# draw the 0 line (should be done after the bars)
	arrows(x1, 0,x2,0, code=0, col='black', lwd=2)
	
	## add axis labels ##
	xStart<-xStart+peers+2;
}



a$male<-ifelse(a$gender=='male',1,0)


a$treat<-paste('p', a$numberOfPeers, '_v', a$peersViolate, '_c', a$peersComply, sep='')

### this is the model presented in Table S4
m2a<-lm(comply2 ~ comply1 + treat + 
	age + male + 
	control_questions, data=a)

## these are robustness tests; logistic model, and a model with controls
m2b<-glm(comply2 ~ comply1 + treat + 
	age + male + 
	control_questions, family='binomial', data=a)

m2c<-lm(comply2 ~ comply1 + treat + 
	age + male + 
	patience + risk_taking + 
	guilt_neg + guilt_repair + shame_neg + shame_withdraw + 
	extraversion + agreeableness + conscientiousness + emotional_stability + openness + 
	 condCoop + 
	control_questions, data=a)


### output tables
stargazer(m2a, type="html", ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE, star.cutoffs=c(0.05/27,0.01/27,0.001/27), out="Table S4.html")
stargazer(m2a, m2b, m2c, type="html", ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE, star.cutoffs=c(0.05/27,0.01/27,0.001/27), out="Table S4_robustness.html")

########## PEER EFFECTS MODEL SIMPLIFIED #######
### we will now consider the fraction of 'peers' violating (this means that the no-peers baseline cannot be in the model)
# specify cases where ALL peers either complied or vioated
a$allComply<-ifelse(a$peersComply==a$numberOfPeers,1,0)
a$allViolate<-ifelse(a$peersViolate==a$numberOfPeers,1,0)
a$peersPresent<-sign(a$numberOfPeers)

a$fracPeersComply<-ifelse(a$numberOfPeers==0,0,a$peersComply / a$numberOfPeers)

### these are the models presented in Table S5
m2a<-lm(comply2 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 
	control_questions, data=subset(a, a$peersPresent==1))

m2b<-lm(comply2 ~ comply1 + fracPeersComply * numberOfPeers + 
	allComply + allViolate +
	age + male + 
	control_questions, data=subset(a, a$peersPresent==1))


### these are robustness tests
m2c<-glm(comply2 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 
	control_questions, family='binomial', data=subset(a, a$peersPresent==1))

m2d<-lm(comply2 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 
	patience + risk_taking + 
	guilt_neg + guilt_repair + shame_neg + shame_withdraw + 
	extraversion + agreeableness + conscientiousness + emotional_stability + openness + 
	 condCoop + 	
	control_questions, data=subset(a, a$peersPresent==1))


### output tables #######

stargazer(m2a, m2b, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S5.html")
stargazer(m2a, m2b, m2c,m2d, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S5_robustness.html")


##################### PEER EFFECTS iteration 3 #################
### this is Table S6
m3a<-lm(comply3 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 
	control_questions, family='binomial', data=subset(a, a$peersPresent==1))

m3b<-lm(comply3 ~ comply1 + fracPeersComply * numberOfPeers + 
	allComply + 
	allViolate + 
	age + male + 
	control_questions, family='binomial', data=subset(a, a$peersPresent==1))

### robustness tests (logistic model and model with additional predictors)

m3c<-glm(comply3 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 
	control_questions, family='binomial', data=subset(a, a$peersPresent==1))
m3d<-lm(comply3 ~ comply1 + fracPeersComply * numberOfPeers + 
	age + male + 	patience + risk_taking + 
	guilt_neg + guilt_repair + shame_neg + shame_withdraw + 
	extraversion + agreeableness + conscientiousness + emotional_stability + openness + 
	 condCoop + 
	control_questions, family='binomial', data=subset(a, a$peersPresent==1))

### output table #######

stargazer(m3a, m3b, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S6.html")
stargazer(m3c, m3d, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S6_robustness.html")


#### Table testing the patterns observed in Fig. 3C
### normative expectations 
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 3, startRow = 1, colNames = TRUE)
### 
## Figure 3C just plots the parameter estimates for each treatment (they compare appropriateness rating with no-peers baseline)

a$treat<-relevel(factor(a$treatment), ref=5)

m1<-lm(treatmentViolateRating ~ treat + gender + age , data=a)
m2<-lm(treatmentComplyRating ~ treat + gender + age , data=a)

###treat1: 1 comply, 0 violate
###treat2: 0 comply, 1 violate
###treat3: 6 comply, 0 violate
###treat4: 0 comply, 6 violate


stargazer(m1,m2, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05/4,0.01/4,0.001/4), out="Table S7.html")

############# EXPERIMENTS INTRODUCING EXTERNATLITIES AND PUNISHMENT #########

### Statistics supporting Fig. 4 ####

#a<-read.table('TLT with and without externalities - Appropriateness.txt', sep='\t', header=TRUE)

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 10, startRow = 1, colNames = TRUE)

a$treat<-factor(a$treatment)
m1<-lm(violateRating ~ treat + age + gender, data=a)
m2<-lm(complyRating ~ treat + age + gender, data=a)

### empirical expectations ####
#a<-read.table('TLT with and without externalities - Expectations.txt', sep='\t', header=TRUE)
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 11, startRow = 1, colNames = TRUE)

a$treat<-factor(a$treatment)
m3<-lm(estimateComply ~ treat+ age + gender, data=a)

stargazer(m1,m2,m3, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S8.html")

summary(glht(m3, linfct = mcp(treat = "Tukey"), alternative='greater'))


############## CONDITIONAL PREFERENCES ############
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 12, startRow = 1, colNames = TRUE)

### transform the dataset so that each decision is on a new row
b<-matrix(nrow=0, ncol=8)
for (i in 1:nrow(a)){

	male<-ifelse(a$gender[i]==1, 1,0)
	for (j in 1:5){
		newRow<-c(a$session[i]*1000 + a$playerNr[i], a$treatment[i], a$externalities[i], a$detectionProb[i], ((j-1)*20+10)/100, 1-a[i,6+j],male,a$age[i])
		b<-rbind(b, newRow)
	}
}
b<-data.frame(b)
names(b)<-c('participant','treatment', 'externalities', 'detectionProb', 'condition','comply','male','age')
rownames(b)<-c()

a<-b

a$treat<-factor(a$treatment)

### linear models shown in Table S9
m1a<-lmer(comply ~ treat + condition + age + male + (1|participant), data=a)
m1b<-lmer(comply ~ treat * condition + age + male + (1|participant), data=a)

### robustness tests (take a while to run so commented out here)
#m1a_logistic<-glmer(comply ~ treat + condition + age + male + (1|participant), family='binomial', data=a)
#m1b_logistic<-glmer(comply ~ treat * condition + age + male + (1|participant), family='binomial', data=a)


######### EMPIRICAL EXPECTATIONS #####
#a<-read.table('TLT with externalities and punishmnent - Conditional compliance - empirical.txt', header=TRUE)
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 13, startRow = 1, colNames = TRUE)

### transform the dataset so that each decision is on a new row
b<-matrix(nrow=0, ncol=8)
for (i in 1:nrow(a)){

	male<-ifelse(a$gender[i]==1, 1,0)
	for (j in 1:5){
		newRow<-c(a$session[i]*1000 + a$playerNr[i], a$treatment[i], a$externalities[i], a$detectionProb[i], (100-((j-1)*20+10))/100, 1-a[i,6+j],male,a$age[i])
		b<-rbind(b, newRow)
	}
}
b<-data.frame(b)
names(b)<-c('participant','treatment', 'externalities', 'detectionProb', 'condition','comply','male','age')
rownames(b)<-c()

a<-b

a$treat<-factor(a$treatment)

m2a<-lmer(comply ~ treat + condition + age + male + (1|participant), data=a)
m2b<-lmer(comply ~ treat * condition + age + male+ (1|participant), data=a)

## robustness tests (take a while to run so commented out here)
#m2a_logistic<-glmer(comply ~ treat + condition + age + male + (1|participant), family='binomial',data=a)
#m2b_logistic<-glmer(comply ~ treat * condition + age + male+ (1|participant), family='binomial',data=a)


### for stargazer output, we need to change the model object class
class(m1a) <- "lmerMod"
class(m1b) <- "lmerMod"
class(m2a) <- "lmerMod"
class(m2b) <- "lmerMod"
#class(m1a_logistic) <- "lmerMod"
#class(m1b_logistic) <- "lmerMod"
#class(m2a_logistic) <- "lmerMod"
#class(m2b_logistic) <- "lmerMod"


###### output table #########
stargazer(m1a,m1b, m2a,m2b, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S9.html")
#stargazer(m1a,m1a_logistic, m1b, m1b_logistic, m2a, m2a_logistic, m2b, m2b_logistic, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S9_robustness.html")

#########################

### behavioural task; Fig 4E ###

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 9, startRow = 1, colNames = TRUE)

a$move1<-ifelse(a$move1<20000, a$move1, 20000)
a$move2<-ifelse(a$move2<20000, a$move2, 20000)
a$move3<-ifelse(a$move3<20000, a$move3, 20000)
a$comply1<-ifelse(a$move1<12000,0,1)
a$comply2<-ifelse(a$move2<12000,0,1)
a$comply3<-ifelse(a$move3<12000,0,1)
a$male<-2-a$gender

a$treat<-factor(a$treatment)
a$trN<-ifelse(a$treatment==0, 'NE', ifelse(a$treatment==1, 'NP', ifelse(a$treatment==2, 'WP', 'SP')))

### in these experiments we did not consider any peers so we simply take the average compliance levels across three solo decisions
a$meanComply<-NA;
for (i in 1:nrow(a)){
	a$meanComply[i]<-mean(c(a$comply1[i], a$comply2[i], a$comply3[i]))
}

m1<-lm(meanComply ~ treat + age + male, data=a)
m1b<-glm(meanComply ~ treat + age + male, family='binomial', data=a)
summary(m1)

###### output table #########
stargazer(m1, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), out="Table S10.html")
stargazer(m1, m1b, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, single.row=TRUE,star.cutoffs=c(0.05,0.01,0.001), family='binomial', out="Table S10_robustness.html")

summary(glht(m1, linfct = mcp(treat = "Tukey"), alternative='greater'))


##### Figure 4 plot #### 

### this is A LOT of code to produce the 5 panels of Fig. 4

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 9, startRow = 1, colNames = TRUE)
b<-subset(a, a$treatment==0)

## do some calculations (not very elegant)
a$violate1<-ifelse(a$move1<12000,1,0)
a$violate2<-ifelse(a$move2<12000,1,0)
a$violate3<-ifelse(a$move3<12000,1,0)

### as above, simply take the average across three iterations of the solo task
a$meanViol<-NA;
for (i in 1:nrow(a)){
	a$meanViol[i]<-mean(c(a$violate1[i], a$violate2[i], a$violate3[i]))
}

N<-c()
violRates<-c(); cnt<-1;
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
	# take the compliance rate across the three iterations
	violRates[cnt]<-mean(b$meanViol)
	N[cnt]<-nrow(b)
	cnt<-cnt+1;
}

### calculate mean compliance rates and SEs
complyRates<-1-violRates
complyRates
ses<- 1.96 * sqrt(complyRates * (1-complyRates) / N)
SEs<- sqrt(complyRates*(1-complyRates)/N)

##################
### define plotting area (we start all the way on the right)

par(plt=c(0.84, 0.99, 0.05, 0.95), cex.axis=1.5, lend=1)
plot(0, type='n', xlim=c(0.5, 4.5), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
axis(1, at=1:4, labels=FALSE)
axis(2, at=0:5/5,labels=FALSE)

## define the dot types for the simulation results
pchs<-c(1,16,15,17)

## show compliance rates as bars
for (i in 1:4){
	rect(i-0.4,0,i+0.4, complyRates[i], col='#50b47b', lwd=2)
	arrows(i,complyRates[i]-SEs[i],i,complyRates[i]+SEs[i], code=0,lwd=3)
}

### ADD SIMULATIONS RESULTS ####
## read simulation outputs for the relevant conditions
a<-read.table('simulation_compliance_vary_phi_N500.txt', sep='\t', header=TRUE)
# add simulation points to the plot
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)

	## for each of the balance levels of descriptive vs injunctive norms
	for (p1 in 0:20/20){
		d<-subset(b, b$p==p1)
		y<-mean(d$complianceRate)
		
		x<- tr+0.6 + p1*0.8
		for (i in 1:nrow(d)){
			y<-d$complianceRate[i]
			points(x,y, pch=16, cex=0.05, col=adjustcolor('red', alpha=0.05))
	
		}		
	}
	for (p1 in 0:4/5){
		d<-subset(b, floor(b$p*5)/5 ==p1)
		y<-mean(d$complianceRate)
		x<- tr+0.6 + (p1+0.1)*0.8
		points(x,y, pch=pchs[tr+1], cex=0.8, col=adjustcolor('black', alpha=1))
	}
	
	## add some grid lines to make the reading easier
	arrows(tr+0.6, 0.95, tr+1.4, 0.95, code=0)
	for (i in 0:2/2){
		arrows(tr+0.6+i*0.8, 0.94, tr+0.6+i*0.8, 0.95, code=0)
	}
	
	
}

## panel A (appropriateness ratings)
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 10, startRow = 1, colNames = TRUE)

### define plotting area
par(plt=c(0.05, 0.25, 0.05, 0.95), new=TRUE)
plot(0, type='n', xlim=c(-0.5, 3.5), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
axis(1, at=0:3, labels=FALSE)
axis(2, at=0:5/5,labels=FALSE)

## some matrices and variables for bookkeeping of the ratings
appropriatenessMatrixComply<-matrix(0, nrow=4, ncol=4)
appropriatenessMatrixViolate<-matrix(0, nrow=4, ncol=4)
n<-rep(0,4)

## loop through treatments and store distribution of responses
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
	for (j in 1:4){
		appropriatenessMatrixComply[tr+1,j]<-length(which(b$complyRating==j))
		appropriatenessMatrixViolate[tr+1,j]<-length(which(b$violateRating==j))
	}
	n[tr+1]<-nrow(b)
}

# start plotting
# define colours to indicate response types
cols<-c('#b94a48','#b74d86', '#6780d8', '#50b47b')

## loop through treatments and plot distributions as stacked bars
for (tr in 0:3){
	comp<-appropriatenessMatrixComply[tr+1,]/n[tr+1]
	comp<-c(0, cumsum(comp))
	
	viol<-appropriatenessMatrixViolate[tr+1,]/n[tr+1]
	viol<-c(0, cumsum(viol))
	
	for (i in 1:4){
		rect(tr-0.4, comp[i], tr-0.02, comp[i+1], col=cols[i], lwd=2)
		rect(tr+0.02, viol[i], tr+0.4, viol[i+1], col=cols[i], lwd=2)
	if (i<3) {
		rect(tr-0.4, comp[i], tr-0.02, comp[i+1], col='black',lwd=1.5, density=10)
		rect(tr+0.02, viol[i],tr+0.4, viol[i+1], col='black', lwd=1.5, density=10)
				
		}
	}
}


### panel B: descriptive norms ###
# set plotting area for this panel
par(plt=c(0.3, 0.45, 0.05, 0.95), new=TRUE)
plot(0, type='n', xlim=c(-0.5, 3.5), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
axis(1, at=0:3, labels=FALSE)
axis(2, at=0:5/5,labels=FALSE)

# load data
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 11, startRow = 1, colNames = TRUE)

## a function to find the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Ns<-c()

#loop through treatments and plot the distributions
for (tr in 0:3){

	b<-subset(a, a$treatment==tr)
	Ns<-c(Ns, nrow(b))
	s<-summary(b$estimateComply/100)
	arrows(tr,s[1],tr,s[6], lwd=3, code=0)
	rect(tr-0.3,s[2],tr+0.3,s[5], col='orange', lwd=3)
	arrows(tr-0.3, s[3], tr+0.3, s[3], lwd=4, code=0)
	arrows(tr-0.3, s[4], tr+0.3, s[4], lwd=3, lty=3, code=0)
}
Ns


### panels D and E: conditional preferences ####
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 12, startRow = 1, colNames = TRUE)

## get sample sizes per treatment
Ns<-c()
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
	Ns<-c(Ns, nrow(b))
}

# define colours for plotting
cols<-c("#4a4e00","#7f63da")

## bookkeeping matrices
complMat<-matrix(0, nrow=4, ncol=5)
nMat<-matrix(0, nrow=4, ncol=5)

## loop through treatments to calculate compliance rates for each condition
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
		
	complMat[tr+1,1]<- length(which(b$disapproval.0.20==0))
	nMat[tr+1,1]<- length(which(b$disapproval.0.20==0)) + length(which(b$disapproval.0.20==1))
	
	complMat[tr+1,2]<- length(which(b$disapproval.21.40==0))
	nMat[tr+1,2]<- length(which(b$disapproval.21.40==0)) + length(which(b$disapproval.21.40==1))
	
	complMat[tr+1,3]<- length(which(b$disapproval.41.60==0))
	nMat[tr+1,3]<- length(which(b$disapproval.41.60==0)) + length(which(b$disapproval.41.60==1))
	
	complMat[tr+1,4]<- length(which(b$disapproval.61.80==0))
	nMat[tr+1,4]<- length(which(b$disapproval.61.80==0)) + length(which(b$disapproval.61.80==1))
	
	complMat[tr+1,5]<- length(which(b$disapproval.81.100==0))
	nMat[tr+1,5]<- length(which(b$disapproval.81.100==0)) + length(which(b$disapproval.81.100==1))
	
}
pMat<-complMat/nMat
seMat<-sqrt( pMat * (1 - pMat) / nMat)


### define plotting area

par(plt=c(0.5, 0.62, 0.05, 0.95), new=TRUE)
plot(0, type='n', xlim=c(0,100), ylim=c(0,1), axes=FALSE, xlab='', ylab='')
axis(1, at=c(-200,200), labels=FALSE)
axis(1, at=c(0:5)*20, labels=FALSE)
axis(2, at=0:5/5, labels=FALSE)

## save the dot styles
pchs<-c(1,16,15,17)
## set the x positions of the dots
xx<-c(10,30,50,70,90)

## loop through treatments and plot data
for (tr in 0:3){
	for (i in 1:5){
		x<-xx[i];
		# move close values in x direction for visibility
		if (tr==0) x<-xx[i]-3
		if (tr==1) x<-xx[i]+3
		arrows(x, pMat[tr+1,i]-seMat[tr+1,i], x, pMat[tr+1,i]+seMat[tr+1,i], code=0, lwd=2)
		points(x, pMat[tr+1,i], pch=pchs[tr+1], col=cols[1], cex=1.5)

	}
	xxx<-xx;
	if (tr==0) xxx<-xx-3
	if (tr==1) xxx<-xx+3
	lines(xxx[1:5], pMat[tr+1,1:5], col=cols[1])	
}

### panel D: descriptive norms (setup VERY similar to panel C)
a <- read.xlsx("Why people follow rules - data.xlsx", sheet = 13, startRow = 1, colNames = TRUE)
## get sample sizes
Ns<-c()
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
	Ns<-c(Ns, nrow(b))
}

## bookkeeping matrices
complMat<-matrix(0, nrow=4, ncol=5)
nMat<-matrix(0, nrow=4, ncol=5)

## loop through treatments
for (tr in 0:3){
	b<-subset(a, a$treatment==tr)
		
	complMat[tr+1,1]<- length(which(b$violate.0.20==0))
	nMat[tr+1,1]<- length(which(b$violate.0.20==0)) + length(which(b$violate.0.20==1))
	
	complMat[tr+1,2]<- length(which(b$violate.21.40==0))
	nMat[tr+1,2]<- length(which(b$violate.21.40==0)) + length(which(b$violate.21.40==1))
	
	complMat[tr+1,3]<- length(which(b$violate.41.60==0))
	nMat[tr+1,3]<- length(which(b$violate.41.60==0)) + length(which(b$violate.41.60==1))
	
	complMat[tr+1,4]<- length(which(b$violate.61.80==0))
	nMat[tr+1,4]<- length(which(b$violate.61.80==0)) + length(which(b$violate.61.80==1))
	
	complMat[tr+1,5]<- length(which(b$violate.81.100==0))
	nMat[tr+1,5]<- length(which(b$violate.81.100==0)) + length(which(b$violate.81.100==1))
	
}
pMat<-complMat/nMat
seMat<-sqrt( pMat * (1 - pMat) / nMat)

### set plotting area
par(plt=c(0.67, 0.79, 0.05, 0.95), new=TRUE)
plot(0, type='n', xlim=c(0,100), ylim=c(0,1), axes=FALSE, xlab='', ylab='')
axis(1, at=c(-200,200), labels=FALSE)
axis(1, at=c(0:5)*20, labels=FALSE)
axis(2, at=0:5/5, labels=FALSE)

# loop through treatments and plot the dots
for (tr in 0:3){
	for (i in 1:5){
		x<-xx[6-i];
		# move close values in x direction for visibility
		if (tr==0) x<-xx[6-i]-3.5
		if (tr==1) x<-xx[6-i]+3.5
		arrows(x, pMat[tr+1,i]-seMat[tr+1,i], x, pMat[tr+1,i]+seMat[tr+1,i], code=0, lwd=2)
		points(x, pMat[tr+1,i], pch=pchs[tr+1], col=cols[2], cex=1.5)

	}
	xxx<-xx;
	if (tr==0) xxx<-xx-3
	if (tr==1) xxx<-xx+3
	lines(xxx[6-1:5], pMat[tr+1,1:5], col=cols[2])	
}
