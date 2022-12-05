### demographics of participants of Fig. 4
install.packages('openxlsx')
library("openxlsx")

summaryMatrix<-matrix(0, nrow=13, ncol=4)

setwd('C:/Users/lmollem/Desktop/Behavioral anatomy of rule following/repository/Data')

rowTitles<-c('Behavioral task', 'Nottingham students (Fig. 1 caption)', 'Normative beliefs (Fig. 2A)', 'Normative beliefs (Fig. 3C)', 
	'Descriptive beliefs (Fig. 2B)', 'Conditional preferences (normative; Fig. 2C)', 'Conditional preferences (descriptive; Fig. 2D)', 
	'Cross task', 'Behavioural task with externalities (Fig. 4E)', 
	'Externalities - normative beliefs (Fig. 4A)', 'Externalities - normative beliefs (Fig. 4B)',
	'Externalities - Conditional prefs (normative; Fig. 4C)', 'Externalities - Conditional prefs (descriptive; Fig. 4D)')


cnt<-1;
for (sheetNr in c(1:12)){
	a <- read.xlsx("Why people follow rules - data.xlsx", sheet = sheetNr, startRow = 1, colNames = TRUE)
	
	if (sheetNr==3) a<-subset(a,a$treatment==5)
	
	summaryMatrix[cnt,1]<-rowTitles[cnt]
	summaryMatrix[cnt,2]<-nrow(a)
	summaryMatrix[cnt,3]<-paste(round(mean(a$age, na.rm=T),2), " (", round(sd(a$age, na.rm=T),2), ")", sep="")
	if (sheetNr>=8) summaryMatrix[cnt,4]<-round(length(which(a$gender==1))/length(!is.na(a$gender)) * 100)/100
	if (sheetNr<=7) summaryMatrix[cnt,4]<-round(length(which(a$gender=="male"))/length(!is.na(a$gender)) * 100)/100
	
	cnt<-cnt+1;
	if (sheetNr==3) {
		a <- read.xlsx("Why people follow rules - data.xlsx", sheet = sheetNr, startRow = 1, colNames = TRUE)
		a<-subset(a, a$treatment<5)
		summaryMatrix[cnt,1]<-rowTitles[cnt]
		summaryMatrix[cnt,2]<-nrow(a)
		summaryMatrix[cnt,3]<-paste(round(mean(a$age, na.rm=T),2), " (", round(sd(a$age, na.rm=T),2), ")", sep="")
		summaryMatrix[cnt,4]<-round(length(which(a$gender=="male"))/length(!is.na(a$gender)) * 100)/100
		cnt<-cnt+1;
	}
}

a <- read.xlsx("Why people follow rules - data.xlsx", sheet = sheetNr, startRow = 1, colNames = TRUE)
if (sheetNr==3) a<-subset(a,a$treatment==5)



summaryMatrix
