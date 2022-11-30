# read conditional preferences and beliefs from experimental data
setwd('C:/Users/molleman/Desktop/Behavioral anatomy of rule following/data experiments with and without externalities')

#NEdata<-read.table('TLT with externalities and punishmnent - Conditional compliance - normative.txt', header=TRUE, sep='\t');
#EEdata<-read.table('TLT with externalities and punishmnent - Conditional compliance - empirical.txt', header=TRUE, sep='\t');

#normExpData<-read.table('TLT with and without externalities - Appropriateness.txt', sep='\t', header=TRUE)
#empExpData<-read.table('TLT with and without externalities - Expectations.txt', header=TRUE, sep='\t');


normExpData<-read.xlsx("Why people follow rules - data.xlsx", sheet = 9, startRow = 1, colNames = TRUE)
empExpData<- read.xlsx("Why people follow rules - data.xlsx", sheet = 10, startRow = 1, colNames = TRUE)
NEdata<-read.xlsx("Why people follow rules - data.xlsx", sheet = 11, startRow = 1, colNames = TRUE)
EEdata<-read.xlsx("Why people follow rules - data.xlsx", sheet = 12, startRow = 1, colNames = TRUE)




######### simulation settings ########

N<-500			# number of agents
S<-4			# number of sampled others to base Normative Expectations on
replicates<-100 # number of replications per prob of attendance to normative or empirical expectations

complianceMatrix<-matrix(0, nrow=0, ncol=4)
for (treat in 0:3){

	# SPECIFY AGENT'S STRATEGIES

	# define responses to Normative Expectations (NE)
	# matrix with 1 individual on each row
	# 5 columns with conditional responses (violate=0, compliance=1) to the % brackets 'others disapprove'

	NEdata1<-subset(NEdata, NEdata$treatment==treat)
	NEstrategyMatrix<-matrix(nrow=0, ncol=5)
	for (ind in unique(NEdata1$uniquePlayerNr)){
		b<-subset(NEdata1, NEdata1$uniquePlayerNr==ind)
		NEstrategyMatrix<-rbind(NEstrategyMatrix, 1-b[1,7:11])
	}

	# define responses to Empirical Expectations (EE)
	# matrix with 1 individual on each row
	# 5 columns with conditional responses (violate=0, compliance=1) to the % brackets 'others comply'

	EEdata1<-subset(EEdata, EEdata$treatment==treat)
	EEstrategyMatrix<-matrix(nrow=0, ncol=5)
	for (ind in unique(EEdata1$uniquePlayerNr)){
		b<-subset(EEdata1, EEdata1$uniquePlayerNr==ind)
		EEstrategyMatrix<-rbind(EEstrategyMatrix, b[1,7:11])
	}	
	
	# Normative Expectations as measured in the NE experiment
	# How do others rate violations
	# store (somewhat) disapprove as a 1, other cases as a 0
	normExpData1<-subset(normExpData, normExpData$treatment==treat)
	NEvector<-normExpData1$violateRating
	NEvector<-ifelse(NEvector<3,1,0)

	# Empirical Expectations as measured in the EE experiment
	empExpData1<-subset(empExpData, empExpData$treatment==treat)
	EEvector<-empExpData1$estimateComply
	EEvector<-EEvector[!is.na(EEvector)]


	# START SIMULATING: DEFINE BEHAVIOUR (compliance=1; violation=0) BASED ON STRATEGY AND ENVIRONMENT
	## iterate across relative importance of normative vs empirical expectations
	for (p in 0:20/20){
		complianceRates<-c() # an empty vector to store compliance rates for each relative reliance on normative (probability p) or empirical (probability 1-p) expecatations
		
		## iterate across simulation replicates
		for (repl in 1:replicates){
			# create empty vector to store the simulated decisions (0=violation, 1=compliance)
			complianceVector<-c()
			
			# randomize strategy profiles to create an agent
			mat1 <- NEstrategyMatrix[sample(nrow(NEstrategyMatrix), replace=FALSE),]
			NEstrategyMatrix<-mat1
			mat1 <- EEstrategyMatrix[sample(nrow(EEstrategyMatrix), replace=FALSE),]
			EEstrategyMatrix<-mat1			
			# loop for each of the N agents
			for (agent in 1:N){
				
				# define the agent's conditional responses
				NE<-NEstrategyMatrix[sample(1:nrow(NEstrategyMatrix),1),]
				EE<-EEstrategyMatrix[sample(1:nrow(EEstrategyMatrix),1),]
			
				# draw random number from uniform distribution [0,1] to define which expectations (normative or empirical) will be attended to
			

				comply<-c() #initialize variable for this agent's decision
				rnd<-runif(1)
				if (rnd<p){
					# define behaviour based on Empirical expectations
					# sample environment by drawing 1 empirical expectation
					complianceBelief<-sample(EEvector,1)
					if (complianceBelief==100) complianceBelief<-99
					relevantConditionalDecision<-1+floor(complianceBelief/20)
					comply<-EE[relevantConditionalDecision]
				}
				else{
					# define behaviour based on normative expectations
					# sample environment S times, with replacement
					disapprovalBelief<-mean(sample(NEvector,S, replace=TRUE))
					relevantConditionalDecision<-ceiling(disapprovalBelief*5)
					if (relevantConditionalDecision==0) relevantConditionalDecision<-1;
					comply<-NE[relevantConditionalDecision]
				}	

				# add decision of this agent to the vector
				complianceVector<-c(complianceVector, as.integer(comply[1]))
			}
			
			# calculate mean compliance rate for this condition, based on the vector 'complianceVector', which will have length N (so, N 0s and 1s)
			frequencyCompliance<-mean(complianceVector)

			# store in the overall vector 'complianceRates'
			complianceRates<-c(complianceRates, frequencyCompliance)
			
			##### print progress to screen #####
			if (repl%%50==0) {
				print(paste('tr=', treat, ' p=', p, ' repl=', repl, sep=''))
				flush.console()
			}
			complianceMatrix<-rbind(complianceMatrix, c(treat,p,repl,frequencyCompliance))
		}
	}
}

complianceMatrix<-data.frame(complianceMatrix)
names(complianceMatrix)<-c('treatment', 'p', 'repl', 'complianceRate')
write.table(complianceMatrix, file='simulation_results2.txt', sep='\t')

