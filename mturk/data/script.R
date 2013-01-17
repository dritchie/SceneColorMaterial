library(multcomp)
library(car)


datafile <- read.csv("C:\\Users\\sharon\\Documents\\ColorTransfer\\SceneColorMaterial\\mturk\\data\\perPattern.csv")
analyzeBest = FALSE

#Logistic analysis (Some other way of doing it?: http://stats.stackexchange.com/questions/5935/anova-on-binomial-data)
#Ack, this doesn't work
if (analyzeBest)
{
	fit <- glm(cbind(numBest,total-numBest)~method, data=datafile, family=binomial(link="logit"))
} else {
	fit <- glm(cbind(numWorst,total-numWorst)~method, data=datafile, family=binomial(link="logit"))
}

summary(glht(fit, linfct=mcp(method="Tukey")))
anova(fit, test="Chisq")

#Log-odds ratio is printed out under "Estimate column". 


#Just the repeated measures ANOVA
if (analyzeBest){
	myanova <- aov(numBest~method + Error(worker), datafile)
} else {
	myanova <- aov(numWorst~method + Error(worker), datafile)
}

summary(myanova)

#Tukey test, but doesn't work with error terms
if (analyzeBest){
	myanova <- aov(numBest~method, datafile)
} else {
	myanova <- aov(numWorst~method, datafile)
}
summary(glht(myanova, linfct=mcp(method="Tukey")))

#Pairwise t-test with bonferonni adjustment
attach(datafile)
if (analyzeBest) {
	pairwise.t.test(numBest, method, p.adj = "bonf", paired=T)
} else {
	pairwise.t.test(numWorst, method, p.adj = "bonf", paired=T)
}
detach()


#Aggregated CHI squared
datafile <- read.csv("C:\\Users\\sharon\\Documents\\ColorTransfer\\SceneColorMaterial\\mturk\\data\\aggregate.csv")
if (analyzeBest){
	chisquared <- chisq.test(cbind(datafile$numBest,datafile$total-datafile$numBest))
} else {
	chisquared <- chisq.test(cbind(datafile$numWorst,datafile$total-datafile$numBest))
}
chisquared


