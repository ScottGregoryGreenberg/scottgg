set.seed(1)
plusone <- function(star, p1, p2, p3, p4){
   s <- sample(1:4, size=1, prob=c(p1, p2, p3, p4))
   while (!(is.element(1, s) && is.element(2, s) &&      is.element(3, s) && is.element(4,s)))
   {
	star <- star+1
	s <- c(s,sample(1:4, size=1, prob=c(p1, p2, p3, p4)))
   }
	return(star)
}
cereal <- function(star, p1, p2, p3, p4, num){
   prob <- plusone(star, p1, p2, p3, p4)
   prob <- c(sapply(2:num, function(x) plusone(star, p1, p2, p3, p4)),prob)
prob
}
#num means number of trials
num <- 100
unbias <- cereal(1, .25,.25,.25,.25, num)
bias <- cereal(1,.4,.1,.25,.25, num)
#What is the mean number of boxes that a consumer must purchase to get a complete set?
#scenario1
est <- mean(unbias)
est
est + c(-1,1) * qt(1-0.05/2,df=num-1) * sd(unbias)/sqrt(num)
#scenario2
est <- mean(bias)
est
est + c(-1,1) * qt(1-0.05/2,df=num-1) * sd(bias)/sqrt(num)
#What proportion of consumers will need to purchase 14 boxes or more to complete a set?
#scenario1
est <-length(unbias[unbias >= 14])/length(unbias)
est
est + c(-1,1) * qnorm(1-0.1/2) * sqrt( est * ( 1-est) / num)
#scenario 2
est <- length(bias[bias >= 14])/length(bias)
est
est + c(-1,1) * qnorm(1-0.1/2) * sqrt( est * ( 1-est) / num)
