set.seed(1)
birthday <- function(n, nReps)
{
	s <- sample(1:365, size=n, replace = TRUE)
	if (length(unique(s))==n)
	{
		num <- 0
	}else 
	{
		num <- 1
	}
	repeat
	{
		for (i in 2:nReps)
		{
		s <- sample(1:365, size=n, replace = TRUE)
			if (length(unique(s))==n)
			{
				num <- c(0, num)
			}else 
			{
				num <- c(1, num)
			}
		}
		if (i==nReps)
	{
	break
	}
	
	}		
	z <- sum(num)/nReps
	y <- sum(num)/nReps + c(-1,1) * qnorm(1-0.1/2) * sqrt( sum(num)/nReps * ( 1-sum(num)/nReps) / nReps)
	return(c(y,z))
	
}
data <- birthday(1, 100)
for (i in 2:365)
{
	data <- c(data,birthday(i, 100))
}
plot("estimates", "n", xlim=c(0,365), ylim=c(0,1))
data1 <- matrix(data, ncol=3, byrow=TRUE)
data12 <- data1[,3]
lines(c(1:length(data12)),data12)
data21 <- data1[,1]
data31 <- data1[,2]
lines(c(1:length(data21)), data21, lty= 2)
lines(c(1:length(data31)), data31, lty=2)
formula <- 1- prod(seq(from=((364)), to=365)/365)
formula <- c(formula, sapply(2:365, function(x) 1-prod(seq(from=((365-x)), to=365)/365)))
lines(c(1:365), formula, col=2)
