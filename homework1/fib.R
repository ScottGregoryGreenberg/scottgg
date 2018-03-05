#Note it is a must that n>0
fib <- function(n){
	if(n>2){
		f <- rbind(1,1)
			k <- 1
		for(i in 1:(n-2)){
			k <- k+i
			f <- rbind(f,k)
		}
 return(f)
	} else if(n==2){
		f <- rbind(1,1)
	return(f)
	}
	 else if(n==1)
	return(1)
}