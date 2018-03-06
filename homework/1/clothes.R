library(data.table)
# Read in the data from "clothes.csv" file into a data.frame called "clothes".
clothes <- read.csv(file="https://dahl.byu.edu/223/2018a2/hw/01/files/clothes.csv",head=TRUE,sep=",")
# Remove all rows from the data.frame where the variable "Price" is empty,
# i.e., equal to "". 
changetoNA <- function(colnum,df)
{
  {
    col <- clothes[,colnum]
    if (is.atomic(col)) {  #edit: verifying column is atomic
        col[col == "" & is.atomic(col)] <- NA
    }
    return(col)
}
df <- data.frame(sapply(1:11, changetoNA, df))
}

p <-changetoNA(2, clothes)
clothes <- data.frame(clothes$Name, p, clothes[3:11])
complete2 <- complete.cases(clothes$Name, p, clothes[3:11]) 
clothes <- clothes[complete2,]
# Write a function called "num" that takes a character vector of prices as
# input, removes the dollar sign, and returns a numeric vector of prices.
num <- function(string1,string2, df, colnum) { 
v <- df[,colnum]
v <- substring(v,string1)
v1 <- as.numeric(v)
 return	(v1) 

} 



# Using the "num" function you just wrote, convert the variable "Price" in the
# clothes data.frame from a character vector to a numeric vector.
clothes <- data.frame(clothes[,1], num(2, ".",clothes, 2), clothes[,3:11])

# Determine the standard deviation of the price of clothes for each part of the
# body, as indicated by the variable "Body.Location".
clothes <- with(clothes, clothes [order(clothes$id), ])
clothes <- with(clothes, clothes [order(clothes$Body.Location), ])
seperate <-data.frame(table(clothes[,3]))
seperate <- seperate[seperate$Freq > 0,]
seperate
x <- 2
 if(clothes[x,11] >= clothes[x-1,11]){
	a <- rbind(clothes[x-1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	a <-rbind(a, clothes[x+1,])
	x <- x+1
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <-x+1
	b <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <-x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	b <-rbind(b, clothes[x+1,])
	}
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	c <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	c <-rbind(c, clothes[x+1,])
	}
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	d <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	d <-rbind(d, clothes[x+1,])
	}
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	e <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	e <-rbind(e, clothes[x+1,])
	}
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	f <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	f <-rbind(f, clothes[x+1,])
	}
	}
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	g <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	g <-rbind(g, clothes[x+1,])
	}
	}	
	}
	if(clothes[x,11] >= clothes[x-1,11]){
	x <- x+1
	h <- rbind(clothes[x+1,], clothes[x,])
	  while(clothes[x+1,11] >= clothes[x,11]){
	x <- x+1
	if(clothes[x+1,11] >= clothes[x,11]){
	h <-rbind(h, clothes[x+1,])
	}
	}	
	}
	
ibcdefgh<- setkey(setDT(clothes))[!a]
icdefgh<- setkey(setDT(ibcdefgh))[!b]
idefgh<- setkey(setDT(icdefgh))[!c]
iefgh<- setkey(setDT(idefgh))[!d]
ifgh<- setkey(setDT(iefgh))[!e]
igh<- setkey(setDT(ifgh))[!f]
ih<- setkey(setDT(igh))[!g]
i<- setkey(setDT(ih))[!h]
locationsum <- list(sum(a[,2]),sum(b[,2]),sum(c[,2]),sum(d[,2]),sum(e[,2]),sum(f[,2]),sum(g[,2]) ,sum(h[,2]) ,sum(i[,2]))
locationsum <- unlist(locationsum)
Seperate <- seperate[,1]
seperate <- unlist(seperate$Freq)
locationmean <-locationsum/seperate
stddev <- list((sum((a[,2]-locationmean[1])^2)/(seperate[1]-1))^.5, (sum((b[,2]-locationmean[2])^2)/(seperate[2]-1))^.5, (sum((c[,2]-locationmean[3])^2)/(seperate[3]-1))^.5, (sum((d[,2]-locationmean[4])^2)/(seperate[4]-1))^.5, (sum((e[,2]-locationmean[5])^2)/(seperate[5]-1))^.5, (sum((f[,2]-locationmean[6])^2)/(seperate[6]-1))^.5, (sum((g[,2]-locationmean[7])^2)/(seperate[7]-1))^.5, (sum((h[,2]-locationmean[8])^2)/(seperate[8]-1))^.5, (sum((i[,2]-locationmean[9])^2)/(seperate[9]-1))^.5)
names(stddev) <- Seperate
stddev