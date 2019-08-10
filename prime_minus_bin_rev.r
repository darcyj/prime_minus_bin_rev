# This function computes the integer sequence "n-th prime
# minus its binary reversal" up to a maximum integer xmax.
# This sequence can be found at the Online Encyclopedia of
# Integer Sequences (OEIS) as A265326: 
# https://oeis.org/A265326
# This function returns a table of values, where the first
# column "p" is all prime numbers from zero to xmax, the
# second column "binrev" is the binary reversal of p, and
# the third column "pmbinrev" is p - binrev.
# John L. Darcy, August 2019

prime_minus_bin_rev <- function(xmax){
	require(primes)
	require(binaryLogic)
	# function to turn a binary number from as.binary into dec:
	# inspired by https://stackoverflow.com/questions/12892348/convert-binary-string-to-binary-or-decimal-value
	bin2dec <- function(x){ sum(2^(which(rev(as.vector(x)*1 == 1))-1)) }
	p <- primes::generate_primes(min=0, max=xmax)
	binrev <- sapply(X=p, FUN=function(x){bin2dec(rev(binaryLogic::as.binary(x)))})
	pmbinrev <- p - binrev
	return(data.frame( prime=p, binrev, pmbinrev ))
}


# Here is how to use the function:
stuff2plot <- prime_minus_bin_rev(50000)
plot(pmbinrev ~ prime, data=stuff2plot, pch=".",
	xlab="Prime numbers", ylab="Prime minus its binary reversal")


