# n-person stag hunt sim

library(rethinking)

# N_groups : number of groups
# N : number of individuals in each group
# N_rounds : number of rounds for simulation
# L : payoff sensitivity in choice ("inverse heat")
# g : updating rate
# payoffs : list of c(max,min) payoff pairs, one pair for each strategy
# theta : importance of social learning for exploration
# phi : importance of payoff for social learning
# x : rate of comparison across groups

sim_stag <- function( N_groups=2 , N=5 , N_rounds=20 , L=1 , g=0.5 , payoffs=list(c(10,2),c(8,4),c(6,5)) , theta=0 , phi=0 , x=0 ) {
	
	# count strategies
	n_strats <- length(payoffs)
	# prep payoffs
	pay_max <- sapply(1:n_strats,function(j) payoffs[[j]][1] )
	pay_min <- sapply(1:n_strats,function(j) payoffs[[j]][2] )

	# make a list of permanent individual IDs that can track across groups
	N_id <- N_groups*N
	id <- 1:N_id

	# assign groups
	group_id <- rep( 1:N_groups , each=N )

	# init attraction scores
	A <- matrix( 0 , nrow=N_id , ncol=n_strats )

	# init history
	b <- sample( 1:n_strats , size=N_id , replace=TRUE )
	b_hist <- matrix( NA , nrow=N_id , ncol=N_rounds )

	# init payoffs to zero (for payoff-biased learning)
	pay_per_strat <- matrix( 5 , nrow=N_groups , ncol=n_strats )

	# loop!
	for ( i in 1:N_rounds ) {

		# compute prob of each strat for each player
		p <- sapply( 1:N_id , function(j) exp( L*A[j,] ) / sum( exp( L*A[j,] ) ) )
		p <- t(p)

		# social learning?
		# count n per strat
		n_per_strat <- matrix( NA , nrow=N_groups , ncol=n_strats )
		for ( k in 1:N_groups ) {
			n_per_strat[k,] <- sapply( 1:n_strats , function(j) sum( b[ group_id==k ]==j ) )
		}

		# behavior
		s <- matrix( NA , nrow=N_id , ncol=n_strats )
		for ( j in 1:N_id ) {

			# payoff weighted proportions of each strategy for social influence
			# use replicator dynamic : Prob(j) = n/N + phi*( pi_self - pi_other )
			k <- group_id[j]
			if ( runif(1) < x ) {
				# look in another group
				kk <- urn( (1:N_groups)[-k] , size=1 )
				#s[j,] <- n_per_strat[kk,]/N + ifelse( n_per_strat[kk,]>0 , 1 , 0 )*( phi*( pay_per_strat[kk,] - pay_per_strat[k,b[j]] ) )
				f <- n_per_strat[kk,] * exp( pay_per_strat[kk,] - pay_per_strat[k,b[j]] )^phi
				s[j,] <- f / sum(f)
			} else {
				# own group
				#s[j,] <- n_per_strat[k,]/N + ifelse( n_per_strat[k,]>0 , 1 , 0 )*( phi*( pay_per_strat[k,] - pay_per_strat[k,b[j]] ) )
				f <- n_per_strat[k,] * exp( pay_per_strat[k,] - pay_per_strat[k,b[j]] )^phi
				s[j,] <- f / sum(f)
			}
			#s[j,] <- s[j,] / sum( s[j,] )

			p[j,] <- (1-theta)*p[j,] + theta*s[j,]

			b[j] <- sample( 1:n_strats , size=1 , prob=p[j,] )

		}

		# payoffs - assessed within each group
		pay_per_strat <- matrix( NA , nrow=N_groups , ncol=n_strats )
		for ( k in 1:N_groups ) {
			n_per_strat <- sapply( 1:n_strats , function(j) sum( b[ group_id==k ]==j ) )
			pay_per_strat[k,] <- sapply( 1:n_strats , 
				function(j) pay_min[j] + n_per_strat[j]/N * (pay_max[j]-pay_min[j]) )
		}

		# update attraction scores
		for ( j in 1:N_id ) {
			pay_vec <- rep( 0 , n_strats )
			pay_vec[ b[j] ] <- pay_per_strat[ group_id[j] , b[j] ]
			A[j,] <- ( 1-g )*A[j,] + g*pay_vec
		}

		b_hist[,i] <- b

	}#i

	return(b_hist)

}

# sample function that doesn't bug out when x is a single integer
urn <- function( x , ... ) {
	if ( length(x)>1 )
		return( sample( x , replace=TRUE , ... ) )
	else
		return(x)
}

# blank2(w=3)
N_players <- 5
N_groups <- 4
b <- sim_stag( N_groups=N_groups , N=N_players , N_rounds=150 , g=0.2 , L=0.75 , payoffs=list( c(10,2) , c(8,3) , c(6,4) ) , theta=0.1 , phi=0 , x=0 )

plot( NULL , xlim=c(1,ncol(b)) , ylim=c(1,nrow(b)) , xlab="round" , ylab="player" )
for ( i in 1:ncol(b) ) text( rep(i,nrow(b)) , 1:nrow(b) , b[,i] )
for ( i in 1:(N_groups-1) ) abline( h=N_players*i + 0.5 , lty=2 )

