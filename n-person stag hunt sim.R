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

sim_stag <- function( N_groups=2 , N=5 , N_rounds=20 , L=1 , g=0.5 , payoffs=list(c(10,2),c(8,4),c(6,5)) , theta=0 , phi=0 ) {
	
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
	b_hist <- matrix( NA , nrow=N_id , ncol=N_rounds )

	# loop!
	for ( i in 1:N_rounds ) {

		# compute prob of each strat for each player
		p <- sapply( 1:N_id , function(j) exp( L*A[j,] ) / sum( exp( L*A[j,] ) ) )
		p <- t(p)

		# social learning?
		# goes here

		# behavior
		b <- rep( NA , N_id )
		for ( j in 1:N_id ) {
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

b <- sim_stag( N_groups=3 , N=5 , N_rounds=100 , g=0.15 , L=0.75 , payoffs=list( c(10,2) , c(8,3) , c(6,4) ) )

plot( NULL , xlim=c(1,ncol(b)) , ylim=c(1,nrow(b)) )
for ( i in 1:ncol(b) ) text( rep(i,nrow(b)) , 1:nrow(b) , b[,i] )
