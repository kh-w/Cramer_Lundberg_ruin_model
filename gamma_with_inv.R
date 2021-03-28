library(pracma)
# Theta loop
for (th in seq(-1,0.5,0.1)){
  
  graph = c();
  
  theta   <- round(th,1)+ifelse(th==0,0.001,0);
  # loading such that Net Profit Condition holds
  lab     <- 1;                
  # lambda of the poisson distribution
  EX      <- 1;                
  # expected value of claims distribution
  r       <- 0.05;                
  # investment income
  n       <- 400+4000*(1-round(th/(th+0.000001),0));
  # each simulation generates 400 claims except 4400 claims for u = 0
  nSim    <- 10000;            
  # number of simulations, each simulation is a U process
  c.      <- (1+theta)*lab*EX  
  # premium rate with loading
  alpha   <- 2;                
  # shape of gamma (k)
  rate    <- alpha/EX;         
  # rate (beta)
  
  matrix = c();
  
  # seed loop
  for (seed in seq(1,10,1)){
      
    set.seed(seed)
    tobeplot = c();
    case = 1;
    
    # capital loop
    for (y in seq(0, 20, 0.5)){
      u <- y                 
      # run different initial capital
      N <- rep(Inf, nSim)
      
      # simulation loop
      for (k in 1:nSim){
        Wi <- rexp(n)/lab; 
        # /lab is for standardize
        # Wi is a vector in R^n
        
        Xi <- rgamma(n, shape=alpha, scale=1/rate)
        # Xi storing the generated n claims
        ## severity has mean EX=1
        
        Ui <- rep(0,n)
        Ui[1] = u;
        for (j in c(2:n)){
          Ui[j] = Ui[j-1]*(1+r) + Wi[j-1]*c. - Xi[j-1];
        }
        # Ui storing capital amount depsite whether it hits zero at some claim time Ti
        
        ruin <- !all(Ui>=0)
        # ruin = 0 or 1 depends on whether ruin occured in the process
        
        if (ruin) N[k] <- min(which(Ui<0))
        # N is a vector storing the index of the first negative Ui in each simulation
        # Value is "inf" if no negative Ui
      }
      
      N <- N[N<Inf]; 
      # A vector which only stores the index of ruin time of each simulation 
      
      # show progress during the run
      print("-")
      print("-")
      print("-")
      print("-")
      print(paste("Theta =",th))
      print(paste("Seed =",seed))
      print(paste("u =",u))
      print(paste("Ruined =",length(N),"/",nSim))
      print("-----------------")
      
      tobeplot[case] = (nSim - length(N))/nSim
      case = case + 1
    }
    matrix = cbind(matrix,tobeplot);
  }
  
  average = c();
  for (j in seq(0, 41, 1)){
    average[j] = mean(matrix[j,]);
  }
  
  graph = cbind(graph,average);
  
  
  ##############################################################################
  ###  Plots comparison
  ##############################################################################
  
  # Export plots as pdf
  pdf(paste0("/Investment/gamma/plots_theta_",gsub("\\.","",toString(theta)),".pdf"))
  
  # Simulation plot
  plot(seq(0, 20, 0.5),graph[,1],
       xlim=c(0,22),ylim=c(0,1.25),col="blue",
       xlab="Initial Capital (u)",ylab="Pr(Survive)",
       main=paste0("Claim Distribution: Gamma(alpha=2,rate=2) (Theta = ",round(theta,1),")"),
       type="p",pch=19)
  legend(0, 1.15, c("Simulation"), col=c("blue"), pch=19);
  
  dev.off()
  
  #rm(list=ls())
}