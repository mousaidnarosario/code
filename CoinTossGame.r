###############################################
#title: "Coin toss game"
#author: "Mousaidna Rosario"
################################################

# Game mechanics: 
# 1. Guess the coin toss from h (head) and t (tail) with an initial $10.
# 2. Each lose will take a dollar and win will add a dollar to your money
# 3. Enter any number to continue or 0 for Exit.
# 4. After exit, the game will show how many guess/es you made.

# The game is made using R 

CoinTossGame <- function(x){
  
  #initialize function
  initial <- 1
  #initialize money
  money <- 10
  #initialize guess
  your_current_guess <- 1
  #initialize pf
  pf <- 0
  #make a vector of letters
  az <- letters[1:26]
  #make a vector of numbers
  oN <- c(1:9)
  #initialize vector guesses
  guesses <- rep(0, 51)
  #remove h and t in the list
  az <- setdiff(az, c("h","t"))
  
  while(initial != 0 ){
    
    cat("\n##########################\n")
    cat("Welcome to Coin Toss Game!\n")
    cat("##########################\n\n")
    
    cat("---------------------\n")
    cat("  RULES OF THE GAME\n")
    cat("---------------------\n")
    cat("Guess the outcome of the coin toss. Enter h for Head and t for Tail.\n")
    cat("You will have $10 initial money. We will add a dollar for every correct guess and take a dollar for every wrong answer.\n")
    cat("If you are ready, let's begin! or press 0 to exit\n")  
    
    your_current_guess <- readline(prompt="Enter your guess: \n")
    
    #initialize guess
    nguesses <- 1
    lenOfGuess <- length(guesses)	  
    toss <- 0
    thistoss <- 0
    
    #main loop
    for (toss in 1:nguesses)
    {
      #determine pf
      
      if(nguesses <= 5){
        pf <- 0.7
      }
      else{
        pf  <- cnt/nguesses
      }
      
      #for initialization purpose of toss only, check guesses[2] if empty
      if(nguesses==0){
        st <- sample((0:1),replace = TRUE)
        thistoss <- st[1]
      } else if(runif(1) < pf) {
        # we predict a fluctuation, so make this toss equal to previous guess
        thistoss = thistoss
        #cat("less; equal: ",thistoss)
      } else {
        # we predict no fluctuation, so make this toss different to previous guess
        thistoss = 1 - thistoss 
        #cat(" toss : ",toss," thistoss",thistoss," current: ",guesses[toss],"\n")
      }
      
      # h and t input reader
      
      if(your_current_guess=="h" || your_current_guess=="t"){
        guesses <- append(guesses,your_current_guess,after =0)[1:51]
        
        # changing the value of current guess to 0(t) and 1(h)
        
        if(your_current_guess=="h")
        {your_current_guess <- 1}
        else if(your_current_guess=="t")
        {your_current_guess <- 0}
        
        
        #initialize i to get current guess
        i <- 1
        #current Status flip
        f <- 0
        #initialize count status change
        cnt <- 0
        
        #loop for fluctuation
        while(i <= lenOfGuess){
          if (f!= guesses[i]) {
            # get the value of f for next comparison
            f = guesses[i]
            #count changes
            cnt = 1 + cnt
          }
          i = 1 + i
        }
        
        #main condition to determine coin flip
        
        if(thistoss==your_current_guess){
          cat("You win! Your guess is",if(your_current_guess==0){"Tail(t)"}else{"Head(h)"},".Coin is ",if(thistoss==0){"Tail(t)"}else{"Head(h)"},".Adding $1 dollar to your money..\n")
          
          #add one dollar if win 
          money <- money +1
          cat("You money now is: $",money,"\n")
          initial<- readline(prompt="Continue to Play? Choose any key or 0 for Exit: ")
        } else if(thistoss!=your_current_guess){ 
          
          #subtract one dollar if lose 
          money <- money -1
          
          if(money != 0){
            cat("You lose! Your guess is ",if(your_current_guess==0){"Tail(t)"}else{"Head(h)"},".Coin is ",if(thistoss==0){"Tail(t)"}else{"Head(h)"},".Taking $1 dollar from your money..\n")
            cat("You money now is: $",money,"\n")
            initial<- readline(prompt="Continue to Play? Choose any key or 0 for Exit: ")
          } else if(money == 0){
            cat("Your current money is $",money,". Ending the game..\n")
            initial <-0
          } 
        }
      } else if(is.element(your_current_guess, az) || is.element(your_current_guess, oN)){
        cat("WRONG INPUT! You entered",your_current_guess,".Please input h and t only. Your money is $",money,".\nExiting the game...")
        initial <- 0
        guesses <- append(guesses,your_current_guess,after =0)[1:51]
      }
      #add guesses and loop
      toss <- toss + 1
      nguesses <- nguesses+1
      
    } # end of main for loop
    cat("\nNumber guesses made: ",nguesses,"\n")
  } # end of main while loop
} 


#Run the game
CoinTossGame(1)