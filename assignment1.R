# Anything following a hashtag (#) is a comment, and is ignored. This is the file
# where you save your R code that produces the objects (graphs, tables) that you 
# want to embed into  your assignment.  Make sure that there are no errors when 
# you source this file before sourcing it from assignment1.Rmd file.
# To show you how it is done I have included some code below.

rm(list=ls()) #makes sure that your work environment is clean.
library("tidyverse") #we use functions from this library.
slash <- ifelse(.Platform$OS.type=="unix", "/", "\\") #The eternal directory battle: Windows vs. *nix
##########which section of the course are you in?
section <- 1 # set section to either 1 or 2, depending on what section you are in.
#################################################

mydf <- read_csv(paste("publicdata",section,slash,"mm.csv",sep=""))%>%
  mutate(Rounds=ifelse(round<11,"1-10","11-20"))
s_and_d <- read_csv(paste("publicdata",section,slash,"supply_and_demand.csv",sep=""))
outcomes<- read_csv(paste("publicdata",section,slash,"outcomes.csv",sep=""))

plot_sd <- function(sesh,mm){
  supply <- s_and_d%>%
    filter(marketmaker==mm, session==sesh, role=="seller")
  demand <- s_and_d%>%
      filter(marketmaker==mm, session==sesh, role=="buyer")
  outcomes <- outcomes%>%
    filter(marketmaker==mm, session==sesh)
  # need to pad out the supply and demand so it plots properly
  sp <-  c(0, supply$variable, 30)
  sq <- c(0, supply$q, max(supply$q))
  supply <- data.frame(q=sq, variable=sp)
  dq <- c(0, demand$q, max(demand$q))
  dp <- c(30, demand$variable, 0)
  demand <- data.frame(q=dq, variable=dp)
  #ok, now do the plotting...
  ggplot()+
    geom_step(data=demand, aes(q,variable), direction = "vh", col="red", alpha=.25, lwd=2)+
    geom_step(data=supply, aes(q,variable), col="blue", direction = "vh", alpha=.25, lwd=2)+
    geom_text(data=outcomes, aes(q, p, label=round), position="jitter", size=1.5, width=.1,height=.1, alpha=.5)+
    annotate("text", x = 5, y = 15, label = "Hi!", col="green", size=7)+
    annotate("text", x = tail(demand,1)$q, y = tail(demand,1)$variable, label = "D", col="red", size=7)+
    annotate("text", x = tail(supply,1)$q, y = tail(supply,1)$variable, label = "S", col="blue", size=7)+
    xlab("quantity")+ 
    ylab("price")+ 
    ggtitle(paste("Rounds: ", sesh, " Market maker: ", mm, sep=""))
}

(first_plot <- plot_sd("1-10","gain")) #plot supply and demand for rounds 1-10 where marketmaker makes a gain.
ggsave(file="first_plot.jpeg",plot=first_plot)

