
##########################
## Power Analysis performed for Pulse flow proposal in 2018
## code written by Cyril Michel cyril.michel@noaa.gov
##########################

##########################
##Define functions
##########################

## to make simulated capture history for CJS models
simul.cjs<-function(phi,p,marked)
{
  n.occasions<-length(p)+1
  Phi<-matrix(phi,n.occasions-1,nrow=sum(marked),byrow=T)
  P<-matrix(p,n.occasions-1,nrow=sum(marked),byrow=T)

  #n.occasions<-dim(Phi)[2]+1
  CH<-matrix(0,ncol=n.occasions,nrow=sum(marked))
  #define a vector with marking occasion
  mark.occ<-rep(1:length(marked),marked[1:length(marked)])
  #fill in CH
  for (i in 1:sum(marked))
  {
    CH[i,mark.occ[i]]<-1
    if (mark.occ[i]==n.occasions) next
    for(t in (mark.occ[i]+1):n.occasions)
    {
      #survive?
      sur<-rbinom(1,1,Phi[i,t-1])
      if(sur==0) break #move to next
      #recaptured?
      rp<-rbinom(1,1,P[i,t-1])
      if(rp==1) CH[i,t]<-1
    } #t
  } #i
  return(CH)
}

###function to create capture history character inp strings (only need this for RMark runs)
pasty<-function(x)
{
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n)
  {
    out[i]<-paste(x[i,],collapse="")
  }
  return(out)
}

######################
## Now setup simulations for Power analysis
######################

set.seed(50)
library(rlang)
library(magrittr)
library(cli)
library(glue)
library(vctrs)
library(RMark)

## this is the number of capture events, i.e. receiver station. So the # of reaches is n.ocass-1
n.occas<-6
## this defines release group number
marked<- c(200,300,400,500,600)
## this defines the baseline capture probability, here set to 95%
p.input<-rep(0.95,n.occas-1)
## this defines reach specific survival, reach 1 is the reach of interest. This baseline survival is from earlier versions of the analysis published here https://doi.org/10.1002/ecs2.3498, and doesn't exactly match the numbers in the paper.
phi.input <- c(0.22, 1.000, 1.000, 0.691, 1.000)
## this defines the number of simulations
num_simuls <- 300
## this defines the custom higher survival rate for the pulse group in reach 1, for this power analysis, we want a 50%, 75%, 100%, and 150% improvement in survival
improved_phi <- c(0.22*1.5, 0.22*1.75,0.22*2,0.22*2.5)

## create empty list to paste in results from loop
simuls <- list()

for(i in improved_phi){
  phi.input2 <- c(i, 1.000, 1.000, 0.691, 1.000)
  for(j in marked){
    for (k in 1:num_simuls){
      print(paste(i,j,k,sep="-"))
      CH <-rbind(simul.cjs(phi=phi.input,p=p.input,marked=j),
                 simul.cjs(phi=phi.input2,p=p.input,marked=j))
      CH.RMark<-pasty(CH)
      rmark.data<-data.frame(ch=CH.RMark, flow=c(rep("below", j), rep("above", j)))

      lfc.process <- process.data(rmark.data, model="CJS", begin.time=1, groups=c("flow") )

      lfc.ddl <- make.design.data(lfc.process)

      lfc.ddl$Phi$river <- 0
      lfc.ddl$Phi$river[lfc.ddl$Phi$time==1] <- 1

      Phi.t.plus.linear.rflow <- list(formula=~time + river:flow)
      Phi.t <- list(formula= ~time)
      p.1 <- list(formula= ~1)

      simul <- mark(lfc.process, lfc.ddl, model.parameters=list(Phi=Phi.t.plus.linear.rflow, p=p.1), realvcv=TRUE, invisible = T, silent = T, output = F)
      results <- cbind(simul$results$real[1,1:4], simul$results$real[6,1:4])
      oldnames <- colnames(results)
      colnames(results) <- paste(c(rep("above_", 4),rep("below_",4)), oldnames, sep = "")
      simuls[[paste(i,j,k,sep = "-")]] <-  cbind(improved_phi = i,samplesize = j, run_num = k, results)

      cleanup(ask=F)
    }
  }
}

## bind together results
final_simul <- do.call(rbind,simuls)

## this below logical tests for an alpha level of approximately <0.01 for a sig diff between survival for the above and below flow threshold groups. Technically, the 95%CI of one estimate should not overlap the point estimate of the other to be sig diff at an alpha level of <0.05
## if an alpha level of <0.05 is desired, one way is to make 83.4% confidence intervals, then run this below logical. To make such CIs, we will need to produce CIs ourselves on the logit scale with the beta estimates, then transform to real scale
final_simul$sig_diff <- final_simul$above_lcl > final_simul$below_ucl

simul_results <- aggregate(list(signif.models = final_simul$sig_diff), by = list(improved_phi = final_simul$improved_phi,samplesize = final_simul$samplesize), FUN = sum)
simul_results$num_simuls <- num_simuls
simul_results$percent_signif <- round(simul_results$signif.models/simul_results$num_simuls*100,1)
simul_results$precent_improvement <- round(simul_results$improved_phi / 0.22*100,0)-100

