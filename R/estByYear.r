library(kimisc)
library(tidyverse)
source('generalCode/estimationFunctions.r')

#if(!exists('ya')) ya <- FALSE

abvs <- c(HS='hs',Associates='cc',Bachelors='bach')


est1 <- function(edLev,des,raw=FALSE){
    abv <- if(edLev%in%names(abvs)) abvs[edLev] else 'lev'
    subsets <- list('AGEP','ageRange','raceEth','SEX',c('SEX','raceEth'))

    dat <- des$variables[,c('DEAR','attain',unique(unlist(subsets)))]
    dat$pwgtp <- des$pweights
    dat <- cbind(dat,des$repweights)
    dat[[abv]] <- dat$attain>=edLev

    if(raw){
      dat$ya <- dat$AGEP<30
      subsets <- c('DEAR','ya',subsets)
    }
    subsets <- lapply(subsets,function(sss)
      if(raw) unique(c('DEAR',sss)) else unique(c('DEAR','AGEP',sss)))

    outfun <- function(sss){
      out <- dat%>%group_by(!!! syms(sss))%>%do(x=factorProps(abv,.))
      ests <- out$x
      out$x <- NULL
      out <- bind_cols(out,do.call('bind_rows',ests))
      out <- out[,-grep('FALSE',colnames(out))]
      out <- rename(out,`I(attain >= edLev)TRUE`=`% TRUE`,se2=`TRUE SE`,Freq=n)
      out
    }


    out <- lapply(subsets, outfun)

    names(out) <- paste0(abv,c(if(raw) c('Tot','25.29','ByAge') else 'Tot',
                               'ByAgeCat','Race','Sex','RaceSex'))

    if(!raw) out[[paste0(abv,'25.29')]] <- subset(out[[paste0(abv,'Tot')]],AGEP<30)

    out
}




sampSize <- function(des){
    out <- list()
    out$bachByAge <- out$hsByAge <- xtabs(~AGEP+DEAR,des$variables,drop=TRUE)
    out$bach25.29 <- out$hs25.29 <- xtabs(~DEAR+I(AGEP<30),des$variables,drop=TRUE)
    out$bachTot <- out$hsTot <- xtabs(~DEAR,des$variables,drop=TRUE)
    out$bachByAgeCat <- out$hsByAgeCat <- xtabs( ~ageRange+DEAR,des$variables,drop=TRUE)

    out$bachRace <- out$hsRace <- xtabs(~raceEth+DEAR,des$variables,drop=TRUE)

    out$bachSex <- out$hsSex <- xtabs(~SEX+DEAR,des$variables,drop=TRUE)

    out$bachRaceSex <- out$hsRaceSex <- xtabs(~SEX+raceEth+DEAR,des$variables,drop=TRUE)

    out
}

yearInfo <- function(year,des){
    print(year)
    if(missing(des)) load(paste0('../../data/byYear/design',year,'.RData'))
    des <- subset(des,AGEP>24 & AGEP<65) ## changed this to change age range
#    des <- update(des,raceEth=ifelse(raceEth%in%c('American Indian','Asian/PacIsl'),'Other',raceEth))

    levs <- c('HS','Associates','Bachelors')

    outRaw <- do.call('c',lapply(levs,est1,des=des,raw=TRUE))
    outAdj <-  do.call('c',lapply(levs,est1,des=des,raw=FALSE))
    outSS <- sampSize(des)

    rm(des);gc()

    list(outRaw=outRaw,outAdj=outAdj,outSS=outSS)
}


