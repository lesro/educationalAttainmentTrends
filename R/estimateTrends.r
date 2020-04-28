library(estimatr)
library(tidyverse)
source('R/trends.r')


load('output/estByYear.RData')

### some more processing of results
for(i in 1:length(overTime)){
    ### add year as data
    for(j in 1:length(overTime[[i]])){
        overTime[[i]][[j]]$year <- i
    }
    for(j in 1:length(overTimeAge[[i]])){
        overTimeAge[[i]][[j]]$year <- i
    }
    ### separate race/sex estimates by sex
    for(lev in c('hs','cc','bach'))
        for(sex in 1:2){
            overTime[[i]][[paste0(lev,'Race',c('M','F')[sex])]] <-
                subset(overTime[[i]][[paste0(lev,'RaceSex')]],SEX==sex,select=-SEX)
            overTimeAge[[i]][[paste0(lev,'Race',c('M','F')[sex])]] <-
                subset(overTimeAge[[i]][[paste0(lev,'RaceSex')]],SEX==sex,select=-SEX)
        }

    ### estimates for ages 25-29
    overTime[[i]]$hs25.29 <- overTime[[i]]$hs25.29[-grep('FALSE',rownames(overTime[[i]]$hs25.29)),]
    overTime[[i]]$bach25.29 <- overTime[[i]]$bach25.29[-grep('FALSE',rownames(overTime[[i]]$bach25.29)),]
    overTime[[i]]$hs25.29[['I(AGEP < 30)']] <- overTime[[i]]$bach25.29[['I(AGEP < 30)']] <- NULL
}

## remname se column
for(i in 1:length(overTime))
  for(j in 1:length(overTime[[i]]))
    names(overTime[[i]][[j]]) <- gsub('se2','se',names(overTime[[i]][[j]]))

for(i in 1:length(overTimeAge))
  for(j in 1:length(overTimeAge[[i]]))
    names(overTimeAge[[i]][[j]]) <- gsub('se2','se',names(overTimeAge[[i]][[j]]))

## add 2018 age distribution for weighting regressions
load('output/ageDist18.RData')
attr(overTimeAge,'weightDat') <- ageDist18


### estimate trends

subs <- c('Tot',#'25.29',
          'ByAgeCat',
          'Sex','Race','RaceM','RaceF')#,'RaceSex')



analysis <- list()
pTrend <- list()
pDiff <- list()
for(s in subs){
    for(l in c('hs','cc','bach')){
        nn <- paste0(l,s)
        if(s%in%c('Tot','25.29')){
            analysis[[nn]] <- tabFun(nn,overTime,retEst=TRUE)
            pTrend[[nn]] <- with(analysis[[nn]],
                                 pval(ests[c('Trend','Trend (Adj)'),1],ses[c('Trend','Trend (Adj)'),1]))
            pDiff[[nn]] <- with(analysis[[nn]],
                                c(unadj=diffP(ests['Trend',],ses['Trend',]),
                                  adj=diffP(ests['Trend (Adj)',],ses['Trend (Adj)',])))
            analysis[[nn]]$SS <- getSampleSizes(nn,overTime,NULL)
        }
        else{
            analysis[[nn]] <- list(deaf=tabFun(nn,overTime,deaf=TRUE,retEst=TRUE),
                               hear=tabFun(nn,overTime,deaf=FALSE,retEst=TRUE))
            pTrend[[nn]] <- with(analysis[[nn]]$deaf,
                                 cbind(unadj=pval(ests['Trend',],ses['Trend',]),
                                       adj=pval(ests['Trend (Adj)',],ses['Trend (Adj)',])))
            pDiff[[nn]] <- with(analysis[[nn]], do.call('rbind',lapply(1:ncol(deaf[[1]]), function(cc)
                c(unadj=diffP(c(deaf$ests['Trend',cc],hear$ests['Trend',cc]),
                      c(deaf$ses['Trend',cc],hear$ses['Trend',cc])),
                  adj=diffP(c(deaf$ests['Trend (Adj)',cc],hear$ests['Trend (Adj)',cc]),
                            c(deaf$ses['Trend (Adj)',cc],hear$ses['Trend (Adj)',cc]))))))
            analysis[[nn]]$deafSS <- getSampleSizes(nn,overTime,TRUE)[colnames(analysis[[nn]]$deaf$ests)]
            analysis[[nn]]$hearSS <- getSampleSizes(nn,overTime,FALSE)[colnames(analysis[[nn]]$deaf$ests)]
        }
    }
}

alphas <- c(0.1,0.05,0.01,0.001)
rejAdj <- lapply(alphas,function(alpha) mult4(pTrend,pDiff,alpha,TRUE))
names(rejAdj) <- alphas
#rejUnadj <- lapply(alphas,function(alpha) mult(pTrend,pDiff,alpha,FALSE))

for(nn in names(analysis)){
    analysis[[nn]]$trendStars <-
        if(names(analysis[[nn]])[1]=='deaf')
            rbind(unadj=rep('',ncol(analysis[[nn]]$deaf[[1]])),
                  adj=vapply(1:ncol(analysis[[nn]]$deaf[[1]]),function(cc) stars(rejAdj,'trend',nn,cc),'a'))
        else c(unadj='',
               adj=stars(rejAdj,'trend',nn,1))
    analysis[[nn]]$diffStars <-
        if(names(analysis[[nn]])[1]=='deaf')
            rbind(unadj=rep('',ncol(analysis[[nn]]$deaf[[1]])),
                  adj=vapply(1:ncol(analysis[[nn]]$deaf[[1]]),function(cc) stars(rejAdj,'diff',nn,cc),'a'))
        else c(unadj='',adj=stars(rejAdj,'diff',nn,1))
}

