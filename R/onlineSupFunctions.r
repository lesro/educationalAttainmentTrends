

combineDat <- function(nnn,ot,deaf=NULL){
    tdat <- do.call('rbind', lapply(ot, function(x) x[[nnn]]))
    names(tdat) <- gsub('bach|hs','y',names(tdat))
    names(tdat)[grep('edLev',names(tdat))] <- 'y'

    for(i in 1:ncol(tdat)) if(is.factor(tdat[,i])) tdat[,i] <- as.character(tdat[,i])

    if('SEX'%in%names(tdat)) tdat$SEX <- c('Male','Female')[tdat$SEX]

    if(!is.null(deaf)){
        tdat <- tdat[tdat$DEAR==ifelse(deaf,1,2),]
        tdat$DEAR <- NULL
    }

    tdat$se[tdat$se==0] <- NA

    subCols <- setdiff(names(tdat),c('y','AGEP','se','Freq','se1','se2','year'))
    subsets <- if(length(subCols)>1) do.call(expand.grid,lapply(tdat[,subCols],unique))
               else cbind(unique(tdat[,subCols]))
    list(tdat=tdat,subCols=subCols,subsets=subsets)
}


extrSubs <- function(ccc,varb){
    ddd <- ccc$tdat[,c('year',varb,ccc$subCols)]
    names(ddd) <- c('year','varb',paste0('sub',seq(length(ccc$subCols))))
    tab <- spread(ddd, key=sub1,value=varb)%>%as.data.frame()
    rownames(tab) <- 2007+tab$year
    tab$year <- NULL
    tab
}

tabFunBegin <- function(nnn,ot,deaf=NULL){
    ccc <- combineDat(nnn,ot,deaf)

    ## trnd <- trends(nnn,ot)
    ## if(!is.null(deaf)) trnd <- trnd[grep(paste0('DEAR=',ifelse(deaf,1,2)),names(trnd))]

    if(nnn%in%names(overTimeAge[[1]])){
        trndsAdj <- trends(nnn,overTimeAge)
        if(!is.null(deaf)) trndsAdj <-
            trndsAdj[grep(paste0('DEAR=',ifelse(deaf,1,2)),names(trndsAdj))]
    }

    ests <- extrSubs(ccc,'y')*1
    ses <- extrSubs(ccc,'se')*1

    ## ests <- rbind(ests,
    ##               Growth=ests[nrow(ests),]-ests[1,])

    ## ses <- rbind(ses,Growth=sqrt(ses[nrow(ses),]^2+ses[1,]^2))

    out <- list(ests=ests,ses=ses)#,trnd=trnd[sapply(names(ests),function(nn) grep(nn,names(trnd)))])
    if(nnn%in%names(overTimeAge[[1]]))
        out$trndsAdj <- trndsAdj[sapply(names(ests),function(nn) grep(nn,names(trndsAdj)))]
    out
}


tabFun <- function(nnn,ot,deaf=NULL,moe=FALSE){

    tfb <- tabFunBegin(nnn,ot,deaf)
    if(grepl('RaceSex',nnn)) tfb <- estsRS(tfb)
    ests <- tfb$ests
    ses <- tfb$ses
    trndsAdj <- tfb$trndsAdj

    ests <- rbind(ests,
                  Trend= vapply(1:ncol(ests),function(j)
                          trndsAdj[[grep(colnames(ests)[j],names(trndsAdj))]][1]*1,1))
        ses <- rbind(ses,
                     Trend=vapply(1:ncol(ests),function(j)
                         trndsAdj[[grep(colnames(ests)[j],names(trndsAdj))]][2]*1,1))
    tabFinish(ests=ests,ses=ses,deaf=deaf,moe=moe)
}

tabFinish <- function(ests,ses,deaf=TRUE,moe=TRUE){ #ests,ses,moe,strs){
    tab <- ests
    for(j in 1:ncol(ests)){
        for(i in 1:nrow(ests)){
            if(moe) tab[i,j] <- paste(round(ests[i,j],2),'$\\pm$',round(ses[i,j]*1.96,2))
            else tab[i,j] <- paste0(round(ests[i,j],2),' (',round(ses[i,j],2),')')
        }
    }
    if(is.null(deaf)){
        colnames(tab) <- c('Deaf','Hearing')
    }

    tab <- tab%>%
      tibble::rownames_to_column('Year')

    tab
}

trends <- function(nnn,ot,weightDat=NULL,intercept=FALSE,returnMods=FALSE){
    ccc <- combineDat(nnn,ot)

    rows <- if(intercept) c('(Intercept)','year') else 'year'
    trends <- list()
    with(ccc,{
      form <- if('AGEP'%in%names(tdat)) y~year+as.factor(AGEP) else y~year
      if(!is.null(attr(ot,'weightDat'))){
        tdat <- full_join(tdat,attr(ot,'weightDat'),by='AGEP')
      } else
        tdat$ww <- if(any(is.na(tdat$se))) tdat$Freq else 1/tdat$se^2

#      if(any(is.na(tdat$y)&!is.na(tdat$se))|any(is.na(tdat$se)&!is.na(tdat$y))) stop('weird na')
#      tdat$y[is.na(tdat$y)] <- 0

      mods <- list(deaf=list(),hearing=list())
      for(i in 1:nrow(subsets)){
        keep <- rep(TRUE,nrow(tdat))
        for(j in 1:ncol(subsets)){
          if(is.factor(subsets[,j])) subsets[,j] <- as.character(subsets[,j])
          keep <- keep&(tdat[,subCols[j]]==subsets[i,j])
        }
        trnd <- lm_robust(form,data=tdat[keep,],weights=ww)
        if(is.nan(summary(trnd)$coef[rows,'Std. Error']))
          trnd <- lm_robust(form,data=tdat[keep,],weights=ww,se_type='HC1')

        if(subsets$DEAR[i]==1){
          if(endsWith(nnn,'Tot')){
            mods$deaf <- trnd
          } else
            mods$deaf[[subsets[i,2]]] <- trnd
        } else{
          if(endsWith(nnn,'Tot')){
            mods$hearing <- trnd
          } else
            mods$hearing[[subsets[i,2]]] <- trnd
        }
        trends[[paste(subCols,subsets[i,],collapse=' ',sep='=')]] <- summary(trnd)$coef[rows,]
      }
      if(returnMods) return(mods)
      trends
    })
}


trendsFull <- function(ot){
   sapply(names(ot[[1]]),trends,ot=ot)
}


#### functions for plotting gaps over time
gapDat <- function(nnn,ot){
  ccc <- combineDat(nnn,ot)

  gdat <- ccc$tdat%>%
    select(-Freq)%>%
    group_by_at(vars(-DEAR,-y,-se))%>%
    summarize(gap=y[DEAR==2]-y[DEAR==1],se=sqrt(se[DEAR==1]^2+se[DEAR==2]^2))%>%
    mutate(year=year+2007)

  list(gdat=gdat,subCols=ccc$subCols,subsets=ccc$subsets)
}

gapAdjOne <- function(tab,...){
  names(tab)[grep('edLev',names(tab))] <- 'y'
  mod <- lm_robust(y~DEAR+as.factor(AGEP),data=tab,subset=!is.na(y)&!is.na(se))
  out <- as.data.frame(summary(mod)$coef)['DEAR',1:2]
  if(any(is.nan(out[['Std. Error']]))){
    mod <- update(mod,se_type='HC1')
    out <- as.data.frame(summary(mod)$coef)['DEAR',1:2]
  }
  out
}

gapAdj <- function(dat){
  names(dat)[grep('edLev',names(dat))] <- 'y'
  subCols <- setdiff(names(dat),c('y','AGEP','se','Freq','se1','se2','year','DEAR'))

  if('SEX'%in%names(dat)) dat$SEX <- c('Male','Female')[dat$SEX]


  if(!length(subCols))
    return(gapAdjOne(dat))

  dat%>%
    group_by_at(vars(!! sym(subCols)))%>%
    group_modify(gapAdjOne)%>%
    mutate(year=dat$year[1])
}

gapTrendOne <- function(tdat,returnMod=FALSE,...){
  mod <-
    if(any(is.na(tdat$se))){
      lm_robust(y~(DEAR+splines::ns(AGEP,5))*year,tdat,weights=ww)
    } else
      lm_robust(y~(DEAR+as.factor(AGEP))*year,weights=ww,data=tdat)
  if(returnMod) return(mod)
  as.data.frame(summary(mod)$coef)['DEAR:year',c(1,2,4)]
}

gapTrend <- function(nnn,overTimeAge,weightDat,...){
  ccc <- combineDat(nnn,overTimeAge)
  tdat <- ccc$tdat
  if(!is.null(weightDat)){
    tdat <- full_join(tdat,weightDat)
  } else
    tdat$ww <- if(any(is.na(tdat$se))) tdat$Freq else 1/tdat$se^2

  if(length(ccc$subCols)==1) return(gapTrendOne(tdat))

  tdat%>%
    group_by_at(vars(!! sym(ccc$subCols[2])))%>%
    group_modify(gapTrendOne)
}

gapTrendMod <- function(nnn,overTimeAge,weightDat){
  ccc <- combineDat(nnn,overTimeAge)
  tdat <- ccc$tdat
  if(!is.null(weightDat)){
    tdat <- full_join(tdat,weightDat,by='AGEP')
  } else
    tdat$ww <- if(any(is.na(tdat$se))) tdat$Freq else 1/tdat$se^2

  if(length(ccc$subCols)==1) return(gapTrendOne(tdat,returnMod=TRUE))

  tdat%>%
    group_by_at(vars(!! sym(ccc$subCols[2])))%>%
      group_map(gapTrendOne,returnMod=TRUE,keep=TRUE)
}

gapAdjYr <- function(nnn,overTimeAge,weightDat=NULL){
  gaps <- lapply(overTimeAge, function(ota) gapAdj(ota[[nnn]]))

  ests <- do.call('rbind',lapply(gaps,function(x) x$Estimate))
  ses <- do.call('rbind',lapply(gaps,function(x) x$`Std. Error`))
  rownames(ests) <- rownames(ses) <- 2007+seq(length(gaps))
  if(ncol(ests)>1) colnames(ests) <- colnames(ses) <- gaps[[1]][[1]]



  gtrends <- gapTrend(nnn,overTimeAge,weightDat=weightDat)
  ests <- rbind(ests,trend=gtrends$Estimate)
  ses <- rbind(ses,trend=gtrends$`Std. Error`)
  trendPs <- gtrends$`Pr(>|t|)`
  if(ncol(ests)>1) names(trendPs) <- unlist(gtrends[,1])

  list(ests=ests,ses=ses,pvals=trendPs)
}

gapDatAdj <- function(nnn,overTimeAge){
  gay <- gapAdjYr(nnn,overTimeAge)
  ccc <- combineDat(nnn,overTimeAge)
  if(ncol(gay$ests)==1)
    return(
      list(
        gdat=data.frame(year=seq(nrow(gay$est)-1)+2007,gap=gay$ests[-nrow(gay$ests)],se=gay$se[-nrow(gay$ests)]),
        subCols=ccc$subCols,
        subsets=ccc$subsets
      )
    )

  gdat <- distinct(ccc$tdat[,c(ccc$subCols[2],'year')])
  gdat$year <- gdat$year+2007
  gdat$gap <- gdat$se <- NA
  for(i in 1:nrow(gdat)){
    gdat$gap[i] <- gay$ests[as.character(gdat$year[i]),gdat[[ccc$subCols[2]]][i]]
    gdat$se[i] <- gay$ses[as.character(gdat$year[i]),gdat[[ccc$subCols[2]]][i]]
  }

  list(
    gdat=gdat,
    subCols=ccc$subCols,
    subsets=ccc$subsets
  )
}

makeGapTab <- function(gap1,moe=FALSE){
  ests <- gap1$ests
  ses <- gap1$ses
#  strs <- cbind(gap1$stars,c('',''))

  tab <- ests
  for(j in 1:ncol(ests)){
    for(i in 1:nrow(ests)){
      if(moe) tab[i,j] <- paste(round(ests[i,j],2),'$\\pm$',round(ses[i,j]*1.96,2))
      else tab[i,j] <- paste0(round(ests[i,j],2),' (',round(ses[i,j],2),')')
    }
  }
  tab
}

### regression tables
makeObj <- function(mod){
  ext <- texreg:::extract(mod)
  texreg:::createTexreg(coef.names=gsub('as.factor(AGEP)','Age=',ext@coef.names,fixed=TRUE),ext@coef,ext@se,ext@pvalues,gof.names=ext@gof.names,gof=ext@gof,gof.decimal=ext@gof.decimal,model.name=mod$modName)
}

### attainment
attRegTab <- function(nnn,overTimeAge,weightDat){
  mods <- trends(nnn=nnn,ot=overTimeAge,weightDat=weightDat,returnMods=TRUE)
  mods <- mods%>%map(function(m) map(names(m)%>%setNames(.,.),function(x) {m[[x]]$modName <- x;m[[x]]}))

  map(mods,function(x) map(x,makeObj))
}

gapRegTab <- function(nnn,overTimeAge,weightDat){
  mods <- gapTrendMod(nnn=nnn,overTimeAge,weightDat=weightDat)

  mods <-  map(names(mods)%>%setNames(.,.),function(x) {mods[[x]]$modName <- x;mods[[x]]})

  map(mods,function(x) map(x,makeObj))
}
