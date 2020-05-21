library(magrittr)
library(estimatr)
library(tidyverse)
library(knitr)
library(rmarkdown)
select <- dplyr::select
source('R/trends.r')
source('R/onlineSupFunctions.r')

load('output/estByYear.RData')
load('output/ageDist18.RData')

overTimeAge <- processEsts(overTimeAge,ageDist18)

overTime <- processEsts(overTime)

### estimate trends

subs <- c('Tot',
          'ByAgeCat',
          'Sex','Race','RaceM','RaceF')%>%
  setNames(.,.)


### attainment rates/trends

for(lev in c('hs','cc','bach')){
  Lev <- switch(lev,hs='High School',cc='Associates',bach='Bachelors')
  sink(paste0('tables/',Lev,' Attainment.md'))
  cat("# ",Lev,"Attainment, 2008-2018\n\n\n")
  cat("## Overall\n\n")
  print(kable(tabFun(paste0(lev,'Tot'),ot=overTime,deaf=NULL,moe=FALSE)))
  for(ss in subs[-1]){
    cat("##", switch(ss,ByAgeCat="By Age",Sex="By Gender",Race="By Race/Ethnicity",RaceM="By Race/Ethnicity: Males",RaceF="By Race/Ethnicity: Females"),'\n\n')
    for(deaf in c(TRUE,FALSE)){
      cat("###", ifelse(deaf,"Deaf","Hearing"),'\n')
      print(kable(tabFun(paste0(lev,ss),ot=overTime,deaf=deaf),caption=paste(Lev,ifelse(deaf,"deaf","hearing"),"attainment")))
      cat("\n\n\n")
    }
  }
  sink()
}

### attainment gaps/trends
for(lev in c('hs','cc','bach')){
  Lev <- switch(lev,hs='High School',cc='Associates',bach='Bachelors')
  sink(paste0('tables/',Lev,' Gaps.md'))
  cat("# ",Lev,"Age-Adjusted Deaf Hearing Gaps, 2008-2018\n\n\n")
  for(ss in subs){
    cat("##", switch(ss,Tot="Overall",ByAgeCat="By Age",Sex="By Gender",Race="By Race/Ethnicity",RaceM="By Race/Ethnicity: Males",RaceF="By Race/Ethnicity: Females"),'\n\n')
    gapAdjYr('hsRace',overTimeAge,ageDist18)%>%makeGapTab()%>%kable(caption=paste(Lev,"age-adjusted deaf-hearing gaps."))%>%print()
      cat("\n\n\n")
  }
  sink()
}

### regression tables: attainment
for(lev in c('hs','cc','bach')){
  Lev <- switch(lev,hs='High School',cc='Associates',bach='Bachelors')
  sink(paste0('tables/',Lev,' AttainmentRegressions.tex'))
cat('\\documentclass[fullpage]{paper}
\\usepackage{longtable}
\\title{',Lev,'Attainment Trend Regressions}
\\begin{document}
\\section{Overall}\n'
    )
    mods <- trends(paste0(lev,'Tot'),overTimeAge,ageDist18,returnMods=TRUE)
    mods$deaf$modName <- "Deaf"
    mods$hearing$modName <- "Hearing"
    texreg(map(mods,makeObj),
      stars=numeric(0),caption=paste("Regressions to estimate overall trends in", Lev,"Attainment"),longtable=TRUE,use.packages=FALSE)%>%print()
    for(ss in subs[-1]){
      cat("\\section{", switch(ss,ByAgeCat="By Age",Sex="By Gender",Race="By Race/Ethnicity",RaceM="By Race/Ethnicity: Males",RaceF="By Race/Ethnicity: Females"),'}\n\n')
      objs <- attRegTab(paste0(lev,ss),overTimeAge=overTimeAge,weightDat=ageDist18)
      for(deaf in c(TRUE,FALSE)){
        cat("\\subsection{", ifelse(deaf,"Deaf","Hearing"),'}\n')
        print(
          texreg(
            objs[[ifelse(deaf,'deaf','hearing')]],
            caption=paste("Regressions to estimate trends in", Lev,"attainment for",ifelse(deaf,'deaf','hearing'),"people"),
            longtable=TRUE,use.packages=FALSE
            )
          )
      }
    }
cat('\\end{document}')
  sink()
}


### regression tables: gaps
for(lev in c('hs','cc','bach')){
  Lev <- switch(lev,hs='High School',cc='Associates',bach='Bachelors')
  sink(paste0('tables/',Lev,' GapRegressions.tex'))
cat('\\documentclass[fullpage]{paper}
\\usepackage{longtable}
\\title{',Lev,'Gap Trend Regressions}
\\begin{document}
\\section{Overall}\n'
    )
    mods <- gapTrendMod(paste0(lev,'Tot'),overTimeAge,ageDist18)
    mods$modName <- "Overall"
    texreg(makeObj(mods),
      stars=numeric(0),caption=paste("Regressions to estimate overall trends in", Lev,"deaf-hearing gaps."),longtable=TRUE,use.packages=FALSE)%>%print()
    for(ss in subs[-1]){
      cat("\\section{", switch(ss,ByAgeCat="By Age",Sex="By Gender",Race="By Race/Ethnicity",RaceM="By Race/Ethnicity: Males",RaceF="By Race/Ethnicity: Females"),'}\n\n')
      objs <- attRegTab(paste0(lev,ss),overTimeAge=overTimeAge,weightDat=ageDist18)
      for(deaf in c(TRUE,FALSE)){
        cat("\\subsection{", ifelse(deaf,"Deaf","Hearing"),'}\n')
        print(
          texreg(
            objs[[ifelse(deaf,'deaf','hearing')]],
            caption=paste("Regressions to estimate trends in", Lev,"attainment for",ifelse(deaf,'deaf','hearing'),"people"),
            longtable=TRUE,use.packages=FALSE
            )
          )
      }
    }
cat('\\end{document}')
  sink()
}




setwd('tables')
fff <- list.files()
for(f in fff[endsWith(fff,'.tex')]){
  system(paste0('pdflatex "',f,'"'))
  system(paste0('pdflatex "',f,'"'))
}
setwd('..')

