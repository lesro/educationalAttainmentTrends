


load('output/estByYear.RData')

overTime <- processEsts(overTime)

for(lev in c('hs','cc','bach')){
    figFun(lev,overTime)
    ggsave(paste0('figures/',lev,'.png'),width=6.4,height=6)
}
