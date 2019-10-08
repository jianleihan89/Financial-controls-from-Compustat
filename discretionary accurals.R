compfunda=compfunda %>% group_by(gvkey) %>% mutate(dca=act-lag(act))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dcl=lct-lag(lct))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dca=act-lag(act))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dcash=ceq-lag(ceq))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dstd=dlc-lag(dlc))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dtp=txp-lag(txp))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(lagta=lag(at))
compfunda=compfunda %>% group_by(gvkey) %>% mutate(dsale=sale-lag(sale))

compfunda$dsale=compfunda$dsale/compfunda$lagta
compfunda$accrual=(compfunda$dca-compfunda$dcl-compfunda$dcash+compfunda$dstd-compfunda$dp+compfunda$dtp)/compfunda$lagta
compfunda$workcap=compfunda$act-compfunda$lct
compfunda$zscore=(3.3*compfunda$ib+1.4*compfunda$re+1.2*compfunda$workcap)/compfunda$at
compfunda$tacc=(compfunda$ibc-(compfunda$oancf-compfunda$xidoc))/compfunda$at

emdata=subset(compfunda,select=c("gvkey","fyear","tacc","at","dsale","ppe","indu"))
emdata$iat=1/emdata$at
emdata$tacc=winsor(emdata$tacc,trim=cutoff)
emdata$iat=winsor(emdata$iat,trim=cutoff)
emdata$dsale=winsor(emdata$dsale,trim=cutoff)
emdata$ppe=winsor(emdata$ppe,trim=cutoff)


emdata=na.omit(emdata)
indus=emdata$indu[!duplicated(emdata$indu)]
yeari=min(emdata$fyear)
j=1
temp=subset(emdata,(fyear==yeari)&(indu==indus[j]))
reg=lm(tacc~iat+dsale+ppe,data=temp)
temp$da=residuals(reg)
result=temp
for (yeari in (min(emdata$fyear)+1):max(emdata$fyear)){
  for (j in 1:length(indus)){
    temp=subset(emdata,(fyear==yeari)&(indu==indus[j]))
    if (nrow(temp)>14) {
      reg=lm(tacc~iat+dsale+ppe,data=temp)
      temp$da=residuals(reg)
      result=rbind(result,temp)
    }
  }
}
useda=subset(result,select=c("gvkey","fyear","da")) ####result file
