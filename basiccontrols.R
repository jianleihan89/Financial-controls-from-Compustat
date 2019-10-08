library(RPostgres)
library(readr)
library(dplyr)
library(psych)
cutoff=0.01         ###setup winsorize percentage
###input your username and password of wrds
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user="username",
                  password="password")

####get varaible names form compustat fundamentals data
#res <- dbSendQuery(wrds, "select column_name
#                   from information_schema.columns
#                   where table_schema='comp'
#                   and table_name='funda'
#                   order by column_name")
#data=dbFetch(res, n=-1)
#dbClearResult(res)
#data
res <- dbSendQuery(wrds, "select a.gvkey,a.cusip,a.cik,a.fyear,a.datadate,
                   a.at,a.xrd,a.sale,a.ib,a.ppent,a.dltt,a.dlc,a.capx,a.ceq,
                   a.csho,a.prcc_f,a.dp,a.dvc,a.che,a.act,a.lct,b.sic,
                   a.txp,a.dp,a.ibc,a.oancf,a.xidoc,a.re
                   from comp.funda a join comp.company b
                   on a.gvkey=b.gvkey
                   where a.consol='C'
                   and a.datafmt='STD'
                   and a.popsrc='D'
                   and a.at>0")
compfunda=dbFetch(res,n=-1)
dbClearResult(res)


compfunda$logat=winsor(log(compfunda$at),trim=cutoff)
compfunda$logsale=winsor(log(compfunda$sale),trim=cutoff)
compfunda$rd=winsor(compfunda$xrd/compfunda$sale,trim=cutoff)
compfunda$roa=winsor(compfunda$ib/compfunda$at,trim=cutoff)
compfunda$ppe=winsor(compfunda$ppent/compfunda$at,trim=cutoff)
compfunda$lev=winsor((compfunda$dltt+compfunda$dlc)/compfunda$at,trim=cutoff)
compfunda$cap=winsor(compfunda$capx/compfunda$ppent,trim=cutoff)
compfunda$q=winsor((compfunda$at-compfunda$ceq+(compfunda$csho*compfunda$prcc_f))/compfunda$at,trim=cutoff)
compfunda$cashflow=winsor((compfunda$ib+compfunda$dp)/compfunda$ppent,trim=cutoff)
compfunda$dividend=winsor(compfunda$dvc/compfunda$at,trim=cutoff)
compfunda$cash=winsor(compfunda$che/compfunda$ppent,trim=cutoff)
compfunda$indu=trunc(as.numeric(compfunda$sic)/100)
