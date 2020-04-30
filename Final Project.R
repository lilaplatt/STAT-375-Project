### Impact of Food Availability on Student Behavior Using Data from the ECLS 2011

c(
  "foreign",
  "dplyr",
  "tidyverse",
  "rpart",
  "caret",
  "tree",
  "igraphdata",
  "ggraph",
  "igraph",
  "tidyverse",
  "grDevices",
  "stats",
  "graphics",
  "Hmisc"
) -> package_names

install.packages(c(
    "foreign",
    "dplyr",
    "tidyverse",
    "rpart",
    "caret",
    "tree",
    "igraphdata",
    "ggraph",
    "igraph",
    "tidyverse",
    "grDevices",
    "stats",
    "graphics",
    "Hmisc"
  )
)

library(foreign)
library(dplyr)
library(tidyverse)
library(rpart)
library(caret)
library(tree)
library(igraphdata)
library(ggraph)
library(igraph)
library(tidyverse)
library(grDevices)
library(stats)
library(graphics)
library(Hmisc)

for (package_name in package_names) {
  if (!is.element(package_name, installed.packages()[, 1])) {
    install.packages(package_name,
                     repos = "http://cran.mtu.edu/")
  }
  library(
    package_name,
    character.only = TRUE,
    quietly = TRUE,
    verbose = FALSE
  )
}
rm(list = c("package_name", "package_names"))


# Reading in the data
data <-
  read.spss(file = "/Users/lilaplatt/Desktop/Junior Year/STAT 375/ECLSK2011_K5PUF.sav", to.data.frame = TRUE)


# Extracting variables of interest (not all are used in analyses)
a <- select(
  data,
  X9PAR1EMP_I,
  X9PAR2EMP_I,
  X9PAR1OCC_I,
  X9PAR2OCC_I,
  X9PAR1SCR_I,
  X9PAR2SCR_I,
  X9INCCAT_I,
  P9TINCTH_I,
  X9HTOTAL,
  X9NUMSIB,
  X9LESS18,
  X9OVER18,
  X9POVTY_I,
  X9FRMEAL_I,
  S9PCTFLN_I,
  X9FSRAW2,
  X9FSADRA2,
  X9FSCHRA,
  X9FSSCAL2,
  X9FSADSC2,
  X9FSCHSC,
  X9FSSTAT2,
  X9FSADST2,
  X9FSCHST,
  X9SESL_I,
  X9SCTYP,
  X9PUBPRI,
  X9REGION,
  X9LOCALE
)

b <- select(
  data,
  X_AMINAN_R,
  X_ASIAN_R,
  X_HAWPI_R,
  X_BLACK_R,
  X_WHITE_R,
  X_HISP_R,
  X_MULTR_R,
  X_RACETHP_R,
  X_RACETH_R,
  X9GRDLVL,
  X7RSCALK5,
  X8RSCALK5,
  X9RSCALK5,
  X7MSCALK5,
  X8MSCALK5,
  X9MSCALK5,
  X7SSCALK5,
  X8SSCALK5,
  X9SSCALK5,
  C7LKREAD,
  C7INTREAD,
  C7CTWREAD,
  C7GDREAD,
  C7ENJREAD,
  C7LIKMTH,
  C7INTMTH,
  C7CTWMTH,
  C7GDMTH,
  C7ENJMTH,
  C7LKSCI,
  C7INTSCI,
  C7CTWSCI,
  C7GDSCI,
  C7ENJSCI,
  C7HASFRNDS,
  C7MKFRNDS,
  C7GETALNG,
  C7EASYLIK,
  C7WTMEFRND,
  C7MORFRND
)

c <- select(
  data,
  C9FITIN,
  C9CLOSCL,
  C9CLOSTC,
  C9ENJOY,
  C9SAFE,
  C9FINISH,
  C9TRYMST,
  C9WKGOAL,
  C9WKHDQT,
  C9WKSETDO,
  C9TRYIMPRV,
  C9WRYTST,
  C9HARDFIN,
  C9ASHAME,
  C9WRYWEL,
  C9WRYFIN,
  C9KNWFREE,
  C9KNWHW,
  C9KNWGRD,
  X1TCHCON,
  X1TCHPER,
  X1TCHEXT,
  X1TCHINT,
  X2TCHCON,
  X2TCHPER,
  X2TCHEXT,
  X2TCHINT,
  X3TCHCON,
  X3TCHPER,
  X3TCHEXT,
  X3TCHINT,
  X4TCHCON,
  X4TCHPER,
  X4TCHEXT,
  X4TCHINT
)

d <- select(
  data,
  X5TCHCON,
  X5TCHPER,
  X5TCHEXT,
  X5TCHINT,
  X6TCHCON,
  X6TCHPER,
  X6TCHEXT,
  X6TCHINT,
  X7TCHCON,
  X7TCHPER,
  X7TCHEXT,
  X7TCHINT,
  X8TCHCON,
  X8TCHPER,
  X8TCHEXT,
  X8TCHINT,
  X9TCHCON,
  X9TCHPER,
  X9TCHEXT,
  X9TCHINT,
  X1TCHAPP,
  X2TCHAPP,
  X3TCHAPP,
  X4TCHAPP,
  X5TCHAPP,
  X6TCHAPP,
  X7TCHAPP,
  X8TCHAPP,
  X9TCHAPP,
  X1ATTNFS,
  X1INBCNT,
  X2ATTNFS,
  X2INBCNT,
  X4ATTNFS,
  X4INBCNT,
  X4KATTNFS,
  X4KINBCNT
)

e <- select(
  data,
  X6ATTMCQ,
  X6INTMCQ,
  X7ATTMCQ,
  X7INTMCQ,
  X8ATTMCQ,
  X8INTMCQ,
  X9ATTMCQ,
  X9INTMCQ,
  T6OSTEAS,
  T7OSTEAS,
  G8OSTEAS,
  G9OSTEAS,
  T6OSLIES,
  T7OSLIES,
  G8OSLIES,
  G9OSLIES,
  T6OSPUSH,
  T7OSPUSH,
  G8OSPUSH,
  G9OSPUSH,
  T6OSLFTO,
  T7OSLFTO,
  G8OSLFTO,
  G9OSLFTO,
  T6TSTEAS,
  T7TSTEAS,
  G8TSTEAS,
  G9TSTEAS,
  T6TSLIES,
  T7TSLIES,
  G8TSLIES,
  G9TSLIES,
  T6TSPUSH,
  T7TSPUSH,
  G8TSPUSH,
  G9TSPUSH,
  T6TSLFTO,
  T7TSLFTO,
  G8TSLFTO,
  G9TSLFTO,
  T7PLYMTE,
  G8PLYMTE,
  G9PLYMTE,
  T7PAVOID,
  G8PAVOID,
  G9PAVOID,
  T7EXLUED,
  G8EXLUED,
  G9EXLUED,
  T7IGNRED
)

f <- select(
  data,
  G8IGNRED,
  G9IGNRED,
  G8UNDFEL,
  G9UNDFEL,
  G8INTPER,
  G9INTPER,
  G8SOLINT,
  G9SOLINT,
  G8EFFBEV,
  G9EFFBEV,
  G8LIKSCH,
  G8DISLSH,
  G8FUNSCH,
  G8LBESCH,
  G8UNHAPY,
  G8ENJACT,
  G8GRNACT,
  G9LIKSCH,
  G9DISLSH,
  G9FUNSCH,
  G9LBESCH,
  G9UNHAPY,
  G9ENJACT,
  G9GRNACT,
  P8MKREAS,
  P8CDREAD,
  P8CUPSET,
  P8STAYHM,
  P8CMPLNS,
  P9MKREAS,
  P9CDREAD,
  P9CUPSET,
  P9STAYHM,
  P9CMPLNS,
  C7TEASED,
  C7LIESABT,
  C7PUSHCH,
  C7EXCLDCH,
  C7WRYTHK,
  C7WRYDTLK,
  C7AFRDNTLK,
  C7CHEERUP,
  C7HLPOTH,
  C7NICEOTH
)

g <- select(
  data,
  C8TEASED,
  C8LIESABT,
  C8PUSHCH,
  C8EXCLDCH,
  C8WRYTHK,
  C8WRYDTLK,
  C8AFRDNTLK,
  C8TRYHRD,
  C8WRKHRD,
  C8PARDIS,
  C8PAYATT,
  C8LSTNCL,
  C8KIDBTR,
  C8KIDPLY,
  C8KIDHAP,
  C8KIDHLP,
  C8FRIEND,
  C8HELPMN,
  C8LONELY,
  C8LFTOUT,
  C8ALONE,
  C9TEASED,
  C9LIESABT,
  C9PUSHCH,
  C9EXCLDCH,
  C9WRYTHK,
  C9WRYDTLK,
  C9AFRDNTLK,
  C9TRYHRD,
  C9WRKHRD,
  C9PARDIS,
  C9PAYATT,
  C9LSTNCL,
  C9KIDBTR,
  C9KIDPLY,
  C9KIDHAP,
  C9FRIEND,
  C9HELPMN,
  C9LONELY,
  C9LFTOUT,
  C9ALONE
)

df <- data.frame(a, b, c, d, e, f, g)
str(df)

# Removal of negative values in food availability variables
table(df$X9FRMEAL_I)
df$X9FRMEAL_I[df$X9FRMEAL_I == -9] <- NA
df$X9FRMEAL_I[df$X9FRMEAL_I == -1] <- NA
table(df$X9FRMEAL_I)

table(df$X9FSSCAL2)
df$X9FSSCAL2[df$X9FSSCAL2 == -9] <- NA
df$X9FSSCAL2[df$X9FSSCAL2 == -6] <- NA
table(df$X9FSSCAL2)

table(df$X9FSADSC2)
df$X9FSADSC2[df$X9FSADSC2 == -9] <- NA
df$X9FSADSC2[df$X9FSADSC2 == -6] <- NA
table(df$X9FSADSC2)

table(df$X9FSSTAT2)
df$X9FSSTAT2[df$X9FSSTAT2 == -9] <- NA
table(df$X9FSSTAT2)

table(df$X9FSCHST)
df$X9FSCHST[df$X9FSCHST == -9] <- NA
table(df$X9FSCHST)



# Removal of negatives in subject level variables in child questionnaire for reading, math, and science
# Tables are included before and after to ensure values were correctly removed

table(df$C7LKREAD)
df$C7LKREAD[df$C7LKREAD == -9] <- NA
df$C7LKREAD[df$C7LKREAD == -7] <- NA
table(df$C7LKREAD)

table(df$C7INTREAD)
df$C7INTREAD[df$C7INTREAD == -9] <- NA
df$C7INTREAD[df$C7INTREAD == -7] <- NA
table(df$C7INTREAD)

table(df$C7CTWREAD)
df$C7CTWREAD[df$C7CTWREAD == -9] <- NA
df$C7CTWREAD[df$C7CTWREAD == -7] <- NA
table(df$C7CTWREAD)

table(df$C7GDREAD)
df$C7GDREAD[df$C7GDREAD == -9] <- NA
df$C7GDREAD[df$C7GDREAD == -7] <- NA
table(df$C7GDREAD)

table(df$C7ENJREAD)
df$C7ENJREAD[df$C7ENJREAD == -9] <- NA
df$C7ENJREAD[df$C7ENJREAD == -7] <- NA
table(df$C7ENJREAD)

table(df$C7LIKMTH)
df$C7LIKMTH[df$C7LIKMTH == -9] <- NA
df$C7LIKMTH[df$C7LIKMTH == -7] <- NA
table(df$C7LIKMTH)

table(df$C7INTMTH)
df$C7INTMTH[df$C7INTMTH == -9] <- NA
df$C7INTMTH[df$C7INTMTH == -7] <- NA
table(df$C7INTMTH)

table(df$C7CTWMTH)
df$C7CTWMTH[df$C7CTWMTH == -9] <- NA
df$C7CTWMTH[df$C7CTWMTH == -7] <- NA
table(df$C7CTWMTH)

table(df$C7GDMTH)
df$C7GDMTH[df$C7GDMTH == -9] <- NA
df$C7GDMTH[df$C7GDMTH == -7] <- NA
table(df$C7GDMTH)

table(df$C7ENJMTH)
df$C7ENJMTH[df$C7ENJMTH == -9] <- NA
df$C7ENJMTH[df$C7ENJMTH == -7] <- NA
table(df$C7ENJMTH)

table(df$C7LKSCI)
df$C7LKSCI[df$C7LKSCI == -9] <- NA
df$C7LKSCI[df$C7LKSCI == -7] <- NA
table(df$C7LKSCI)

table(df$C7INTSCI)
df$C7INTSCI[df$C7INTSCI == -9] <- NA
df$C7INTSCI[df$C7INTSCI == -7] <- NA
table(df$C7INTSCI)

table(df$C7CTWSCI)
df$C7CTWSCI[df$C7CTWSCI == -9] <- NA
df$C7CTWSCI[df$C7CTWSCI == -7] <- NA
table(df$C7CTWSCI)

table(df$C7GDSCI)
df$C7GDSCI[df$C7GDSCI == -9] <- NA
df$C7GDSCI[df$C7GDSCI == -7] <- NA
table(df$C7GDSCI)

table(df$C7ENJSCI)
df$C7ENJSCI[df$C7ENJSCI == -9] <- NA
df$C7ENJSCI[df$C7ENJSCI == -7] <- NA
table(df$C7ENJSCI)


# Removing Negative Values from child questionnaire- percieved interest/competence in peer relationships

table(df$C7HASFRNDS)
df$C7HASFRNDS[df$C7HASFRNDS < 0] <- NA
table(df$C7HASFRNDS)

table(df$C7MKFRNDS)
df$C7MKFRNDS[df$C7MKFRNDS < 0] <- NA
table(df$C7MKFRNDS)

table(df$C7GETALNG)
df$C7GETALNG[df$C7GETALNG < 0] <- NA
table(df$C7GETALNG)

table(df$C7EASYLIK)
df$C7EASYLIK[df$C7EASYLIK < 0] <- NA
table(df$C7EASYLIK)

table(df$C7WTMEFRND)
df$C7WTMEFRND[df$C7WTMEFRND < 0] <- NA
table(df$C7WTMEFRND)

table(df$C7MORFRND)
df$C7MORFRND[df$C7MORFRND < 0] <- NA
table(df$C7MORFRND)



# Removing Negative Values from child questionnaire- school belonging

table(df$C9FITIN)
df$C9FITIN[df$C9FITIN < 0] <- NA
table(df$C9FITIN)

table(df$C9CLOSCL)
df$C9CLOSCL[df$C9CLOSCL < 0] <- NA
table(df$C9CLOSCL)

table(df$C9CLOSTC)
df$C9CLOSTC[df$C9CLOSTC < 0] <- NA
table(df$C9CLOSTC)

table(df$C9ENJOY)
df$C9ENJOY[df$C9ENJOY < 0] <- NA
table(df$C9ENJOY)

table(df$C9SAFE)
df$C9SAFE[df$C9SAFE < 0] <- NA
table(df$C9SAFE)


# Removing Negative Values from child questionnaire- worry/stress

table(df$C9FINISH)
df$C9FINISH[df$C9FINISH < 0] <- NA
table(df$C9FINISH)

table(df$C9TRYMST)
df$C9TRYMST[df$C9TRYMST < 0] <- NA
table(df$C9TRYMST)

table(df$C9WKGOAL)
df$C9WKGOAL[df$C9WKGOAL < 0] <- NA
table(df$C9WKGOAL)

table(df$C9WKHDQT)
df$C9WKHDQT[df$C9WKHDQT < 0] <- NA
table(df$C9WKHDQT)

table(df$C9WKSETDO)
df$C9WKSETDO[df$C9WKSETDO < 0] <- NA
table(df$C9WKSETDO)

table(df$C9TRYIMPRV)
df$C9TRYIMPRV[df$C9TRYIMPRV < 0] <- NA
table(df$C9TRYIMPRV)

table(df$C9WRYTST)
df$C9WRYTST[df$C9WRYTST < 0] <- NA
table(df$C9WRYTST)

table(df$C9HARDFIN)
df$C9HARDFIN[df$C9HARDFIN < 0] <- NA
table(df$C9HARDFIN)

table(df$C9ASHAME)
df$C9ASHAME[df$C9ASHAME < 0] <- NA
table(df$C9ASHAME)

table(df$C9WRYWEL)
df$C9WRYWEL[df$C9WRYWEL < 0] <- NA
table(df$C9WRYWEL)

table(df$C9WRYFIN)
df$C9WRYFIN[df$C9WRYFIN < 0] <- NA
table(df$C9WRYFIN)



# Removing Negative Values from child questionnaire- parental monitoring

table(df$C9KNWFREE)
df$C9KNWFREE[df$C9KNWFREE < 0] <- NA
table(df$C9KNWFREE)

table(df$C9KNWHW)
df$C9KNWHW[df$C9KNWHW < 0] <- NA
table(df$C9KNWHW)

table(df$C9KNWGRD)
df$C9KNWGRD[df$C9KNWGRD < 0] <- NA
table(df$C9KNWGRD)



# Removing Negative Values from child questionnaire- peer victimization (both grades 3 and 5)

table(df$C7TEASED)
df$C7TEASED[df$C7TEASED < 0] <- NA
table(df$C7TEASED)

table(df$C7LIESABT)
df$C7LIESABT[df$C7LIESABT < 0] <- NA
table(df$C7LIESABT)

table(df$C7PUSHCH)
df$C7PUSHCH[df$C7PUSHCH < 0] <- NA
table(df$C7PUSHCH)

table(df$C7EXCLDCH)
df$C7EXCLDCH[df$C7EXCLDCH < 0] <- NA
table(df$C7EXCLDCH)

table(df$C9TEASED)
df$C9TEASED[df$C9TEASED < 0] <- NA
table(df$C9TEASED)

table(df$C9LIESABT)
df$C9LIESABT[df$C9LIESABT < 0] <- NA
table(df$C9LIESABT)

table(df$C9PUSHCH)
df$C9PUSHCH[df$C9PUSHCH < 0] <- NA
table(df$C9PUSHCH)

table(df$C9EXCLDCH)
df$C9EXCLDCH[df$C9EXCLDCH < 0] <- NA
table(df$C9EXCLDCH)



# Removing Negative Values from child questionnaire- social anxiety/ fear of negative evaluation (both grades 3 and 5)

table(df$C7WRYTHK)
df$C7WRYTHK[df$C7WRYTHK < 0] <- NA
table(df$C7WRYTHK)

table(df$C7WRYDTLK)
df$C7WRYDTLK[df$C7WRYDTLK < 0] <- NA
table(df$C7WRYDTLK)

table(df$C7AFRDNTLK)
df$C7AFRDNTLK[df$C7AFRDNTLK < 0] <- NA
table(df$C7AFRDNTLK)

table(df$C9WRYTHK)
df$C9WRYTHK[df$C9WRYTHK < 0] <- NA
table(df$C9WRYTHK)

table(df$C9WRYDTLK)
df$C9WRYDTLK[df$C9WRYDTLK < 0] <- NA
table(df$C9WRYDTLK)

table(df$C9AFRDNTLK)
df$C9AFRDNTLK[df$C9AFRDNTLK < 0] <- NA
table(df$C9AFRDNTLK)


# Removing Negative Values from child questionnaire- prosocial behavior

table(df$C7CHEERUP)
df$C7CHEERUP[df$C7CHEERUP < 0] <- NA
table(df$C7CHEERUP)

table(df$C7HLPOTH)
df$C7HLPOTH[df$C7HLPOTH < 0] <- NA
table(df$C7HLPOTH)

table(df$C7NICEOTH)
df$C7NICEOTH[df$C7NICEOTH < 0] <- NA
table(df$C7NICEOTH)



# Removing Negative Values from child questionnaire- loneliness

table(df$C9LONELY)
df$C9LONELY[df$C9LONELY < 0] <- NA
table(df$C9LONELY)

table(df$C9LFTOUT)
df$C9LFTOUT[df$C9LFTOUT < 0] <- NA
table(df$C9LFTOUT)

table(df$C9ALONE)
df$C9ALONE[df$C9ALONE < 0] <- NA
table(df$C9ALONE)



# Removing Negative Values from teacher report- school liking

table(df$G9LIKSCH)
df$G9LIKSCH[df$G9LIKSCH < 0] <- NA
table(df$G9LIKSCH)

table(df$G9DISLSH)
df$G9DISLSH[df$G9DISLSH < 0] <- NA
table(df$G9DISLSH)

table(df$G9FUNSCH)
df$G9FUNSCH[df$G9FUNSCH < 0] <- NA
table(df$G9FUNSCH)

table(df$G9LBESCH)
df$G9LBESCH[df$G9LBESCH < 0] <- NA
table(df$G9LBESCH)

table(df$G9UNHAPY)
df$G9UNHAPY[df$G9UNHAPY < 0] <- NA
table(df$G9UNHAPY)

table(df$G9ENJACT)
df$G9ENJACT[df$G9ENJACT < 0] <- NA
table(df$G9ENJACT)

table(df$G9GRNACT)
df$G9GRNACT[df$G9GRNACT < 0] <- NA
table(df$G9GRNACT)



# Removing Negative Values from parent report- school avoidance

table(df$P9MKREAS)
df$P9MKREAS[df$P9MKREAS < 0] <- NA
table(df$P9MKREAS)

table(df$P9CDREAD)
df$P9CDREAD[df$P9CDREAD < 0] <- NA
table(df$P9CDREAD)

table(df$P9CUPSET)
df$P9CUPSET[df$P9CUPSET < 0] <- NA
table(df$P9CUPSET)

table(df$P9STAYHM)
df$P9STAYHM[df$P9STAYHM < 0] <- NA
table(df$P9STAYHM)

table(df$P9CMPLNS)
df$P9CMPLNS[df$P9CMPLNS < 0] <- NA
table(df$P9CMPLNS)


# Removing Negative Values from teacher reported inhibitory control and focus for grades 3 and 5

table(df$X7ATTMCQ)
df$X7ATTMCQ[df$X7ATTMCQ < 0] <- NA
table(df$X7ATTMCQ)

table(df$X7INTMCQ)
df$X7INTMCQ[df$X7INTMCQ < 0] <- NA
table(df$X7INTMCQ)

table(df$X9ATTMCQ)
df$X9ATTMCQ[df$X9ATTMCQ < 0] <- NA
table(df$X9ATTMCQ)

table(df$X9INTMCQ)
df$X9INTMCQ[df$X9INTMCQ < 0] <- NA
table(df$X9INTMCQ)



# Reading IRT scale scores for grades 3 and 5

table(df$X7RSCALK5)
df$X7RSCALK5[df$X7RSCALK5 < 0] <- NA
table(df$X7RSCALK5)

table(df$X8RSCALK5)
df$X8RSCALK5[df$X8RSCALK5 < 0] <- NA
table(df$X8RSCALK5)

table(df$X9RSCALK5)
df$X9RSCALK5[df$X9RSCALK5 < 0] <- NA
table(df$X9RSCALK5)



# Math IRT scale scores for grades 3 and 5

table(df$X7MSCALK5)
df$X7MSCALK5[df$X7MSCALK5 < 0] <- NA
table(df$X7MSCALK5)

table(df$X8MSCALK5)
df$X8MSCALK5[df$X8MSCALK5 < 0] <- NA
table(df$X8MSCALK5)

table(df$X9MSCALK5)
df$X9MSCALK5[df$X9MSCALK5 < 0] <- NA
table(df$X9MSCALK5)



# Science IRT scale scores for grades 3 and 5

table(df$X7SSCALK5)
df$X7SSCALK5[df$X7SSCALK5 < 0] <- NA
table(df$X7SSCALK5)

table(df$X8SSCALK5)
df$X8SSCALK5[df$X8SSCALK5 < 0] <- NA
table(df$X8SSCALK5)

table(df$X9SSCALK5)
df$X9SSCALK5[df$X9SSCALK5 < 0] <- NA
table(df$X9SSCALK5)


# Correlation Matrix of five potential predictors

food <-
  subset(df,
         select = c(X9FRMEAL_I, X9FSSCAL2 , X9FSADSC2 , X9FSSTAT2 , X9FSCHST))
cor(food, use = 'complete.obs')

# X9FSSTAT2 is not used in analysis, as it is highly correlated with other variables and takes only three levels



### Models

## Food Availability and Child Questionnaire Responses Regarding Reading, Math, and Science
# Significant trees: C7LIKMTH , C7INTMTH , C7CTWMTH , C7ENJMTH

tree::tree(C7LKREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7INTREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7CTWREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7GDREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7ENJREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_1 <-
  tree::tree(C7LIKMTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_1)
print(treem_1)
plot(treem_1)

treem_2 <-
  tree::tree(C7INTMTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_2)
print(treem_2)
plot(treem_2)

treem_3 <-
  tree::tree(C7CTWMTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_3)
print(treem_3)
plot(treem_3)

tree::tree(C7GDMTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_4 <-
  tree::tree(C7ENJMTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_4)
print(treem_4)
splot(treem_4)

tree::tree(C7LKSCI ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7INTSCI ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7CTWSCI ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7GDSCI ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7ENJSCI ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)


## Food Availability and Child Questionnaire- Interest/Competence in Peer Relationships
# No significant trees

tree::tree(C7HASFRNDS ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7MKFRNDS ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7GETALNG ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7EASYLIK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7WTMEFRND ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7MORFRND ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)


## Food Availability and Child Questionnaire- School Belonging
# Significant trees: C9ENJOY

tree::tree(C9FITIN ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9CLOSCL ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9CLOSTC ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_5 <-
  tree::tree(C9ENJOY ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_5)
print(treem_5)
plot(treem_5)

tree::tree(C9SAFE ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Child Questionnaire- Worry/Stress
# Significant trees: C9WRYTST

tree::tree(C9FINISH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9TRYMST ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WKGOAL ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WKHDQT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WKSETDO ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9TRYIMPRV ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_6 <-
  tree::tree(C9WRYTST ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_6)
print(treem_6)
plot(treem_6)

tree::tree(C9HARDFIN ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9ASHAME ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WRYWEL ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WRYFIN ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)




## Food Availability and Child Questionnaire- Parental Monitoring
# No significant trees

tree::tree(C9KNWFREE ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9KNWHW ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9KNWGRD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Child Questionnaire- Peer Victimization (grades 3 and 5)
# No significant trees

tree::tree(C7TEASED ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7LIESABT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7PUSHCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7EXCLDCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9TEASED ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9LIESABT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9PUSHCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9EXCLDCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)


## Food Availability and Child Questionnaire- Social Anxiety/Fear of Negative Evaluation (grades 3 and 5)
# No significant trees

tree::tree(C7WRYTHK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7WRYDTLK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C7AFRDNTLK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WRYTHK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9WRYDTLK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9AFRDNTLK ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Child Questionnaire- Prosocial Behavior (grade 3)
# Significant trees: C7CHEERUP , C7HLPOTH , C7NICEOTH

treem_7 <-
  tree::tree(C7CHEERUP ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_7)
print(treem_7)
plot(treem_7)

treem_8 <-
  tree::tree(C7HLPOTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_8)
print(treem_8)
plot(treem_8)

treem_9 <-
  tree::tree(C7NICEOTH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_9)
print(treem_9)
plot(treem_9)



## Food Availability and Child Questionnaire- Loneliness (grade 5)
# Significant trees: C9LONELY

treem_10 <-
  tree::tree(C9LONELY ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_10)
print(treem_10)
plot(treem_10)

tree::tree(C9LFTOUT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(C9ALONE ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Teacher Report- School Liking (grade 5)
# Significant trees: G9LIKSCH , G9DISLSH, G9LBESCH , G9ENJACT

treem_11 <-
  tree::tree(G9LIKSCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_11)
print(treem_11)
plot(treem_11)

treem_12 <-
  tree::tree(G9DISLSH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_12)
print(treem_12)
plot(treem_12)

tree::tree(G9FUNSCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_13 <-
  tree::tree(G9LBESCH ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_13)
print(treem_13)
plot(treem_13)

tree::tree(G9UNHAPY ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)

treem_14 <-
  tree::tree(G9ENJACT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_14)
print(treem_14)
plot(treem_14)

tree::tree(G9GRNACT ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Parent Report- School Avoidance (grade 5)
# Significant trees: P9MKREAS , P9CDREAD , P9CUPSET , P9STAYHM

treem_15 <-
  tree::tree(P9MKREAS ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_15)
print(treem_15)
plot(treem_15)

treem_16 <-
  tree::tree(P9CDREAD ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_16)
print(treem_16)
plot(treem_16)

treem_17 <-
  tree::tree(P9CUPSET ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_17)
print(treem_17)
plot(treem_17)

treem_18 <-
  tree::tree(P9STAYHM ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_18)
print(treem_18)
plot(treem_18)

tree::tree(P9CMPLNS ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)



## Food Availability and Teacher Report- Inhibitory Control and Focus (grades 3 and 5)
# No significant trees

tree::tree(X7ATTMCQ ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(X7INTMCQ ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(X9ATTMCQ ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
tree::tree(X9INTMCQ ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)




## Food Availability and Reading, Math, and Science scores (grades 3 and 5)
# All significant trees

treem_19 <-
  tree::tree(X7RSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_19)
print(treem_19)
plot(treem_19)

treem_20 <-
  tree::tree(X9RSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_20)
print(treem_20)
plot(treem_20)

treem_21 <-
  tree::tree(X7MSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_21)
print(treem_21)
plot(treem_21)

treem_22 <-
  tree::tree(X9MSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_22)
print(treem_22)
plot(treem_22)

treem_23 <-
  tree::tree(X7SSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_23)
print(treem_23)
plot(treem_23)

treem_24 <-
  tree::tree(X9SSCALK5 ~ X9FRMEAL_I + X9FSSCAL2 + X9FSADSC2 + X9FSCHST, df)
summary(treem_24)
print(treem_24)
plot(treem_24)


### Linear Models

model1 <- lm(C7LIKMTH ~ X9FRMEAL_I, data = df)
summary(model1)

model2 <- lm(C7INTMTH ~ X9FRMEAL_I, df)
summary(model2)

model3 <- lm(C7CTWMTH ~ X9FRMEAL_I, df)
summary(model3)

model4 <- lm(C7ENJMTH ~ X9FRMEAL_I, df)
summary(model4)

model5 <- lm(C9ENJOY ~ X9FSSCAL2, df)
summary(model5)

model6 <- lm(C9WRYTST ~ X9FRMEAL_I, df)
summary(model6)

model7 <- lm(C7CHEERUP ~ X9FRMEAL_I, df)
summary(model7)

model8 <- lm(C7HLPOTH ~ X9FRMEAL_I, df)
summary(model8)

model9 <- lm(C7NICEOTH ~ X9FRMEAL_I, df)
summary(model9)

model10 <- lm(C9LONELY ~ X9FSSCAL2, df)
summary(model10)

model11 <- lm(G9LIKSCH ~ X9FSSCAL2, df)
summary(model11)

model12 <- lm(G9DISLSH ~ X9FSSCAL2, df)
summary(model12)

model13 <- lm(G9LBESCH ~ X9FSSCAL2, df)
summary(model13)

model14 <- lm(G9ENJACT ~ X9FSSCAL2, df)
summary(model14)

model15 <- lm(P9MKREAS ~ X9FSADSC2, df)
summary(model15)

model16 <- lm(P9CDREAD ~ X9FSADSC2, df)
summary(model16)

model17 <- lm(P9CUPSET ~ X9FSSCAL2, df)
summary(model17)

model18 <- lm(P9STAYHM ~ X9FSSCAL2, df)
summary(model18)

model19 <- lm(X7RSCALK5 ~ X9FRMEAL_I + X9FSCHST, df)
summary(model19)

model20 <- lm(X9RSCALK5 ~ X9FRMEAL_I + X9FSSCAL2, df)
summary(model20)

model21 <- lm(X7MSCALK5 ~ X9FRMEAL_I, df)
summary(model21)

model22 <- lm(X9MSCALK5 ~ X9FRMEAL_I, df)
summary(model22)

model23 <- lm(X7SSCALK5 ~ X9FRMEAL_I, df)
summary(model23)

model24 <- lm(X9SSCALK5 ~ X9FRMEAL_I, df)
summary(model24)
