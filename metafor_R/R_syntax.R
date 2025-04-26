# Install and load libraries ----------------------------------------------

install.packages(c("robumeta", "metafor", "dplyr"))

library("robumeta")
library("metafor")
library("dplyr")


# Load and Manipulate Data ------------------------------------------------

dat <- get(data(dat.molloy2014)) # from `metadat` package
dat <- mutate(dat, study_id = 1:16) # This adds a study id column 
dat <- dat %>%
  select(study_id, authors:quality) # This brings the study id column to the front

# To see other meta-analytic dataset included in `metadat`
data(package="metadat")


## Some checks -------------------------------------------------------------

View(dat)
dim(dat)
glimpse(dat)

# Start (random-effects) meta-analysis -------------------------------------

dat <- escalc(
  measure="ZCOR",
  ri=ri,
  ni=ni,
  data=dat,
  slab=paste(authors, year, sep=", ")
  )
View(dat)

res <- rma(yi, vi, data=dat) 
res

predict(res, digits=3, transf=transf.ztor)

confint(res)


# Bajaut Plot -------------------------------------------------------------

b_res <- rma(yi, vi, data=dat, slab=study_id)  # New meta-analysis with study ID identifier 
baujat(b_res)
inf <- influence(res)
print(inf)


# Forest Plot -------------------------------------------------------------

forest(res, xlim=c(-1.6,1.6), atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6)), digits=c(2,1), cex=.8)
text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)


# Publication Bias --------------------------------------------------------

funnel(res, xlab = "Correlation coefficient")
regtest(res)
ranktest(res)
fsn(yi, vi, data = dat, type="Rosenthal", alpha=.05)


# Trim and Fill Procedure -------------------------------------------------

## Load and View the biased dataset ---------------------------------------
dat_bias <- read.csv("dat_bias.csv")
View(dat_bias)

## Run a meta-analysis with random effect on the new dataset---------------
res.b <- rma(yi, vi, data=dat_bias) 
res.b 
confint(res.b)
funnel(res.b, xlab = "Correlation coefficient") 
regtest(res.b)
ranktest(res.b)
res.tf <- trimfill(res.b)
res.tf
funnel(res.tf, xlab = "Correlation coefficient")


# Moderation Analyses -----------------------------------------------------

res.modage <- rma(yi, vi, mods = ~ meanage, data=dat) 
res.modage

res.modq <- rma(yi, vi, mods = ~ quality, data=dat)
res.modq

res.mes <- rma(yi, vi, mods = ~ factor(controls), data=dat)
res.mes
