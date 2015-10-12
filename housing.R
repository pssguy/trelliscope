
## standard libraries

library(dplyr)
library(ggplot2)
library(datadr)
library(trelliscope)

#library(shiny)
library(shinyapps)

## housing - work from zillow data
library(housingData)


conn <- vdbConn("vdbHousing", name = "tesseraTutorial")



byCounty <- divide(housing,
                   by = c("county", "state"))



panel_ggplot <- function(x)
  ggplot(data=x,aes(x=time,y=medListPriceSqft))+
  geom_point(colour="blue") +
  ylab("List Price $ per sq ft") +
  xlab("") +
  ggtitle("Average Monthly House Listing Prices - Zillow Data") +
  theme_bw()

## test
#panel_ggplot(byCounty[[5]]$value)

##http://www.zillow.com/homes/Abbeville-County-SC_rb/
#zillowString <- gsub(" ", "-", do.call(paste, getSplitVars(byCounty)))

#cognostics function to be applied to each plot
priceCog <- function(x) {
  zillowString <- gsub(" ", "-", do.call(paste, getSplitVars(x)))
  list(
    slope = cog(coef(lm(medListPriceSqft ~ time, data = x))[2],
                desc = "list price slope"),
    meanList = cogMean(x$medListPriceSqft), ## cogMean prob already removes NAs
    nObs = cog(length(which(!is.na(x$medListPriceSqft))),
               desc = "number of non-NA list prices"),
    zillowHref = cogHref(
      sprintf("http://www.zillow.com/homes/%s_rb/", zillowString),
      desc = "zillow link")
  )
}



## prob making a ggplot getting
#Error : Discrete value supplied to continuous scale

#lattice can do 
#At least one of the variables is not numeric.  Casting as numeric for quantile calculation purposes
## so until resolved

makeDisplay(byCounty,
                        panelFn = panel_ggplot,
                        cogFn   = priceCog,
                        name    = "list_vs_time_xy_free",
                        desc    = "List price per square foot vs. time with x and y axes free")


deployToShinyApps(vdbConn = getOption("vdbConn"), appName = "trelliscope",
                  account = "mytinyshinys", redeploy = TRUE, size = NULL, instances = NULL,
                  quiet = FALSE)



# *** Copying latest viewer to vdb directory...
# *** Syncing local data...
# Error in findRsync() : 
#   rsync executable not found. Use 'rsync' argument to specify the correct path.

### went into the vdbHousing directory and opened server file then published to trelliscope as trelliscope_Housing
### seems to work ok
## can remove displays from folder presumably so they dont show up


#view()

## currently easiest way to publish is to open relevant directory and then publish

##
# makeDisplay(byCounty,
#             panelFn = panel_ggplot,
#             cogFn   = priceCog,
#             name    = "list_vs_time_xy_same",
#             desc    = "List price per square foot vs. time with x and y axes same",
# #             lims    = list(x = "same", y = "free"))
# 
# 
# preFn <- function(d) {
#   list(
#     xlim = range(d$time, na.rm = TRUE),
#     ylim = range(d$medListPriceSqft, na.rm = TRUE)
#   )
# }
# 
# # compute axis limits prior to creating display using prepanel() _ 
# pre <- prepanel(byCounty, prepanelFn = preFn)
# 
# makeDisplay(byCounty,
#             panelFn = panel_ggplot,
#             cogFn   = priceCog,
#             name    = "list_vs_time_xy_same",
#             desc    = "List price per square foot vs. time with x and y axes same",
#             lims    = list(x = "same", pre))
# 
# view()