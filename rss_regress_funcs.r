# requires tidyverse and gridExtra packages.

#--------------------------------------------------------------------------
# Plot response versus predictor  for a simple linear regression
#   model       Model object created by lm()
#   title2      Subtitle
#   line        TRUE to plot regression line
#   outlierVal  Labels points with |standardized residual| > outlierVal
responsePredictor <- function(model, title2='',line=FALSE,outlierVal=2 ){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") %>%
        mutate(labelU = if_else(.stdresid > outlierVal,label," "),
               labelL = if_else(.stdresid < -1*outlierVal,label," "))
    gg <-  ggplot(aes(x=fmodel[,3], y=fmodel[,2]), data=fmodel) +
        geom_point() +
        geom_text(aes(label=labelU),
                  nudge_y = if_else(sign(max(fmodel[,2])) ==
                                    sign(min(fmodel[,2])), 
                            0.10*(max(fmodel[,2]) - min(fmodel[,2])),
                            0.10*max(fmodel[,2]))) +
        geom_text(aes(label=labelL),
                  nudge_y = if_else(sign(max(fmodel[,2])) ==
                                        sign(min(fmodel[,2])), 
                                    -0.10*(max(fmodel[,2]) - min(fmodel[,2])),
                                   - 0.10*min(fmodel[,2]))) +
        labs(x=names(fmodel)[3],
             y=names(fmodel)[2],
             title=paste("Response vs Predictor", title2, sep='\n'))
    if (line==TRUE) {
        gg <- gg + geom_abline(aes(intercept=coef(model)[1],slope=coef(model)[2]))
    }
    gg
}

#----------------------------------------------------------------------
#  Plots residuals vs fitted values
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
residFit <- function(model, title2='',outlierVal=2){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") %>%
        mutate(labelU = if_else(.stdresid > outlierVal,label," "),
               labelL = if_else(.stdresid < -1*outlierVal,label," "))
    ggplot(aes(x=.fitted, y=.resid), data=fmodel) +
        geom_point() +
        geom_hline(yintercept=0) +
        geom_smooth(se = TRUE) +
        geom_text(aes(label=labelU),
                   nudge_y= 0.10*max(fmodel$.resid)) +
        geom_text(aes(label=labelL),
                  nudge_y= 0.10*min(fmodel$.resid)) +
        labs(x="Fitted Values",
             y="Residuals",
             title=paste("Residuals vs Fitted: ", title2, sep="\n")) 
}

# --------------------------------------------------------------------
# Plots residuals vs data order or date
#   model       Model object created by lm()
#   data        If useDate is TRUE, data is the data frame containing date
#   useDate     TRUE for x to be the date in data frame data
#   title2      Subtitle
residOrder <- function(model, data=NULL, useDate=FALSE, title2='') {
    fmodel <- fortify(model)
    if (useDate == TRUE) {
         fmodel <- rownames_to_column(fmodel, var=".key.") %>%
            select(.key.,.resid)
         data2  <- rownames_to_column(data, var=".key.")
         data2  <- inner_join(fmodel,data2, by=".key.")
         gg <- ggplot(data2, aes(x=date, y=.resid))+
               labs(x="date",
                    title=paste("Residuals vs Date",
                                title2, sep="\n"))
    } else {
         gg <- ggplot(fmodel, aes(x=seq_along(.resid), y=.resid)) +
               labs(x="order",
                    title=paste("Residuals vs Order",
                                title2, sep="\n"))
    }
    gg + geom_point() +
         geom_hline(yintercept=0) +
         geom_smooth(se = TRUE) +
         labs(y="Residuals")
}

#--------------------------------------------------------------------
# QQ plot
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
normalQQ <- function(model, title2='', outlierVal=2) {
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") %>%
        mutate(.stdresid = if_else(near(.resid, 0.), 0., .stdresid),
               labelU = if_else(.stdresid > outlierVal,label," "),
               labelL = if_else(.stdresid < (-1)*outlierVal,label," "),
              .r. =rank(.stdresid),
              .x. = qnorm((.r.-0.5)/(length(.stdresid)))
              )
    ggplot(fmodel, aes(sample=.stdresid)) +stat_qq() + geom_qq_line() +
        geom_text(aes(x=.x., y=.stdresid, label=labelU),
                  nudge_y= 0.10*max(fmodel$.stdresid)) +
        geom_text(aes(x=.x., y=.stdresid, label=labelL),
                  nudge_y= 0.10*min(fmodel$.stdresid)) +
        labs(x="Theoretical",
             y="Standaridized residuals",
             title=paste("Normal Q-Q:", title2
                         , sep="\n")) 
}

#---------------------------------------------------------------
# Plots sqrt(|residual|) vs fitted vakues
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
scaleLocation <- function(model, title2='', outlierVal=2 ){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label")  %>%
        mutate(labelU = if_else(abs(.stdresid) > outlierVal,label," "))
    ggplot(aes(x=.fitted, y=sqrt(abs(.stdresid))), data=fmodel) +
        geom_point() +
        geom_smooth(se = FALSE,span=1) +
        geom_text(aes(label=labelU),
                  nudge_y= 0.10*(max(sqrt(abs(fmodel$.resid)))-
                                 min(sqrt(abs(fmodel$.resid)))))+
        labs(x="Fitted Values",
             y="Sqrt(|Standardized residuals|)",
             title=paste("Scale-Location: ", title2, sep="\n"))
}

#--------------------------------------------------------------
#  Plots Cooks distance vs data order
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal or
#                   Cooks distance > 0.5
cooksDistance <- function(model,title2='',outlierVal=2){
    fmodel <- fortify(model)
    fmodel <- rownames_to_column(fmodel,var="label") 
    fmodel <- mutate(fmodel,
               label2 = if_else(abs(.stdresid)>outlierVal |
                                    .cooksd > 0.5,label," "))
    ggplot(aes(x=seq_along(.cooksd),y= .cooksd), data=fmodel) +
        geom_col() +
        geom_text(aes(label=label2),
                  position="stack",vjust=-.025) + 
        labs(x="Obs number",
             y="Cook's distance",
             title=paste("Cook's Distance: ", title2,  sep="\n")) 
}

#--------------------------------------------------------------------------
# Plots standardized residuals vs leverage.  Size of plotting symbol
# indicates Cooks distance.
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal or
#                   Cooks distance > 0.5 or leverage > 2p/n
residLeverage <- function(model,title2='',outlierVal=2){
    p <- length(coef(model))
    fmodel <- fortify(model) %>%
              mutate(.stdresid = if_else(near(.resid, 0.), 0., .stdresid)) %>%
              mutate(.cooksd = if_else(near(.resid, 0.), 0., .cooksd))
    n <- length(fmodel$.stdresid)
    fmodel <- rownames_to_column(fmodel,var="label")  %>%
        mutate(labelU = if_else((.stdresid > outlierVal) |
                                   (.stdresid>0 &
                                      (.cooksd>0.5 | .hat>2*p/n)),label," "),
               labelL = if_else(.stdresid < -1*outlierVal |
                                   (.stdresid<0 &
                                      (.cooksd>0.5 | .hat>2*p/n)),label," "))
    ggplot(aes(x=.hat, y=.stdresid), data=fmodel) +
        geom_point(aes(size=.cooksd), na.rm=TRUE) +
        geom_hline(yintercept=0) +
        geom_vline(xintercept=2*p/n, linetype="dashed") +
        geom_vline(xintercept=3*p/n, linetype = "dashed") +
        geom_text(aes(label=labelU),
                  nudge_y= 0.10*max(fmodel$.stdresid)) +
        geom_text(aes(label=labelL),
                  nudge_y= 0.10*min(fmodel$.stdresid)) +
        labs(x="Leverage",
             y="Standardized residuals",
             title=paste("Residuals vs Leverage: ", title2,  sep="\n")) +
        scale_size_continuous("Cook's Distance", range=c(1,5)) +
        theme(legend.position="bottom")
}

#--------------------------------------------------------------------------
# Regression diagnostic plots
#   model       Model object created by lm()
#   title2      Subtitle
#   outlierVal  Labels points with |standardized residual| > outlierVal
diagPlots123 <- function(model,title2, outlierVal=2) {
    grid.arrange(residFit(model,title2,outlierVal=2),
                 normalQQ(model,title2,outlierVal=2),
                 scaleLocation(model,title2,outlierVal=2),
                 ncol=2)
}

diagPlots45 <- function(model,title2, outlierVal=2) {
    grid.arrange(cooksDistance(model,title2),
                 residLeverage(model,title2,outlierVal=2),
                 ncol=2)
}