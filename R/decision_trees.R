calcTrees <- function(.data, save = FALSE) {
  # Classification Tree with rpart::rpart

  # fitCategories <<- rpart::rpart(Category ~ Shift + safety_A1 + safety_A2 + safety_A3 + safety_A4 +
  #                           safety_B1 + safety_B2 + safety_B3 + safety_B4 + safety_B5 + safety_B6 + safety_B7 +
  #                           safety_C1 + safety_C2 + safety_C3 + safety_C4 +
  #                           safety_D1 + safety_D2 + safety_D3 + safety_D4 +
  #                           batch_C1 + batch_C2 + batch_C3 + batch_C4,
  #                         method="class", data=.data, control = rpart::rpart.control(cp=0.01), model=TRUE)
  # plot_tree(fitCategories, save)

  # grow tree
  fitminWait <<- rpart::rpart(minWaitingParts ~ Shift + safety_A1 + safety_A2 + safety_A3 + safety_A4 +
                         safety_B1 + safety_B2 + safety_B3 + safety_B4 + safety_B5 + safety_B6 + safety_B7 +
                         safety_C1 + safety_C2 + safety_C3 + safety_C4 +
                         safety_D1 + safety_D2 + safety_D3 + safety_D4 +
                         batch_C1 + batch_C2 + batch_C3 + batch_C4,
                       method="anova", data=.data, control = rpart::rpart.control(cp=0.01), model=TRUE)
  plot_tree(fitminWait, save)

  fitminLB <<- rpart::rpart(minLeanBuffer ~ Shift + safety_A1 + safety_A2 + safety_A3 + safety_A4 +
                       safety_B1 + safety_B2 + safety_B3 + safety_B4 + safety_B5 + safety_B6 + safety_B7 +
                       safety_C1 + safety_C2 + safety_C3 + safety_C4 +
                       safety_D1 + safety_D2 + safety_D3 + safety_D4 +
                       batch_C1 + batch_C2 + batch_C3 + batch_C4,
                     method="anova", data=.data, control = rpart::rpart.control(cp=0.01), model=TRUE)
  plot_tree(fitminLB, save)

  fitminLT <<- rpart::rpart(minLT_Plant ~ Shift + safety_A1 + safety_A2 + safety_A3 + safety_A4 +
                       safety_B1 + safety_B2 + safety_B3 + safety_B4 + safety_B5 + safety_B6 + safety_B7 +
                       safety_C1 + safety_C2 + safety_C3 + safety_C4 +
                       safety_D1 + safety_D2 + safety_D3 + safety_D4 +
                       batch_C1 + batch_C2 + batch_C3 + batch_C4,
                     method="anova", data=.data, control = rpart::rpart.control(cp=0.01), model=TRUE)
  plot_tree(fitminLT, save)

  fitmaxOut <<- rpart::rpart(maxOut ~ Shift + safety_A1 + safety_A2 + safety_A3 + safety_A4 +
                        safety_B1 + safety_B2 + safety_B3 + safety_B4 + safety_B5 + safety_B6 + safety_B7 +
                        safety_C1 + safety_C2 + safety_C3 + safety_C4 +
                        safety_D1 + safety_D2 + safety_D3 + safety_D4 +
                        batch_C1 + batch_C2 + batch_C3 + batch_C4,
                      method="anova", data=.data, control = rpart::rpart.control(cp=0.01), model=TRUE)
  plot_tree(fitmaxOut, save)

  #printcp(fit) # display the results
  #plotcp(fit) # visualize cross-validation results
  #summary(fitCategories) # detailed summary of splits
}

list.rules.rpart <- function(model)
{
  if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
  #
  # Get some information.
  #
  frm     <- model$frame
  names   <- row.names(frm)
  ylevels <- attr(model, "ylevels")
  ds.size <- model$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      # The following [,5] is hardwired - needs work!
      cat("\n")
      cat(sprintf(" Rule number: %s ", names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- rpart::path.rpart(model, nodes=as.numeric(names[i]), print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    }
  }
}
