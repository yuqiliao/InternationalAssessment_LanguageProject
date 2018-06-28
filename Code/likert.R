#' @title Produce a likert chart using an edsurveyTable object.
#' @name likert
#' @description Produce a likert chart using an edsurveyTable object.
#' @param sdf an \code{edsurvey.data.frame.list}
#' @param data an \code{edsurveyTable}.
#' @param LikertVar character vector specifying the variable that has a Likert-type scale.
#' @param byVar character vector specifying the variable that the likert chart is plotted by.
#' @param levelLabels character vector specifying user-defined value labels (response categories) of an questionnaire item, which will be shown as the legend of the likert chart. The default level labels will be the ones shown in the \code{edsurveyTable}.
#' @param pal character vector assigning the color pallettes for each value labels specified in \code{levels}
#' @param ascending a logical value specifying the order of \code{byVar} in the likert chart.
#' @param ChartTitle character vector specifying the title of the likert chart.
#' @param LegendTitle character vector specifying the a logical value specifying the title of \code{LikertVar} in the likert chart.
#' @return A ggplot2 plot object
#' @author Paul Bailey and Yuqi Liao
#' @example \man\examples\likert.R
#' @import ggplot2
#' @import reshape2
#' @export

likert <- function(sdf,
                   data,
                   LikertVar,
                   byVar,
                   levelLabels,
                   pal,
                   ascending,
                   ChartTitle,
                   LegendTitle) {
  
  ##define "levels"
  
  if(missing(levelLabels) || is.null(levelLabels)){
    #if "levelLabel" is undefined or defined as "NULL", define "levels" from "data"
    lv <- data[,LikertVar]
    if(inherits(lv, "factor")) {
      levels <- levels(lv)
    } else {
      levels <- sort(unique(lv))
    }
  } else {
    #if "levelLabel" is defined as anything else, check if "levelLabel" could match the levels perfectly, if not, return an error message. [questions for Paul/Michael: I wanted to give users more flexibility here so that it is okay if the user-defined "levelLabel" does not match the exact levels form data, but the challenge is to correctly link user-defined "levelLabel" with the levels from data. Do you have any recommendations?].
    lv <- data[,LikertVar]
    if(sum(!unique(lv) %in% levelLabels)>0) {
      stop(paste0("Dang, values not in levelLabels you specified!",paste(dQuote(unique(lv)[!unique(lv) %in% levelLabels]), collapse=", ")))
    } else {
      levels <- levelLabels #Ask Paul: is there a way to have users customize the level labels? So that "Every day" could be perhaps called "every day"?
     }
    }

  
  ##reshape the EdsurveyTable data
  data <- data[,c(byVar, LikertVar,"PCT")]
  data <- reshape(data, idvar = byVar, timevar = LikertVar, direction = "wide")
  

#  colnames <- colnames(data)
#  levels <- colnames[-1] #I don't want the levels to be what "levelsSDF(LikertVar,sdf)" will return, becuase sometimes (and in this case), there will be omitted and other categories.
#  levels <- substr(levels, 5, nchar(levels))
  colnames(data) <- c(byVar,levels)
  colnames <- colnames(data)


  ##setting up a data frame
  premdfr <- data.frame(column.name=data[,byVar],stringsAsFactors=FALSE)
  colnames(premdfr) <- byVar
 
  #for each level, find the "start" and "end" points
  premdfr[,paste0("start.", levels)] <- t(apply(cbind(rep(0, nrow(data)), data[,levels]),1,cumsum))[,-(length(levels)+1)]
  premdfr[,paste0("end.", levels)] <- t(apply(data[,levels],1, cumsum))
  
  #define centerLevel for odd number of value labels, or define levelBeforeCenterLine for even number of value labels
  if(ncol(data) %% 2 == 0){
    centerLevel <- colnames[ncol(data)/2+1]
    premdfr[,paste0("mid")] <- 
      (premdfr[,paste0("start.", centerLevel)] + premdfr[,paste0("end.", centerLevel)])/2
  } else {
    levelBeforeCenterLine <- colnames[(ncol(data)+1)/2]
    premdfr[,paste0("mid")] <- premdfr[,paste0("end.", levelBeforeCenterLine)]
  }

  #now, move everything to the middle of centerLevel
  premdfr[,paste0("start.", levels)] <- premdfr[,paste0("start.", levels)] - premdfr[,paste0("mid")]
  premdfr[,paste0("end.", levels)] <- premdfr[,paste0("end.", levels)] - premdfr[,paste0("mid")]
  
  #order by the starting points of the first level
  if(ascending){
    premdfr[,byVar] <- factor(premdfr[,byVar], levels = premdfr[,byVar][order((premdfr[,paste0("start.", levels[1])]))])
  } else {
    premdfr[,byVar] <- factor(premdfr[,byVar], levels = premdfr[,byVar][order(-(premdfr[,paste0("start.", levels[1])]))])
  }

  ##restructure the data frame
  # select "start" columns
  startcolumns <- premdfr[,c(byVar, paste0("start.", levels))]
  # select "end" columns
  endcolumns <- premdfr[,c(byVar, paste0("end.", levels))]
 
  #melt "start" columns & change level labels for merging
  melt.startcolumns <- melt(startcolumns, id=byVar)
  colnames(melt.startcolumns) <- c(byVar,"variable","start")
  melt.startcolumns$variable <- droplevels(melt.startcolumns$variable)
  levels(melt.startcolumns$variable) <- levels
  
  #melt "end" columns & change level labels for merging
  melt.endcolumns <- melt(endcolumns, id=byVar)
  colnames(melt.endcolumns) <- c(byVar,"variable","end")
  melt.endcolumns$variable <- droplevels(melt.endcolumns$variable)
  levels(melt.endcolumns$variable) <- levels
  
  #merge
  mdfr <- merge(melt.startcolumns,melt.endcolumns, by=c(byVar, "variable"))
  
  ##Define ChartTilte and LegendTtile
  if(missing(ChartTitle)) {
    LikertVar.name <- searchSDF(LikertVar,sdf)
    LikertVar.name <- LikertVar.name$Labels
    
    byVar.name <- searchSDF(byVar,sdf)
    byVar.name <- byVar.name$Labels
    
    ChartTitle <- paste0("Percentage Distribution of ", LikertVar.name, " [", LikertVar, "], by ", byVar.name, " [", byVar, "]")
  } 
  
  if(missing(LegendTitle)) {
    LikertVar.name <- searchSDF(LikertVar,sdf)
    LikertVar.name <- LikertVar.name$Labels
    
    byVar.name <- searchSDF(byVar,sdf)
    byVar.name <- byVar.name$Labels
    
    LegendTitle <- paste0(LikertVar.name, " [", LikertVar, "]")
  }
  

  #use geom_segment to plot the data
    p <- ggplot(data=mdfr) +
    geom_segment(aes_string(x = byVar, y = "start", xend = byVar, yend = "end", colour = "variable"), size = 3.5) +
    geom_hline(yintercept = 0, color =c("#646464")) +
    coord_flip() +
    scale_color_manual(LegendTitle, labels = levels, values = pal, guide="legend") +
    labs(title=ChartTitle, y="Percent",x="") +
    scale_y_continuous(breaks=seq(-100,100,25), limits=c(-100,100)) +
    theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_line(colour = "#CBCBCB"))
 
  p
}
