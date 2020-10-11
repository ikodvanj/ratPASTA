#' @title Load griPASTA Data
#' @description This function is used for loading and processing all data related to startle experiments. All data generated with PASTA, Platform for Acoustic STArle experiments, should be placed in a single folder. Navigate to this folder, set it as working directory, and call this function. All data will be loaded, processed and assigned to a chosen variable that can be passed on other functions.
#'
#' @return A dataframe with all startle data
#' @importFrom stringr str_remove
#' @export
#'
loadgriPASTA <- function(){


  group_names <- list.files(pattern="*pasta")
  l <- lapply(group_names, read.csv)
  names(l) <- str_remove(list.files(pattern="*pasta"), ".pasta")

  return(l)

}



#' @title Find peaks
#' @description Finds
#'
#' @param df Data from griPASTA
#' @param n Number of trials
#' @param range Estimated length of each trial in seconds.
#'
#' @return returns values of peaks
#' @importFrom dplyr arrange
#' @export
#' @example
#' \dontrun{
#' findPeaks(df, n, range)
#' }
findPeaks <- function(df, n, range){
  colnames(df) <-  c("time", "value")
  df$row <- seq(1, nrow(df), 1)
  peaks <- c()
  min <- data.frame(time=NA, value=NA)
  for(i in 1:n){
    peaks[i] <- df[df$value == min(df$value),][1]
    min <- rbind(min, df[df$value == min(df$value),c("time", "value")])
    if (round(df[df$value == min(df$value), "row"]-range/2/0.015 < 0)){
      df[0:as.numeric(round(df[df$value == min(df$value), "row"]+range/2/0.015)), "value"] <- 0
    } else if (round(df[df$value == min(df$value), "row"]-range/2/0.015 > max(df$row))){
      df[as.numeric(round(df[df$value == min(df$value), "row"]-range/2/0.015)):max(df$row), "value"] <- 0
    } else {
      df[as.numeric(round(df[df$value == min(df$value), "row"]-range/2/0.015)):as.numeric(round(df[df$value == min(df$value), "row"]+range/2/0.015)), "value"] <- 0
    }
  }
  min <- na.omit(min)
  return(arrange(min, time))
}


#' @title Plot function
#' @description Returns a line plot for each animal.
#'
#' @param df raw griPASTA data
#'
#' @return ggplot object
#' @importFrom ggplot2 ggplot aes geom_line xlab ylab
#' @export
#'
plotf <- function(df){
  colnames(df) <-  c("time", "value")
  df$time <-  df$time / 1000

  ggplot(df, aes(time,value)) + geom_line() +  xlab("Time (seconds)") + ylab("Value (gram)")

}



#' @title Artefact identifier
#' @description A function that behaves similar to a loop - reruns itself until user reports that no artifacts are present. If artifact is present, user is promted to enter starting and ending x coordinate of the artefact, and artefact is deleted.
#'
#' @param df data
#' @param usePlotly TRUE or FALSE argument. If set to TRUE, plotly will be used to render plot for each animal.
#'
#' @return Returns a processed griPASTA data
#' @importFrom utils menu
#' @importFrom plotly ggplotly
#'
errors <- function(df, usePlotly){
  colnames(df) <-  c("time", "value")
  if(usePlotly == TRUE){
    print(ggplotly(plotf(df)))
  } else {
    print(plotf(df))
  }
  ggplotly()
  x <- menu(c("Yes", "No"), title="Are there any artifacts present?")
  if (x == 1){
    start <- as.numeric(readline(prompt = "Enter starting x coordinate of the artifact (seconds):"))
    end <- as.numeric(readline(prompt = "Enter ending x coordinate of the artifact (seconds):"))
    df <- df[df$time < start * 1000 | df$time > end * 1000,]
    errors(df, usePlotly = usePlotly)
  } else {
    return(df)
  }

}






#' @title griPASTA main function
#' @description A function that takes in raw griPASTA data and behaves similar to a loop - continuously promts user exclude artifacts until no artifacts are present. If artifact is present, user is promted to enter starting and ending x coordinate of the artefact, and artefact is deleted.
#'
#' @param data Raw griPASTA data loaded with function loadgriPASTA or manually.
#' @param usePlotly TRUE or FALSE argument. If set to TRUE, plotly will be used to render plot for each animal.
#'
#' @return Returns measurements for each animal
#' @importFrom dplyr bind_rows mutate
#'
#' @export
#'
#' @example
#' \dontrun{
#' # data - imported with loadgriPASTA
#' findPeaks(data, usePlotly)
#' }
griPASTA <- function(data, usePlotly = TRUE){


  df_final <- data.frame(time = NA, value = NA, animal = NA)

  for(i in 1:length(data)){

    name <- names(data)[i]
    df <- data[[i]]
    message("New experiment.")

    df <- errors(df, usePlotly = usePlotly)

    n <- as.numeric(readline(prompt = "Enter number of peaks:"))
    range <- as.numeric(readline(prompt = "Enter estimated timespan of each peak (seconds):"))

    df_final <- bind_rows(
      df_final,
      findPeaks(df, n = n, range = range) %>% mutate(animal = name)
    )

  }

  return(df_final[-1,])

}






