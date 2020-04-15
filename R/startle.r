#' @title Load Startle Data
#' @description This function is used for loading and processing all data related to startle experiments. All data generated with PASTA, Platform for Acoustic STArle experiments, should be placed in a single folder. Navigate to this folder, set it as working directory, and call this function. All data will be loaded, processed and assigned to a chosen variable that can be passed on other functions.
#'
#' @param local_import An argument with default value TRUE. If set to TRUE it will load all .pasta files from the working directory and merge them apropriately. If set to FALSE, a user must specify group_names.
#' @param group_names An argument used only if local_import = FALSE. A vector of strings with names of .pasta files pertaining to startle data.
#' @param addhead Optional argument. Adds a number of seconds to the duration of the impulse. Acounts for the lag of the animal.
#' @param addtail Optional argument. Adds a number of seconds to the duration of the impulse. Acounts for the lag of the animal.
#' @param metadata Optional argument. For a custom made pulse protocol.
#' @param correction Argument with default value TRUE. If set to TRUE, values will be corrected for the animal mass.
#' @param synchronise Argument with default value FALSE. If set to TRUE, timestamps will be corrected based on the synchroniseTime.csv in the working directory.
#'
#' @return A dataframe with all startle data
#' @importFrom plyr revalue
#' @importFrom magrittr %>% set_colnames
#' @importFrom dplyr mutate filter left_join select
#' @importFrom lubridate seconds
#' @importFrom stringr str_replace
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#' @importFrom rjson fromJSON
#' @importFrom tidyr gather
#' @export
#'
#' @examples
#' \dontrun{
#' df <- loadStartleData(df, addhead = 0.2, addtail = 0.2)
#' # OR
#' df <- loadStartleData(local_import=FALSE, group_names=c("ctr 1", "ctr 2"), addhead=0.1, addtail=1)
#' }
loadStartleData <- function(local_import = TRUE, group_names,  addhead, addtail, metadata, correction = TRUE, synchronise = FALSE){

  if(missing(addhead)){
    addhead <- 0
  }
  if(missing(addtail)){
    addtail <- 0
  }

  if(missing(metadata)){
    metadata <-  map
  }

  if (local_import == FALSE){

    if(missing(group_names)){
      stop("Group names are missing. If local_import = FALSE, group_names are obligatory argument")
    }

    group_names <- group_names %>% str_replace(".pasta", "")

    df <- data.frame()
    c <- c()
    for(i in group_names){
      i <- paste(i, ".pasta", sep = "")

      c <- c(c,i)

    }

    l <- lapply(c, read.csv)

  }


  if(local_import == TRUE){
    group_names <- list.files(pattern="*.pasta")
    l <- lapply(group_names, read.csv)
  }

  group_names <- tolower(group_names %>% str_replace(".pasta", ""))

  names(l) <- group_names

  if(synchronise == TRUE){
    corx <- read.csv(file = "synchroniseTime.csv", sep = ";") %>%
      mutate(Group = tolower(paste(Group, Animal))) %>%
      select(Group, Offset)
    for(i in 1:dim(corx)[1]){

      l[[corx[i,1]]][,1] <- l[[corx[i,1]]][,1] + corx[i,2]

    }

  }



  df <- as.data.frame(l[1]) %>%
    set_colnames(c("time", "value")) %>%
    mutate(group = group_names[1])

  for(i in 2:length(l)){
    bindingdf <- as.data.frame(l[i]) %>%
      set_colnames(c("time", "value")) %>%
      mutate(group = group_names[i])

    df <- rbind(df, bindingdf)

  }



  metadata <- metadata[metadata$impulsetype != "pause" & metadata$impulsetype != "pre",]
  for(i in 1:(dim(metadata)[1]-1)){
    metadata$end[i] <- metadata$start[i+1]
  }

  metadata <- metadata %>% select(-impulsetype)

  metadata$impulsetype <- NA
  metadata$impulsetype[metadata$stage == 1] <- "W/O PP"
  metadata$impulsetype[metadata$stage == 2] <- "W PP"

  metadata$start[metadata$impulse == TRUE] <- metadata$start[metadata$impulse == TRUE] - addhead*1000
  metadata$start[metadata$impulse == FALSE] <- metadata$start[metadata$impulse == FALSE] + addtail*1000
  metadata$end[metadata$impulse == TRUE] <- metadata$end[metadata$impulse == TRUE] + addtail*1000
  metadata$end[metadata$impulse == FALSE] <- metadata$end[metadata$impulse == FALSE] - addhead*1000


  metadata$start[1] <- 0
  metadata$end[dim(metadata)[1]] <- 210740
  metadata$impulsetype <- as.character(metadata$impulsetype)

  df$cycle <- NA
  df$impulsetype <- NA
  df$impulse <- NA
  df$stage <- NA

  for(i in 1:dim(metadata)[1]){
    df$cycle[df$time >= metadata$start[i] & df$time <= metadata$end[i]] <- metadata$cycle[i]

    df$impulse[df$time >= metadata$start[i] & df$time <= metadata$end[i]] <- metadata$impulse[i]

    df$impulsetype[df$time >= metadata$start[i] & df$time <= metadata$end[i]] <- metadata$impulsetype[i]

    df$stage[df$time >= metadata$start[i] & df$time <= metadata$end[i]] <- metadata$stage[i]

    df$order[df$time >= metadata$start[i] & df$time <= metadata$end[i]] <- metadata$order[i]

  }

  df <- na.omit(df)



  df <- df %>%
    mutate(time = seconds(time/1000), value = abs(value), lgroup = gsub( " .*$", "", group))  #gsub('[[:digit:]]+', '', group)

  df$time2 <- as.numeric(df$time) * 1000

  for(i in levels(factor(df$order))){

    if (i == 0){
      next()
    }

    df$time2[df$order == i] <- df$time2[df$order == i] - min(na.omit((df$time2[df$order == i])))

  }

  df <- df %>% mutate(impulse = factor(as.character(impulse)))
  df$impulse <- revalue(df$impulse, c("TRUE" = "I", "FALSE"="No I"))
  df <- df %>% filter(stage != 0) %>% mutate(stage = factor(as.character(stage)))
  df$stage <- revalue(df$stage, c("1" ="W/O PI", "2"="W PI"))



  ## correction for mass
  if(correction == TRUE){

    list.json.files <- list.files(pattern="*.json")

    if("mass.json" %in% list.json.files){
      mass_correction <- as.data.frame(fromJSON(file = "mass.json"))
      mass_correction <- gather(mass_correction, group, mass)
      mass_correction <- mass_correction %>%
        mutate(correction_factor = mass / mean(mass_correction$mass)) %>%
        mutate(group = gsub(".",  " ", group, fixed = TRUE)) %>%
        select(group, correction_factor)

      df <- df %>%
        left_join(mass_correction, by = "group") %>%
        mutate(value = value / correction_factor) %>%
        select(-correction_factor)
    } else {
      warning("File mass.json is missing from the working directory. Correction for mass was not conducted.")
    }

  }


  return(df)

}




#' @title Basic Startle Plot
#' @description Returns a simple time series plot.
#'
#' @param df A dataframe returned by loadStartleData function.
#' @param filter_groups An optional argument, a vector of strings used for filtering the data and displaying only wished groups on plots.
#' @param n_col An optional argument, defines number of graph columns.
#'
#' @return Returns a time series plot.
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot geom_line facet_wrap scale_x_time theme_classic aes
#' @export
#'
#' @examples
#' \dontrun{
#' ## df is created with loadStartleData function
#' basicStartlePlot(df)
#' }
basicStartlePlot <- function(df, filter_groups, n_col){

  if(!missing(filter_groups)){
    df <- filter(df, group %in% filter_groups)
  }
  if(missing(n_col)){
    n_col <- round(sqrt(length(levels(factor(df$group)))))
  }


  return(ggplot(df, aes(x = time, y = value)) + geom_line() + facet_wrap( ~ group, ncol = n_col) +
    scale_x_time() + theme_classic())

}




#' @title Startle Plot
#' @description  Returns several types of plots, based on the type argument. Type 1 returns a dot plot portraying mean values during the pulse and between pulses for tested groups; type 2, returns a similar result as type 1, but as a boxplot; type 3 displays only values pertaining to pulse period; type 4 displays distribution of values as violin plots; type 5 is a time-series plot with all startle tries overlapped, color indicates whether prepulse was used; type 6 is a boxplot displaying ratio of values recorded during pulse and in between pulses.
#'
#' @param df A dataframe returned by loadStartleData function.
#' @param type Obligatary argument. A number 1, 2 or 3 that specifies which plot will be returned. For more information see th vignette.
#' @param filter_major_groups An optional argument, a vector of strings used for filtering the data and displaying only wished major groups on plots.
#' @param yrange An optional argument used for zooming in. Should be defined as a vector with min and max y axis values (e.g. c(0,10) - y axis will have min value of 0 and max value of 10 )
#' @param n_col An optional argument, defines number of graph columns.
#'
#' @return Returns several types of plots based on type.
#' @importFrom dplyr filter mutate group_by ungroup summarise
#' @importFrom ggplot2 ggplot geom_point facet_wrap theme_classic geom_pointrange stat_summary geom_violin xlab ylab theme scale_y_log10 geom_point aes element_blank geom_blank geom_boxplot coord_cartesian
#' @importFrom ggsci scale_fill_startrek scale_color_startrek
#' @importFrom stats IQR
#' @importFrom lubridate seconds
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' \dontrun{
#' ## df is created with loadStartleData function
#' startlePlot(df, type = 1)
#' }
#'
startlePlot <- function(df, type, filter_major_groups, yrange, n_col){

  if(!missing(filter_major_groups)){
    df <- df %>% filter(lgroup %in% filter_major_groups)
  }
  if(missing(n_col)){
    n_col <- round(sqrt(length(levels(factor(df$group)))))
  }

  if(missing(type)){
    stop('Argument "type" is missing. Specifiy type to 1, 2 or 3.')
  }

  if(missing(yrange)){
    geomadd <- geom_blank()
  } else {
    geomadd <- coord_cartesian(ylim=yrange)
  }



  if(type == 5){
    return(df %>%
      mutate(stage = as.factor(stage), time2 = seconds(time2 / 1000)) %>%
      ggplot(aes(x = time2, y = value, color = stage)) + geom_line(alpha = 0.3) + facet_wrap(~ lgroup, ncol = n_col) +
      theme_classic() + scale_color_startrek() + xlab("Time during 1 cycle [seconds]") + ylab("Value [g]") +geomadd )
  }


  if(type == 4){
    return(df %>%
      mutate(gr = paste(stage, impulse)) %>%
      group_by(gr) %>%
      mutate(outlier = value > median(value) + IQR(value) * 1.5) %>%
      ungroup() %>%
      mutate(value = value + 1, time2 = seconds(time2 / 1000)) %>%
      ggplot(aes(x = gr, y = value, fill = stage)) + geom_violin() +
      stat_summary(fun.y='median', geom='point', size=2)+
      geom_pointrange(mapping = aes(x = gr, y = value),
                      stat = "summary",
                      fun.ymin = function(z) {quantile(z,0.25)},
                      fun.ymax = function(z) {quantile(z,0.75)},
                      fun.y = median)   +
      facet_wrap(~ lgroup, scales = "free_x", ncol = n_col) +
      theme_classic() + scale_fill_startrek() + scale_y_log10() +
      theme(legend.title = element_blank()) + xlab("With/Without PreImpulse - during Impulse / Between Impulses") +
      ylab("Value [g]") + geomadd)
  }

  if(type == 1){
    return(
    df %>%
      mutate(stage = as.factor(stage), time2 = seconds(time2 / 1000)) %>%
      group_by(group, impulse, stage, lgroup) %>%
      summarise(value = mean(value)) %>%
      ggplot(aes(x = impulse, y = value, color = stage)) + geom_point(position = "jitter", size = 2) + facet_wrap(~ lgroup, scales = "free_x", ncol = n_col) +
      theme_classic() + scale_color_startrek() +
      xlab("Impulse") + ylab("Value [g]") + geomadd
    )
  }

  if(type == 2){
    return(
    df %>%
      mutate(stage = as.factor(stage), time2 = seconds(time2 / 1000)) %>%
      group_by(group, impulse, stage, lgroup) %>%
      summarise(value = mean(value)) %>% mutate(x = paste(lgroup, impulse, stage)) %>%
      ggplot(aes(x = x, y = value, fill = stage))  +
      theme_classic() + scale_fill_startrek() + theme(axis.title.x = element_blank(), legend.title = element_blank()) +
      ylab("Value [g]") + geom_boxplot() + geomadd
    )
  }

  if(type == 3){
    return(
      df %>%
        mutate(stage = as.factor(stage), time2 = seconds(time2 / 1000)) %>%
        group_by(group, impulse, stage, lgroup) %>%
        summarise(value = mean(value)) %>%
        filter(impulse == "I") %>%
        mutate(x = paste(lgroup, stage)) %>%
        ggplot(aes(x = x, y = value, fill = stage))  +
        theme_classic() + scale_fill_startrek() + theme(axis.title.x = element_blank(), legend.position = "None") +
        ylab("Value [g]") + geom_boxplot() + geomadd
    )
  }

  if(type == 6){

    df  <- data.frame(df %>% group_by(stage, order, impulse, impulsetype, group, lgroup) %>% summarise(median = median(value)) %>% ungroup())
    df <- df %>% mutate(gr = paste(group, order))
    df <- df %>% filter(impulse == "I") %>%
      left_join(df %>% filter(impulse == "No I") %>% select(gr, median), by = "gr") %>%
      mutate(ratio = median.x / median.y) %>%
      select(stage, order, impulse, impulsetype, group, lgroup, gr, ratio) %>%
      mutate(lgit = paste(lgroup, impulsetype))

    return(ggplot(df, aes(x = lgit, y = ratio, color = lgit)) + geom_boxplot() + theme_classic() +
             theme(legend.position = "None"))


  }

}


#' @title Summarise Startle
#' @description Returns a mathematical summary of the startle data. Returned list with two data frames. First contains calculated median, interquartile range, mean and standard deviation for values pertaining to the period of the pulse and in between pulses. The second contains the results of the desired statistical test, by default Wilcox is used. User can specify which test will be used with method argument.
#'
#' @param df  A dataframe returned by loadStartleData function.
#' @param method A parameter defining what type of statistical testing will be used.
#'
#' @return A mathematical summary in a list.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise mutate select left_join filter
#' @importFrom stats IQR median sd
#' @importFrom ggpubr compare_means
#' @export
#'
#' @examples
#' \dontrun{
#' ## df is created with loadStartleData function
#' summariseStartle(df)
#' }
summariseStartle <- function(df, method = "wilcox.test"){
    p1 <- df %>%
      group_by(stage, impulse, lgroup) %>%
      summarise(mean(value), sd(value), min(value), median(value),  IQR(value), max(value))
    df  <- data.frame(df %>% group_by(stage, order, impulse, impulsetype, group, lgroup) %>% summarise(median = median(value)) %>% ungroup())
    df <- df %>% mutate(gr = paste(group, order))
    df <- df %>% filter(impulse == "I") %>%
      left_join(df %>% filter(impulse == "No I") %>% select(gr, median), by = "gr") %>%
      mutate(ratio = median.x / median.y) %>%
      select(stage, order, impulse, impulsetype, group, lgroup, gr, ratio) %>%
      mutate(lgit = paste(lgroup, impulsetype))
    p2 <- compare_means(ratio ~ lgit,  data = df, method = method)

    print(p1)
    print(p2)

    return(list("T1" = p1, "T2" = p2))

}









#' @title Latency plot
#' @description Returns two plots displaying latency in a list. Latency refers to the reaction of the test subject to the startling sound. It is time period from the startling sound to the time stamp of maximal values recorded.
#'
#' @param df A dataframe returned by loadStartleData function.
#' @param addhead Optional argument. If this argument is used in loadStartleData, set addhead to the value
#'
#' @return List with two plots
#' @importFrom dplyr filter select summarise group_by
#' @importFrom ggplot2 ggplot aes geom_point scale_y_continuous coord_flip scale_color_viridis_c xlab ylab labs
#' @importFrom magrittr %>%
#' @importFrom ggsci scale_color_startrek
#' @export
#'
#' @examples
#' \dontrun{
#' ## df is created with loadStartleData function
#' L <- latencyPlot(df, addhead = 0.2)
#' }
#'
latencyPlot <- function(df, addhead){
  if (!missing(addhead)){
    df$time2 <- df$time2 - addhead * 1000
  }

  df_res <- data.frame(time2 = NA, group = NA, lgroup = NA, order = NA)
  for (i in levels(factor(df$group))){

    for (j in levels(factor(df$order))){
      x <- df %>% filter(group == i, order == j)
      df_res <- rbind(df_res, x[x$value == max(x$value),] %>%
                        select(time2, group, lgroup, order))

    }
  }
  df_res <- df_res[-1,]



  return(list( "LatencyVsCycle" = df_res %>% group_by(order, group) %>% summarise(time2 = mean(time2)) %>%
                 ggplot(aes(x = group, y = time2, color = order)) + geom_point(position = "jitter", size = 0.8) +
                 scale_y_continuous(limits = c(0, 1000)) + coord_flip() + xlab("Group") + ylab("Latency [ms]") + scale_color_viridis_c() +
                 labs(color = "Cycle") + theme_classic(),
               "LatencyVsGroup" = df_res %>% group_by(order, group,lgroup) %>% summarise(time2 = mean(time2)) %>%
                 ggplot(aes(x = order, y = time2, color = lgroup)) + geom_point(position = "jitter", size = 0.8) +
                 scale_y_continuous(limits = c(0, 1000)) + coord_flip() + xlab("Cycle") + ylab("Latency [ms]") + scale_color_startrek() +
                 labs(color = "Group") + theme_classic()

  ))




}






#' @title global variables
#' @docType package
#' @name datasummary
#' @description defining global variables
utils::globalVariables(c("group", "time", "value", "read",
                         "impulse", "stage", "Temperature",
                         "Humidity", "lgroup", "time2", "gr",
                         "map", "mass", "correction_factor",
                         "Group", "Animal", "Offset", "x",
                         "impulsetype", "median.x", "median.y",
                         "ratio", "lgit"))





#' @title metadata
#' @description A metadata for identification of pulses
#' @format dataframe
"map"



