#' This function will generate a droplet size distribution
#'
#' @param VMD dummy
#' @param rel_span dummy
#' @param DV0.1 dummy
#' @param DV0.9 dummy
#' @param Dsize dummy
#' @param fract_Dsize dummy
#' @param min_size dummy
#'
#' @return dummy
#' @export
#'
#' @importFrom stats qnorm
#' @importFrom minpack.lm nlsLM
#' @importFrom stats predict
#' @importFrom stats lm

generate_dsd_rr <- function(VMD, rel_span, DV0.1, DV0.9, Dsize, fract_Dsize, min_size){
  # initialize ----------------------------------------------------------------------------------------------------------------------------
  #define variables
  sd_low <- sd_up <- NA

  # read points based on input ----------------------------------------------------------------------------------------------------------------------------
  # vmd
  df_vmd <- data.frame(cum_fract = 0.5,
                       size = VMD)
  ds_p <- df_vmd

  # Dsize and fract_Dsize
  if(!missing(Dsize) & !missing(fract_Dsize)){
    df_D <- data.frame(cum_fract = fract_Dsize,
                       size = Dsize)
    ds_p <- rbind(ds_p, df_D)
  }

  # Dv0.1
  if(!missing(DV0.1)){
    df_DV0.1 <- data.frame(cum_fract = 0.1,
                           size = DV0.1)
    ds_p <- rbind(ds_p, df_DV0.1)
  }

  # Dv0.9
  if(!missing(DV0.9)){
    df_DV0.9 <- data.frame(cum_fract = 0.9,
                           size = DV0.9)
    ds_p <- rbind(ds_p, df_DV0.9)
  }

  #min_size
  if(missing(min_size)){
    min_size <- 10
  }

  # option 1 ----------------------------------------------------------------------------------------------------------------------------
  # rel_span
  if(!missing(rel_span) & missing(DV0.1) & missing(DV0.9) & missing(fract_Dsize) & missing (Dsize)){
    #calculate dv01 and dv09
    DV01 <- VMD - (rel_span*VMD)/2
    DV09 <- VMD + (rel_span*VMD)/2
    #create data frame and rbind
    df_1 <- data.frame(cum_fract = c(0.1,0.9),
                       size = c(DV01,DV09))
    ds_p <- rbind(ds_p, df_1)
    # determine sd_low
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV01-VMD)/z_low

    # option 2 ----------------------------------------------------------------------------------------------------------------------------
    # rel_span & DV0.1
  } else if(!missing(rel_span) & !missing(DV0.1) & missing(DV0.9) & missing(fract_Dsize) & missing (Dsize)){
    #calculate dv01 and dv09
    DV09 <- DV0.1 + (rel_span*VMD)
    #create data frame and rbind
    df_2 <- data.frame(cum_fract = c(0.9),
                       size = c(DV09))
    ds_p <- rbind(ds_p, df_2)

    # determine sd_low
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1-VMD)/z_low

    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV09-VMD)/z_up

    # option 3 ----------------------------------------------------------------------------------------------------------------------------
    # rel_span & DV0.9
  } else if(!missing(rel_span) & missing(DV0.1) & !missing(DV0.9) & missing(fract_Dsize) & missing (Dsize)){
    #calculate dv01 and dv09
    DV01 <- DV0.9 - (rel_span*VMD)
    #create data frame and rbind
    df_3 <- data.frame(cum_fract = c(0.1),
                       size = c(DV01))
    ds_p <- rbind(ds_p, df_3)

    # determine sd_low
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV01-VMD)/z_low

    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9-VMD)/z_up

    # option 4 ----------------------------------------------------------------------------------------------------------------------------
    # rel_span & Dsize
  } else if(!missing(rel_span) & missing(DV0.1) & missing(DV0.9) & !missing(fract_Dsize) & !missing (Dsize)){
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize-VMD)/zDsize
    #calculate DV01
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low*sd_low
    #calculate dv09
    DV09 <- DV01 + (rel_span*VMD)
    #create data frame and rbind
    df_4 <- data.frame(cum_fract = c(0.1,0.9),
                       size = c(DV01,DV09))
    ds_p <- rbind(ds_p, df_4)

    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV09-VMD)/z_up

    # option 5 ----------------------------------------------------------------------------------------------------------------------------
    # DV0.1 & DV0.9
  } else if(missing(rel_span) & !missing(DV0.1) & !missing(DV0.9) & missing(fract_Dsize) & missing (Dsize)){
    # determine sd_low
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1-VMD)/z_low

    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9-VMD)/z_up

    # option 6 ----------------------------------------------------------------------------------------------------------------------------
    # DV0.1 & DV0.9
  } else if(missing(rel_span) & !missing(DV0.1) & !missing(DV0.9) & !missing(fract_Dsize) & !missing (Dsize)){
    # determine sd_low
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1-VMD)/z_low

    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9-VMD)/z_up

    # option 7 ----------------------------------------------------------------------------------------------------------------------------
    # Dsize & DV0.9
  } else if(missing(rel_span) & missing(DV0.1) & !missing(DV0.9) & !missing(fract_Dsize) & !missing (Dsize)){
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize-VMD)/zDsize
    #calculate DV01
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low*sd_low
    #create data frame and rbind
    df_7 <- data.frame(cum_fract = c(0.1),
                       size = c(DV01))
    ds_p <- rbind(ds_p, df_7)
    # determine sd_up
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9-VMD)/z_up

    # option 8 ----------------------------------------------------------------------------------------------------------------------------
    # Dsize & DV0.1
  } else if(missing(rel_span) & !missing(DV0.1) & missing(DV0.9) & !missing(fract_Dsize) & !missing (Dsize)){
    #based on Dsize
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low1 <- (Dsize-VMD)/zDsize
    #based on DV0.1
    z_low <- stats::qnorm(0.1)
    sd_low2 <- (DV0.1-VMD)/z_low
    #mean
    sd_low <- (sd_low1+sd_low2)/2
    sd_up <- sd_low

    #calculate DV09
    z_up <- stats::qnorm(0.9)
    DV09 <- VMD + z_up*sd_up

    #create data frame and rbind
    df_8 <- data.frame(cum_fract = c(0.9),
                       size = c(DV09))
    ds_p <- rbind(ds_p, df_8)

    # option 9 ----------------------------------------------------------------------------------------------------------------------------
    # Dsize
  } else if(missing(rel_span) & missing(DV0.1) & missing(DV0.9) & !missing(fract_Dsize) & !missing (Dsize)){
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize-VMD)/zDsize
    sd_up <- sd_low
    #calculate DV01
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low*sd_low
    #calculate DV09
    z_up <- stats::qnorm(0.9)
    DV09 <- VMD + z_up*sd_up
    #create data frame and rbind
    df_9 <- data.frame(cum_fract = c(0.1, 0.9),
                       size = c(DV01, DV09))
    ds_p <- rbind(ds_p, df_9)
  }

  #check if sd_low and sd_up are available
  if(is.na(sd_up)){
    sd_up <- sd_low
  }
  if(is.na(sd_low)){
    sd_low <- sd_up
  }

  # calculate min and max ----------------------------------------------------------------------------------------------------------------------------
  # maximal droplet size
  zmax <- stats::qnorm(0.9999)
  DVmax  <- VMD + zmax*sd_up
  df_max_size <- data.frame(cum_fract = 1,
                            size = DVmax)
  ds_p <- rbind(ds_p, df_max_size)

  # minimal droplet size
  fract_min <- 0.001
  zmin <- stats::qnorm(fract_min)
  DVmin  <- VMD + zmin*sd_low
  if(DVmin < min_size){
    DVmin <- min_size
  }
  df_min_size <- data.frame(cum_fract = fract_min,
                            size = DVmin)
  ds_p <- rbind(ds_p, df_min_size)

  # fitting ----------------------------------------------------------------------------------------------------------------------------
  #calculate log10 of accumulated fractions
  ds_p$cum_fract_log <- log10(ds_p$cum_fract)
  #shift size by min size and cum_fract_log by 3
  ds_p$size <- ds_p$size - DVmin
  ds_p$cum_fract_log <- ds_p$cum_fract_log + abs(log10(fract_min))
  # fit nls model
  model <- minpack.lm::nlsLM(cum_fract_log~abs(log10(fract_min))*(1-exp(-(a*size)^b)),
                             data = ds_p,
                             start = list(a=0.03, b=0.8))
  #create prediction data frame
  pred <- data.frame(size = seq(DVmin,DVmax, by = 0.5),
                     exp = NA,
                     cum_fract =NA)
  #shift sizes
  pred$size <- pred$size - DVmin
  #predict exponent
  pred$exp <- stats::predict(model, data.frame(size=pred$size))
  # shift back
  pred$size <- pred$size + DVmin
  pred$exp <- pred$exp - abs(log10(fract_min))
  #calculate cum_fract
  pred$cum_fract <- 10^(pred$exp)

  #### stretch cum_fract to be 1 at DVmax ####
  # create data frame
  df_lm <- data.frame(size = c(DVmin,DVmax),
                      cum_fract = c(0,(1-max(pred$cum_fract))))
  # fit model
  model <- stats::lm(cum_fract ~ size, data=df_lm)
  # create data frame
  pred_lm <- data.frame(size = seq(DVmin,DVmax, by = 0.5),
                        cum_fract =NA)
  #predict stretch value
  pred_lm$cum_fract <- stats::predict(model, data.frame(size=pred_lm$size))
  # add stretch value to cum_fract of prediction
  pred$cum_fract <- pred$cum_fract + pred_lm$cum_fract
  #select size and cum_fract
  pred <- pred[,c(1,3)]

  # droplet classes ----------------------------------------------------------------------------------------------------------------------------
  # create data frame
  df_lm <- data.frame(size = c(100,900),
                      width = c(10,40))
  # fit model
  model <- stats::lm(width ~ size, data=df_lm)
  # create data frame
  pred_lm <- data.frame(size = seq(10,600, by = 5),
                        width =NA)
  # predict stretch value
  pred_lm$width <- stats::predict(model, data.frame(size=pred_lm$size))
  # create droplet class data frame
  dc <- data.frame(mean_size = NA,
                   min_size = NA,
                   max_size = NA,
                   lower_fract = NA,
                   upper_fract = NA,
                   fract = NA,
                   cum_fract = c(1:100))
  dc$cum_fract <- NA

  #first class
  dc$min_size[1] <- DVmin
  width_0 <- round(unname(stats::predict(model, data.frame(size=dc$min_size[1])))/5,0)*5
  if(width_0 < 10){
    width_0 <- 10
  }
  dc$max_size[1] <- dc$min_size[1] + width_0
  # itterate over droplet classes
  for(i in 2:nrow(dc)){
    dc$min_size[i] <- dc$max_size[i-1]
    width_0 <- round(unname(stats::predict(model, data.frame(size=dc$min_size[i])))/5,0)*5
    if(width_0 < 10){
      width_0 <- 10
    }
    dc$max_size[i] <- dc$min_size[i] + width_0
    if(dc$max_size[i] >= DVmax){
      break
    }
  }
  #clean up
  dc <- dc[!is.na(dc$min_size),]
  #calculate mean size
  dc$mean_size <- (dc$min_size+dc$max_size)/2
  #read fractions
  for(i in 1:nrow(dc)){
    #calculate lower fraction
    if(i == 1){
      dc$lower_fract[i] <- 0
    }else{
      dc$lower_fract[i] <- pred$cum_fract[which(pred$size == dc$min_size[i])]
    }

    #calculate upper fraction
    if(i == nrow(dc)){
      dc$upper_fract[i] <- 1
    }else{
      dc$upper_fract[i] <- pred$cum_fract[which(pred$size == dc$max_size[i])]
    }

    #calculate fraction
    dc$fract[i] <- dc$upper_fract[i] - dc$lower_fract[i]
  }

  #calculate accumulated fraction
  dc$cum_fract <- cumsum(dc$fract)

  #limit columns for output
  dc_out <- dc[,c(1,7)]
  dc_out[,1] <- dc_out[,1]*1E-6

  return(dc_out)
}
