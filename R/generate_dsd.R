#' This function will generate a droplet size distribution
#'
#' @param VMD dummy
#' @param rel_span dummy
#' @param DV0.1 dummy
#' @param DV0.9 dummy
#' @param Dsize dummy
#' @param fract_Dsize dummy
#' @param min_size dummy
#' @param min_width dummy
#' @param max_width dummy
#'
#' @return dummy
#' @export
#'
#' @importFrom stats qnorm
#' @importFrom minpack.lm nlsLM
#' @importFrom stats predict
#' @importFrom stats lm

generate_dsd <- function (VMD, rel_span, DV0.1, DV0.9, Dsize, fract_Dsize, min_size, min_width = 5, max_width = 50){
  sd_low <- sd_up <- NA
  df_vmd <- data.frame(cum_fract = 0.5, size = VMD)
  ds_p <- df_vmd
  if (!missing(Dsize) & !missing(fract_Dsize)) {
    df_D <- data.frame(cum_fract = fract_Dsize, size = Dsize)
    ds_p <- rbind(ds_p, df_D)
  }
  if (!missing(DV0.1)) {
    df_DV0.1 <- data.frame(cum_fract = 0.1, size = DV0.1)
    ds_p <- rbind(ds_p, df_DV0.1)
  }
  if (!missing(DV0.9)) {
    df_DV0.9 <- data.frame(cum_fract = 0.9, size = DV0.9)
    ds_p <- rbind(ds_p, df_DV0.9)
  }
  if (missing(min_size)) {
    min_size <- 10
  }
  if (!missing(rel_span) & missing(DV0.1) & missing(DV0.9) &
      missing(fract_Dsize) & missing(Dsize)) {
    DV01 <- VMD - (rel_span * VMD)/2
    DV09 <- VMD + (rel_span * VMD)/2
    df_1 <- data.frame(cum_fract = c(0.1, 0.9), size = c(DV01,
                                                         DV09))
    ds_p <- rbind(ds_p, df_1)
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV01 - VMD)/z_low
  }
  else if (!missing(rel_span) & !missing(DV0.1) & missing(DV0.9) &
           missing(fract_Dsize) & missing(Dsize)) {
    DV09 <- DV0.1 + (rel_span * VMD)
    df_2 <- data.frame(cum_fract = c(0.9), size = c(DV09))
    ds_p <- rbind(ds_p, df_2)
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1 - VMD)/z_low
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV09 - VMD)/z_up
  }
  else if (!missing(rel_span) & missing(DV0.1) & !missing(DV0.9) &
           missing(fract_Dsize) & missing(Dsize)) {
    DV01 <- DV0.9 - (rel_span * VMD)
    df_3 <- data.frame(cum_fract = c(0.1), size = c(DV01))
    ds_p <- rbind(ds_p, df_3)
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV01 - VMD)/z_low
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9 - VMD)/z_up
  }
  else if (!missing(rel_span) & missing(DV0.1) & missing(DV0.9) &
           !missing(fract_Dsize) & !missing(Dsize)) {
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize - VMD)/zDsize
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low * sd_low
    DV09 <- DV01 + (rel_span * VMD)
    df_4 <- data.frame(cum_fract = c(0.1, 0.9), size = c(DV01,
                                                         DV09))
    ds_p <- rbind(ds_p, df_4)
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV09 - VMD)/z_up
  }
  else if (missing(rel_span) & !missing(DV0.1) & !missing(DV0.9) &
           missing(fract_Dsize) & missing(Dsize)) {
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1 - VMD)/z_low
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9 - VMD)/z_up
  }
  else if (missing(rel_span) & !missing(DV0.1) & !missing(DV0.9) &
           !missing(fract_Dsize) & !missing(Dsize)) {
    z_low <- stats::qnorm(0.1)
    sd_low <- (DV0.1 - VMD)/z_low
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9 - VMD)/z_up
  }
  else if (missing(rel_span) & missing(DV0.1) & !missing(DV0.9) &
           !missing(fract_Dsize) & !missing(Dsize)) {
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize - VMD)/zDsize
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low * sd_low
    df_7 <- data.frame(cum_fract = c(0.1), size = c(DV01))
    ds_p <- rbind(ds_p, df_7)
    z_up <- stats::qnorm(0.9)
    sd_up <- (DV0.9 - VMD)/z_up
  }
  else if (missing(rel_span) & !missing(DV0.1) & missing(DV0.9) &
           !missing(fract_Dsize) & !missing(Dsize)) {
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low1 <- (Dsize - VMD)/zDsize
    z_low <- stats::qnorm(0.1)
    sd_low2 <- (DV0.1 - VMD)/z_low
    sd_low <- (sd_low1 + sd_low2)/2
    sd_up <- sd_low
    z_up <- stats::qnorm(0.9)
    DV09 <- VMD + z_up * sd_up
    df_8 <- data.frame(cum_fract = c(0.9), size = c(DV09))
    ds_p <- rbind(ds_p, df_8)
  }
  else if (missing(rel_span) & missing(DV0.1) & missing(DV0.9) &
           !missing(fract_Dsize) & !missing(Dsize)) {
    zDsize <- stats::qnorm(fract_Dsize)
    sd_low <- (Dsize - VMD)/zDsize
    sd_up <- sd_low
    z_low <- stats::qnorm(0.1)
    DV01 <- VMD + z_low * sd_low
    z_up <- stats::qnorm(0.9)
    DV09 <- VMD + z_up * sd_up
    df_9 <- data.frame(cum_fract = c(0.1, 0.9), size = c(DV01,
                                                         DV09))
    ds_p <- rbind(ds_p, df_9)
  }
  if (is.na(sd_up)) {
    sd_up <- sd_low
  }
  if (is.na(sd_low)) {
    sd_low <- sd_up
  }
  zmax <- stats::qnorm(0.9999)
  DVmax <- VMD + zmax * sd_up
  df_max_size <- data.frame(cum_fract = 1, size = DVmax)
  ds_p <- rbind(ds_p, df_max_size)
  fract_min <- 0.001
  zmin <- stats::qnorm(fract_min)
  DVmin <- VMD + zmin * sd_low
  if (DVmin < min_size) {
    DVmin <- min_size
  }
  df_min_size <- data.frame(cum_fract = fract_min, size = DVmin)
  ds_p <- rbind(ds_p, df_min_size)
  ds_p$cum_fract_log <- log10(ds_p$cum_fract)
  ds_p$size <- ds_p$size - DVmin
  ds_p$cum_fract_log <- ds_p$cum_fract_log + abs(log10(fract_min))
  model <- minpack.lm::nlsLM(cum_fract_log ~ abs(log10(fract_min)) *
                               (1 - exp(-(a * size)^b)), data = ds_p, start = list(a = 0.03,
                                                                                   b = 0.8))
  pred <- data.frame(size = seq(DVmin, DVmax, by = 0.5), exp = NA,
                     cum_fract = NA)
  pred$size <- pred$size - DVmin
  pred$exp <- stats::predict(model, data.frame(size = pred$size))
  pred$size <- pred$size + DVmin
  pred$exp <- pred$exp - abs(log10(fract_min))
  pred$cum_fract <- 10^(pred$exp)
  df_lm <- data.frame(size = c(DVmin, DVmax), cum_fract = c(0,
                                                            (1 - max(pred$cum_fract))))
  model <- stats::lm(cum_fract ~ size, data = df_lm)
  pred_lm <- data.frame(size = seq(DVmin, DVmax, by = 0.5),
                        cum_fract = NA)
  pred_lm$cum_fract <- stats::predict(model, data.frame(size = pred_lm$size))
  pred$cum_fract <- pred$cum_fract + pred_lm$cum_fract
  pred <- pred[, c(1, 3)]
  df_lm <- data.frame(size = c(50, 1000), width = c(min_width, max_width))
  model <- stats::lm(width ~ size, data = df_lm)
  pred_lm <- data.frame(size = seq(10, 600, by = 5), width = NA)
  pred_lm$width <- stats::predict(model, data.frame(size = pred_lm$size))
  dc <- data.frame(mean_size = NA, min_size = NA, max_size = NA,
                   lower_fract = NA, upper_fract = NA, fract = NA, cum_fract = c(1:100))
  dc$cum_fract <- NA
  dc$min_size[1] <- DVmin
  width_0 <- round(unname(stats::predict(model, data.frame(size = dc$min_size[1])))/min_width,
                   0) * min_width
  if (width_0 < min_width) {
    width_0 <- min_width
  }
  dc$max_size[1] <- dc$min_size[1] + width_0
  for (i in 2:nrow(dc)) {
    dc$min_size[i] <- dc$max_size[i - 1]
    width_0 <- round(unname(stats::predict(model, data.frame(size = dc$min_size[i])))/min_width,
                     0) * min_width
    if (width_0 < min_width) {
      width_0 <- min_width
    }
    dc$max_size[i] <- dc$min_size[i] + width_0
    if (dc$max_size[i] >= DVmax) {
      break
    }
  }
  dc <- dc[!is.na(dc$min_size), ]
  dc$mean_size <- (dc$min_size + dc$max_size)/2
  for (i in 1:nrow(dc)) {
    if (i == 1) {
      dc$lower_fract[i] <- 0
    }
    else {
      dc$lower_fract[i] <- pred$cum_fract[which(pred$size ==
                                                  dc$min_size[i])]
    }
    if (i == nrow(dc)) {
      dc$upper_fract[i] <- 1
    }
    else {
      dc$upper_fract[i] <- pred$cum_fract[which(pred$size ==
                                                  dc$max_size[i])]
    }
    dc$fract[i] <- dc$upper_fract[i] - dc$lower_fract[i]
  }
  dc$cum_fract <- cumsum(dc$fract)
  dc_out <- dc[, c(1, 7)]
  dc_out[, 1] <- dc_out[, 1] * 1e-06
  return(dc_out)
}
