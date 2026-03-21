# Functions

# CORE function
fncComputeMetrics <- function(stdata, species, lh = lh, year.range, st.col, site.col, date.col){
  data.out <- NULL
  for(sp in species){
  for(l in lh){
    lhist <- lifestages[lifestages$Species %in% sp & lifestages$LifeHistory %in% l,]
    life.stages <- lhist$Lifestage
    for(ls in life.stages){
      for(yy in year.range){
        
        st_md <- lhist$MD_begin[lhist$Lifestage == ls]
        st_y <- yy + lhist$Year_begin[lhist$Lifestage == ls]
        stdate <- as.Date(paste0(st_y, "-", st_md), format = "%Y-%d-%b")
        en_md <- lhist$MD_end[lhist$Lifestage == ls]
        en_y <- yy + lhist$Year_end[lhist$Lifestage == ls]
        endate <- as.Date(paste0(en_y, "-", en_md), format = "%Y-%d-%b")
        rm(st_md, st_y, en_md, en_y)
        
        thresh.hi <- lhist$Thresh_hi[lhist$Lifestage == ls]
        thresh.lo <- lhist$Thresh_lo[lhist$Lifestage == ls]
        
        # limit to the stream reaches for this species and life stage
        #if(length(grep("migr", ls)) > 0){
        #  use <- "Presence"
        #} else if(ls == "rearing"){
        #  use <- "Rearing"
        #} else if(length(grep("spawn", ls)) > 0 | ls == "incubat"){ 
        #  use <- "Spawning" #upriver migration, holding, spawning, and incubation
        #}
        #
        # sp2 <- switch(sp,
        #               "Chinook" = "Chinook Salmon",
        #               "steelhead" = "Steelhead Trout",
        #               "bull trout" = "Bull Trout")
        # #"sockeye" = "Sockeye Salmon",
        # #"rainbow" = "Rainbow Trout")
        # 
        # if(!is.null(sp2)){ #will be NA for 'generic' monthly
        #   cids <- swifd$COMID[swifd$SPECIES %in% sp2 & swifd$USETYPE_DE %in% use]
        #   # https://geo.wa.gov/datasets/wdfw::statewide-washington-integrated-fish-distribution/explore
        #   data2use <- stdata[stdata$COMID %in% cids,]
        # } else {
        #   data2use <- stdata
        # }
        
        data2use <- stdata
        
        # if reaches to process, proceed:
        if(nrow(data2use) > 0){

          # high temp frequency: proportion of days with temps > threshold
          met1 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_pDays.Unsuitable, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh.hi, sign = "GT")
          # high temp duration: longest consecutive stretch with temps > threshold
          met2 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_Cum.Days.Unsuitable, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh.hi, sign = "GT")
          # timing, first week too high
          met3 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_1stWk.Unsuitable, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh.hi, sign = "GT")
          # duration within suitable thermal range
          met4 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_Days.in.Range, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh.hi, YY = thresh.lo)
          # cumulative exposure, ie degree-days
          met5 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_Cum.Exposure, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate)
          # variability
          met6 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_Variance, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate)
          if(any(is.na(met6))){idx <- which(is.na(met6));for(i in idx){met6[[i]] <- rep(NA, 5)}}
          met6 <- t(abind::abind(met6, along = 2))
          colnames(met6) <- c("IWV", "AWV", "MWV", "VAR", "RNG")
          # weekly min/mean/max
          met7 <- lapply(X = unique(data2use[,site.col]), FUN = fnc_Weekly, frame = data2use, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate)
          if(any(is.na(met7))){idx <- which(is.na(met7));for(i in idx){met7[[i]] <- rep(NA, 6)}}
          met7 <- t(abind::abind(met7, along = 2))
          colnames(met7) <- c("IWI", "AWA", "MWA", "AWM", "MWM", "AWI")
          
          dat <- data.frame("SiteCode" = unique(data2use[,site.col]), "pExc" = unlist(met1), "durExc" = unlist(met2), "first.week" = unlist(met3), "daysSuitable" = unlist(met4),
                            "cum.exp" = unlist(met5), met6, met7,
                            "Species" = sp, "LifeHistory" = l, "Life.stage" = ls, "Start" = stdate, "End" = endate, "Thresh.hi" = thresh.hi, "Thresh.lo" = thresh.lo, "year" = yy)
          colnames(dat)[1] <- site.col
          dat <- dat[!is.na(dat$AWA),]
          data.out <- rbind(data.out, dat)
          rm(met1, met2, met3, met4, met5, met6, met7)
        } # end if data
      } # end year range
    } # end life stage
    } # end life history
  } # end species
  data.out$first.week <- as.Date(data.out$first.week)
  data.out <- data.out[!is.na(data.out$AWA),]
  
  return(data.out)
}

# Set up data frame
fnc_Setup <- function(frame, site, st.col, site.col, date.col, start.date, end.date, show.na = T){
  # sort and subset
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[,date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  
  # identify missing data
  datelist.all <- seq.Date(from = start.date, to = end.date, by = 1)
  datelist <- sort(unique(frame[, date.col]))
  na.datelist <- as.Date(setdiff(datelist.all, datelist))
  if(show.na){cat("There are", length(na.datelist),"missing days out of a total of", length(datelist.all), 
                  "(", round(length(na.datelist)/length(datelist.all)*100,2), "%)\n")}
  prop.missing <- length(na.datelist)/length(datelist.all)
  
  if(prop.missing < 0.2){ #proceed only if there are no more than 20% missing data
    return(frame)
  } else {
    return(frame[0,])
  }
}

# Weekly Metrics
fnc_Weekly <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    if(any(!is.na(frame[,st.col]))){
      stlist <- frame[,st.col]
      minlist <- meanlist <- maxlist <- rep(0, (length(datelist) - 6))
      for (i in 1:(length(datelist) - 6)){
        if(any(!is.na(stlist[i:(i + 6)]))){
          minlist[i] <- min(stlist[i:(i + 6)], na.rm = T)
          meanlist[i] <- mean(stlist[i:(i + 6)], na.rm = T)
          maxlist[i] <- max(stlist[i:(i + 6)], na.rm = T)
        } else {minlist[i] <- meanlist[i] <- maxlist[i] <- NA}
      }
      iwit <- min(minlist, na.rm = T) # minimum weekly minimum
      awat <- mean(meanlist, na.rm = T) # average weekly average
      mwat <- max(meanlist, na.rm = T) # maximum weekly average
      awmt <- mean(maxlist, na.rm = T) # average weekly maximum; this is 7DADM
      mwmt <- max(maxlist, na.rm = T) # maximum weekly maximum
      awit <- mean(minlist, na.rm = T) # average weekly minimum
      
      out <- c(iwit, awat, mwat, awmt, mwmt, awit)
      names(out) <- c("IWI", "AWA", "MWA", "AWM", "MWM", "AWI")
      return(out)	
    }
  } else{ return (NA)}
}

# Variance Metrics
fnc_Variance <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    if(any(!is.na(frame[,st.col]))){
      stlist <- frame[,st.col]
      varlist <- rep(0, (length(datelist) - 6))
      for (i in 1:(length(datelist) - 6)){
        if(any(!is.na(stlist[i:(i + 6)]))){
          varlist[i] <- var(stlist[i:(i + 6)], na.rm = T)
        } else {varlist[i] <- NA}
      }
      iwv <- min(varlist, na.rm = T) # minimum weekly variance
      if(is.na(iwv) | is.infinite(iwv)) iwv <- NA
      awv <- mean(varlist, na.rm = T) # average weekly variance
      if(is.na(awv) | is.infinite(awv)) awv <- NA
      mwv <- max(varlist, na.rm = T) # maximum weekly variance
      if(is.na(mwv) | is.infinite(mwv)) mwv <- NA
      var <- var(stlist, na.rm = T) # raw variance
      rng <- range(stlist, na.rm = T)[2] - range(stlist, na.rm = T)[1] #range of raw variance
      
      out <- c(iwv, awv, mwv, var, rng)
      names(out) <- c("IWV", "AWV", "MWV", "VAR", "RNG")
      return(out)	
    }
  } else{ return (NA)}
}

# No. of days with values above or below a threshold 
fnc_pDays.Unsuitable <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    period <- as.numeric(end.date - start.date) + 1    
    if(any(!is.na(frame[,st.col]))){
      if(sign == "GT"){
        dayslist <- frame[!is.na(frame[, st.col]) & frame[, st.col] >= XX,]
      } else if(sign == "LT"){
        dayslist <- frame[!is.na(frame[, st.col]) & frame[, st.col] <= XX,]
      }
      
      return(length(unique(dayslist[, date.col]))/period)
    }
  } else{ return (NA)}
}

# The first week to sustain values above or below a threshold
fnc_1stWk.Unsuitable <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    #for each day in datelist, count number of records above XX degrees
    daysabovelist <- rep(0, length(datelist))
    XXlist = rep(0, (length(datelist) - 6))
    for (i in 1:length(datelist)){
      if(sign == "GT"){
        daysabovelist[i] <- sum(frame[frame[, date.col] == datelist[i], st.col] > XX, na.rm = T) 
      } else if(sign == "LT") {
        daysabovelist[i] <- sum(frame[frame[, date.col] == datelist[i], st.col] < XX, na.rm = T) 
      }
    }
    for (i in 1:(length(datelist) - 6)){
      XXlist[i] <- sum(daysabovelist[i:(i + 6)])
    }
    
    if(any(XXlist > 0)){
      return(datelist[which.max(XXlist)])
    } else{ return (as.Date("1900-01-01"))}
    
  } else{ return (as.Date("1900-01-01"))}
}

# Date at which cumulative exposure surpasses or goes below a threshold
fnc_Date.Unsuitable <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    cumexp <- 0; i = 1
    if(sign == "GT"){
      while(cumexp < XX & i <= length(datelist)){
        cumexp <- sum(cumexp, frame[frame[, date.col] == datelist[i], st.col], na.rm = T) 
        i <- i + 1
      }
      if(cumexp > XX) return(datelist[i]) else return (as.Date("1900-01-01"))
    } else if(sign == "LT"){
      while(cumexp > XX & i <= length(datelist)){
        cumexp <- sum(cumexp, frame[frame[, date.col] == datelist[i], st.col], na.rm = T) 
        i <- i + 1
      }
      if(cumexp < XX) return(datelist[i]) else return (as.Date("1900-01-01"))
    }
    
  } else{ return (as.Date("1900-01-01"))}
}

# Cumulative exposure (for temperature, this is degrees-days)
fnc_Cum.Exposure <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    
    cumexp <- aggregate(frame[,st.col], list(frame[,site.col]), sum, na.rm = T)
    cumexp[,2] <- cumexp[,2]
    colnames(cumexp) <- c("Site", st.col)
    
    return(cumexp[, st.col])
  } else{ return (NA)}
}

# Cumulate days in unsuitable range
fnc_Cum.Days.Unsuitable <- function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    
    if(sign == "GT"){
      # Find indices where values exceed threshold
      exceed_indices <- which(frame[,st.col] > XX)
      
    } else if(sign == "LT"){
      # Find indices where values exceed threshold
      exceed_indices <- which(frame[,st.col] < XX)
    }
    
    # Calculate consecutive durations of exceedances
    consecutive_durations <- rle(exceed_indices)$lengths
    longest_duration <- sum(consecutive_durations)
    
    return(longest_duration)
  } else {return(0)}
}

#No. of days with temperatures within temperature range XX to YY during facet period
fnc_Days.in.Range = function(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = stdate, end.date = endate, XX = thresh, YY, sign = "GT"){
  
  frame <- fnc_Setup(frame = frame, site, st.col = st.col, site.col = site.col, date.col = date.col, start.date = start.date, end.date = end.date, show.na = F)
  
  if(nrow(frame) > 0){
    datelist <- sort(unique(frame[, date.col]))
    frame <- frame[!is.na(frame[,st.col]),]
    return(round(nrow(frame[frame[,st.col] >= YY & frame[,st.col] <= XX,]) ))
  } else {return(0)}
}

