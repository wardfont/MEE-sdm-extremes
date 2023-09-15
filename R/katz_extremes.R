katz_extremes <- function(x, ret_per, side) {

    ## There were two problems:
    ## One was that the default values in the function weren't very good for when the L-moments couldn't be calculated, scaling the data solved this problem
    ## The other was that for very negative shape, mostly the case for ppt in very dry locations where it is almost always zero or close to zero, the se cannot be calculated
    ##
    ## Now the function should only return all NAs if there is an NA in the input data, or if all input data is exaclty the same

    # Scaling needed because fevd functions default work better for scaled data, basically never give NA when scaled, but constantly when unscaled due to the defaults it uses. Estimates are almost identical between unscaled and scaled data for pixels that don't give errors
    x_scaled <- scale(x)
    scaling_center <- attr(x_scaled, "scaled:center")
    scaling_scale <- attr(x_scaled, "scaled:scale")
    x_s <- as.numeric(x_scaled)
    
    ## Necessary to make sure that the function outputs a vector with correct names, whatever result is first ouput of chuncks in app, might only be the case for when first cell in whole spatRaster
    empty_vector <- rep(NA, 9+(length(ret_per)))
    names(empty_vector) <- c("location", "scale", "shape",
                             "se_location", "se_scale", "se_shape",
                             ret_per,
                             "nllh", "aic", "bic")

    if(side == "min"){
            sign <- -1
        } else {
            sign <- 1
        }

    out <- vector()

    if(!any(is.na(x_s))){
        x_evd <- tryCatch(fevd(sign*x_s),
                          error = function(e){return(NULL)}) # Disable more warning, because that still gets triggered because of L-moments
        
        if(length(x_evd) == 18 & class(x_evd) == "fevd"){
            
            pars <- x_evd$results$par
            x_rl <- return.level(x_evd, ret_per)
            
            x_evd_sum <- tryCatch(summary(x_evd),
                                  error = function(e){return(NULL)},
                                  warning = function(e){return(NULL)})
            
            if(length(x_evd_sum) == 6 & class(x_evd_sum) == "list"){
                
                names(x_evd_sum$se.theta) <- c("se_location", "se_scale", "se_shape") # differentiate from parameter estimates
                se_vector <- x_evd_sum$se.theta
                nllh <- x_evd_sum$nllh
                aic <- x_evd_sum$AIC
                bic <- x_evd_sum$BIC
            } else if(length(x_evd_sum) == 4 & class(x_evd_sum) == "list"){
                se_vector <- c(se_location = NA, se_scale = NA, se_shape = NA)
                nllh <- x_evd_sum$nllh
                aic <- x_evd_sum$AIC
                bic <- x_evd_sum$BIC
            } else {
                se_vector <- c(se_location = NA, se_scale = NA, se_shape = NA)
                nllh <- NA
                aic <- NA
                bic <- NA
            }
            
            scale_back <- function(x){
                y <- x*scaling_scale + scaling_center
                return(y)
            }
                
            out <- c(scale_back(sign*pars[1]),
                     pars[2]*scaling_scale,
                     pars[3],
                     se_vector[1]*scaling_scale,
                     se_vector[2]*scaling_scale,
                     se_vector[3],
                     scale_back(sign*x_rl),
                     nllh = nllh,
                     aic = aic,
                     bic = bic)
        }
    }
    
    if(length(out) != length(empty_vector)){
        out <- empty_vector
    }
    
    return(out)
}
