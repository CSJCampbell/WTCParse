
# @title Find faction by caster match
# @description Split vector into blocks 
# @param txt character vector
# @param ind numeric vector
# @return numeric vector
# @examples
# findFaction(txt = "Dominar Rasheth") # [1] 1 
# vals <- c("warbeast", "warjack", "solo")
# v1 <- c("Lylyth Herald of Everblight", vals, 
#     "Kommander Oleg Strakhov", vals)
# findFaction(txt = v1) # [1] 1 5
# v2 <- c(v1, "Lord General Coleman Stryker", vals)
# findFaction(txt = v2, ind = 9) # [1] 9
# findFaction(txt = v2, ind = 9, n = 10L) # [1] 1 5 9

findFaction <- function(txt, ind, n = 1L) {
    if (missing(ind)) { 
        ind <- NULL
    } else {
        ind <- sort(ind)
    }
    res <- numeric(0)
    resLs <- list()
    if (length(ind) < length(txt) && 
            length(txt) != 1L &&
            length(ind) < n) {
        if (length(ind) < n) {
            nextInd <- length(ind) + 1L
            inds <- unique(sort(c(1, ind, length(txt))))
            gaps <- diff(inds)
            # start with largest gap
            for (ig in seq_along(gaps)) {
                gapIG <- sort(gaps, decreasing = TRUE)[ig]
                if (length(gapIG) > 1L) {
                    gaps[gaps == gapIG] <- gaps[gaps == gapIG] - 
                        (seq_along(gapIG) - 1)/100
                }
                gapInds <- inds[which(gaps == gapIG) + 0:1]
                resig <- rep(NA_real_, times = gapInds[2] - gapInds[1])
                # feature to not include last record in caster search
                gapSeq <- seq.int(from = gapInds[1], 
                    to = min(gapInds[2] - 1, length(txt), na.rm = TRUE))
                for (iel in seq_along(gapSeq)) {
                    fc <- findCaster(txt = txt[gapSeq[iel]])
                    if (!is.na(fc$name)) {
                        fc$ind <- gapSeq[iel]
                        resig[iel] <- gapSeq[iel]
                        resLs[[nextInd]] <- fc
                        nextInd <- nextInd + 1
                    }
                }
                if (sum(!is.na(resig)) > 1L) {
                    
                    ind <- sort(unique(c(ind, 
                        which(!is.na(resig)) + gapInds[1L] - 1L)))
                    tot <- length(ind)
                    resig <- resig[!is.na(resig)]
                    res <- c(res, ind)
                    if (tot >= n) { break }
                }
            }
        }
    } else {
        if (length(txt) == 1L & length(ind) == 0L) {
            fc <- findCaster(txt = txt)
            if (is.na(fc$name)) { 
                res <- 0 
            } else {
                resLs <- fc
                res <- fc$ind
            }
        } else { 
            if (length(ind) >= n) { res <- ind }
        }
    }
    attr(res, which = "data") <- resLs
    return(res)
}
