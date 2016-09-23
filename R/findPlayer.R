
# @title Find Player by Pattern
# @description Find player name index 
# @param txt character vector
# @param label character vector
# @return length 5 numeric vector
# with attribute data containing found player names
# @examples
# findPlayer(txt = "Dominar Rasheth") # [1] 0 0 0 0 0
# vals <- c("warbeast", "warjack", "solo")
# v1 <- c("Fred:", "Lylyth Herald of Everblight", vals,
#     "Kommander Oleg Strakhov", vals)
# findPlayer(txt = v1) # [1] 1 0 0 0 0
# v2 <- c(v1, "Georgina - Cygnar", "Lord General Coleman Stryker", vals)
# findPlayer(txt = v2) # [1] 1 10 0 0 0
# findPlayer(txt = c("1st player - George Banavos (Captain)",
#     "2nd player - Nikos Kerazoglou",
#     "2) Skorne Army - 75 / 75 points",
#     "3rd player - Orestis Argiropoulos",
#     "4th player - Dimitris Kasdiovasilis",
#     "1) 5th player- Panagiotis Demiris"))
# findPlayer(txt = c("Daniel Porter (Captain) - Cygnar",
#         "Michael Porter - Legion of Everblight",
#         "Patrick Long - Trollbloods.",
#         "Rob Read - Protectorate of Menoth",
#         "Ciaran Bolger - Skorne"))

findPlayer <- function(txt, label = "txt", path = "") {
    
    txt0 <- txt
    txt <- iconv(txt, from = "", to = "ASCII", sub = "byte")
    factionPat <- c(
        circle = "([Cc]ircle|[Oo]rboros|[Cc]ircle (of ){0,1}[Oo]rboros)", 
        convergence = "([Cc]onvergence|Cc]yriss|[Cc]onvergence of [Cc]yriss)", 
        cryx = "[Cc]ryx", 
        cygnar = "[Cc]ygnar", 
        legion = "([Ll]egion|[Ee]verblight|[Ll]egion of [Ee]verblight)", 
        khador = "[Kk]hador", 
        protectorate = "([Pp]rotectorate|[Mm]enoth|[Pp]rotectorate of [Mm]enoth)", 
        mercenaries = "[Mm]ercenar(y|ies)", 
        minions = "[Mm]inion(s){0,1}", 
        retribution = "([Rr]etribution|[Ss]cyrah|[Rr]etribution of [Ss]cyrah)", 
        skorne = "[Ss]korne", 
        trollbloods = "[Tt]rollblood(s){0,1}")

    p1 <- "[Pp][Ll][Aa][Yy][Ee][Rr](:|( ){0,1}(-|<e2><80><93>) ){0,1}"
    p2 <- paste0(" ((-|<e2><80><93>) |\\()(", paste(factionPat, collapse = "|"), 
        ")(\\))( \\([Cc]aptain\\)){0,1}")
    p3 <- paste0("^(", paste(factionPat, collapse = "|"), 
        ") - WTC - FINAL - ")
    p4 <- "^0[1-5] "
    p5 <- "^[1-5](\\)|\\.) "
    p6 <- "^#[1-5].+:( )*$"
    # do not remove this pattern!
    p7 <- "^.+:( )*$"
    
    playerInd <- grep(pattern = p1, x = txt)
    
    if (!length(playerInd) %in% c(5, 10)) {
        playerInd2 <- grep(pattern = p2,
            x = txt)
        if (!length(playerInd2) %in% c(5, 10)) {
            playerInd3 <- grep(pattern = p3, 
                x = txt)
            if (!length(playerInd3) %in% c(5, 10)) {
                # just Austria 2
                playerInd4 <- grep(pattern = p4, 
                    x = txt)
                if (!length(playerInd4) %in% c(5, 10)) {
                    playerInd5 <- grep(pattern = p5, 
                        x = txt)
                    if (!length(playerInd5) %in% c(5, 10)) {
                        playerInd6 <- grep(pattern = p6, 
                            x = txt)
                        if (!length(playerInd6) %in% c(5, 10)) {
                            playerInd7 <- grep(pattern = p7, 
                                x = txt)
                            if (length(playerInd7) == 5) {
                                playerInd <- playerInd7
                            }
                        } else {
                            playerInd <- playerInd6
                        }
                    } else {
                        playerInd <- playerInd5
                    }
                } else {
                    playerInd <- playerInd4
                }
            } else {
                playerInd <- playerInd3
            }
        } else {
            playerInd <- playerInd2
        }
    }
    if (!length(playerInd) %in% c(5, 10)) {
        pInd <- unique(sort(c(playerInd, playerInd2, 
            playerInd3, playerInd4, playerInd5, 
            playerInd6, playerInd7)))
        if (any(path == "")) {
            warning(length(pInd), " players found in ", label)
        } else {
            cat(paste0("\nwarning: found ", length(playerInd), 
                " players in ", label), 
                file = path, append = TRUE)
        }
        pInd <- pInd[seq_len(min(length(pInd), 5))]
        playerInd <- c(pInd, numeric(5 - length(pInd)))
    }
    # account for when no players where found
    players <- character(5)
    players[playerInd != 0] <- txt0[playerInd]
    # tidy up player names
    if (any(players != "")) {
        players <- gsub(pattern = p1, 
                replacement = "", x = players)
        players <- gsub(pattern = p2, 
                replacement = "", x = players)
        players <- gsub(pattern = p3, 
                replacement = "", x = players)
        players <- gsub(pattern = p4, 
            replacement = "", x = players)
        players <- gsub(pattern = p5, 
            replacement = "", x = players)
        players <- gsub(pattern = p6, 
            replacement = "", x = players)
        players <- gsub(pattern = " - [A-Z][a-z]+[1-3]$", 
            replacement = "", x = players)
        players <- gsub(pattern = "(1st|2nd|3rd|4th|5th) ", 
            replacement = "", x = players)
        players <- gsub(pattern = ":$", 
            replacement = "", x = players)
        players <- gsub(pattern = "^( )+", 
            replacement = "", x = players)
    }
    res <- playerInd
    attr(x = res, which = "data") <- players
    return(res)
}
