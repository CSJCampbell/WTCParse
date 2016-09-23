
setwd("C:/Users/ccampbell/Documents/Misc/R/troop-creator/WTCParse")

library(RJSONIO) # fromJSON(content)
library(XLConnect)

source("R/findCaster.R")
source("R/findFaction.R")
source("R/findPlayer.R")
#source("R/warjackSearch.R")

listsize <- 75
datadir <- "../troop-creator/data"
emaildir <- "emails"
logdir <- "logs"

dir.create(logdir)
# missing line breaks
# Denmark Asgaard
# Sebastian Kaas Petersen
# Wales Dant

# extra line breaks 
# France Asterix
# Hungary, and removed the 2 columns
# Norway Red

# some army numbers removed from Greece Prime

emailXL <- loadWorkbook(file.path(emaildir, 
    "2016 WTC Lists - publicly shared copy.xlsx"))
workSheetNames <- getSheets(emailXL)

###########################################################


# imports
#animosities <- fromJSON(txt = file.path(datadir, "animosities.json"))
circle <- fromJSON(content = file.path(datadir, "orboros.json"))
convergence <- fromJSON(content = file.path(datadir, "convergence.json"))
cryx <- fromJSON(content = file.path(datadir, "cryx.json"))
cygnar <- fromJSON(content = file.path(datadir, "cygnar.json"))
legion <- fromJSON(content = file.path(datadir, "everblight.json"))
khador <- fromJSON(content = file.path(datadir, "khador.json"))
protectorate <- fromJSON(content = file.path(datadir, "menoth.json"))
mercenaries <- fromJSON(content = file.path(datadir, "mercenary.json"))
minions <- fromJSON(content = file.path(datadir, "minion.json"))
retribution <- fromJSON(content = file.path(datadir, "scyrah.json"))
skorne <- fromJSON(content = file.path(datadir, "skorne.json"))
trollbloods <- fromJSON(content = file.path(datadir, "trollblood.json"))

###########################################################

# collect data
wtcLists <- matrix(NA_character_, 
    #nrow = 1 * 400, 
    nrow = 64 * 400, 
    ncol = 12, 
    dimnames = list(NULL, 
        c("team", "player", "faction", "list", "objective", 
            "wrname", "tcname", "id", "cost", "bgtotal", 
            "total", "status")))

###########################################################

# keep track of teams/sheets
wInd <- 1

#emailname <- workSheetNames[1]
for (emailname in workSheetNames) {
    cat(emailname, "\n")
    # sort out logs
    logname <- paste0(emailname, ".log")
    
    bar <- paste0(c(rep("-", times = 60), "\n"), collapse = "")
    cat(paste0("\n", bar, "Log of ", emailname, "\n", bar), 
        file = file.path(logdir, logname))
    
    # read email
    email <- c(na.omit(readWorksheet(object = emailXL, 
        sheet = emailname, 
        header = FALSE, 
        colTypes = "character", simplify = TRUE)))
    
    if (is.data.frame(email)) { 
        cat(paste("\n  error: in emailname, data had structure", 
            dim(email)), 
            file = file.path(logdir, logname), append = TRUE)
    }
    
    ###########################################################
    
    # manually fix data issues
    if (emailname == "Australia Wallaby") {
        # objective listed twice for a list: before and after.
        # would be rather risky to attempt to determine which list the 
        # excess objective belonged to
        email[41] <- ""
    }
    if (emailname == "Australia Echidna") {
        # misc text matching player identifier
        email[147] <- ""
        # no War Room format :-(
        email[148] <- gsub(pattern = "-", replacement = "- WB: +", x = email[148])
        email[158] <- gsub(pattern = "-", replacement = "- WB: +", x = email[158])
    }
    if (emailname == "Australia Koala") {
        # no player identifier
        email[78] <- paste0(email[78], ":")
        email[113] <- paste0(email[113], ":")
        email[150] <- paste0(email[150], ":")
    }
    if (emailname == "Canada Goose") {
        # remove alternate player
        email[201:216] <- ""
    }
    if (emailname == "England Lions") {
        email[c(76, 86, 96, 107)] <- gsub(pattern = "\\+( )*", 
            replacement = "- WB: +", x = email[c(76, 86, 96, 107)])
        email[86] <- gsub(pattern = "Grimm", 
            replacement = "Grim", x = email[86])
    }
    if (emailname == "Ireland Ceol") {
        email[129] <- gsub(pattern = "\\+( )*", 
            replacement = "- WB: +", x = email[129])
    }
    if (emailname == "Italy Leonardo") {
        email[c(88, 98)] <- gsub(pattern = "\\-( )*", 
            replacement = "- WJ: +", x = email[c(88, 98)])
    }
    if (emailname == "Netherlands Vermeer") {
        email[38] <- gsub(pattern = "1 ", 
            replacement = "- WJ: +", x = email[38])
        email[27] <- gsub(pattern = " \\(Xerxis 1\\) \\+", 
            replacement = " - WJ: +", x = email[27])
        email[c(2, 13)] <- gsub(pattern = ": \\+", 
            replacement = " - WJ: +", x = email[c(2, 13)])
    }
    if (emailname == "Scotland Irn") {
        email[c(4, 21, 42, 58, 71, 88, 102, 114, 128, 141)] <- gsub(pattern = "\\+( )*", 
            replacement = "- WB: +", x = email[c(4, 21, 42, 58, 71, 88, 102, 114, 128, 141)])
    }
    # stop words
    # otherwise matches "Old Witch of Khador"
    email[email == "Khador"] <- ""
    
    ###########################################################
    
    nn <- length(email)
    
    cat(paste0("\nmessage: ", nn, " lines read"), 
        file = file.path(logdir, logname), append = TRUE)
    
    ###########################################################
    
    wtcListSummary <- matrix(NA_character_, 
        nrow = 10, 
        ncol = 9, 
        dimnames = list(NULL, 
            c("team", "player", "faction", "list", "objective", 
                "wrname",  "tcname", "id", "cost")))
    
    wtcListEmail <- matrix(NA_character_, 
        nrow = nn, 
        ncol = 12, 
        dimnames = list(NULL, 
            c("team", "player", "faction", "list", "objective", "wrname", 
                "tcname", "id", "cost", "bgtotal", "total", "status")))
    
    wtcListEmail[, "team"] <- emailname
    wtcListEmail[, "wrname"] <- email
    
    parsed <- character(nn)
    whichlist <- numeric(nn)
    
    ###########################################################
    
    wtcListSummary[, "team"] <- emailname
    
    ###########################################################
    
    # identify records corresponding to player name...
    playerInd <- findPlayer(txt = email, 
        label = emailname, 
        path = file.path(logdir, logname))
    
    players <- attr(x = playerInd, which = "data")
    
    if (emailname == "Canada Goose") {
        players <- email[c(2, 41, 85, 123, 167)]
    }
    if (emailname == "Canada Moose") {
        players <- email[c(2, 31, 62, 102, 133)]
    }
    if (emailname == "Czech Rep Red") {
        players <- email[c(1, 34, 66, 101, 138)]
    }
    if (emailname == "Denmark Asgaard") {
        players <- email[c(1, 31, 56, 83, 117)]
    }
    if (emailname == "Denmark Jotunheim") {
        players <- email[c(1, 44, 85, 124, 165)]
    }
    if (emailname == "England Roses") {
        players <- email[c(2, 34, 68, 97, 134)]
    }
    if (emailname == "England Knights") {
        players <- email[c(2, 38, 70, 99, 134)]
    }
    if (emailname == "Finland Ilmarinen") {
        players <- email[c(1, 24, 53, 76, 100)]
    }
    if (emailname == "France Asterix") {
        players <- email[c(2, 41, 73, 105, 150)]
    }
    if (emailname == "France Obelix") {
        players <- email[c(1, 31, 61, 85, 115)]
    }
    if (emailname == "Germany Gold") {
        players <- email[c(1, 39, 73, 108, 147)]
    }
    if (emailname == "Germany Red") {
        players <- email[c(1, 33, 71, 100, 124)]
    }
    if (emailname == "Germany Black") {
        players <- email[c(3, 32, 62, 90, 121)]
    }
    if (emailname == "Greece Epic") {
        players <- email[c(2, 30, 59, 88, 118)]
    }
    # if (emailname == "Greece Prime") {
    #     players <- email[c(1, )]
    # }
    
    
    
    if (length(players) == 5L) {
        players <- rep(players, each = 2)
    }
    wtcListSummary[, "player"] <- players
    
    
    ###########################################################
    
    # instead, parse by brute force
    
    factionInd <- findFaction(txt = email, n = 10)
    factionData <- attr(factionInd, which = "data")
    # handle multi-card casters
    ids <- vapply(
        X = factionData, 
        FUN = function(x) { x[["id"]] }, 
        FUN.VALUE = character(1))
    dup <- duplicated(ids) & c(FALSE, diff(factionInd)  < 5)
    if (any(dup)) {
        # write out parsed data
        getNameID <- function(x) { unlist(c(x[c("name", "id")], list(cost = 0L))) }
        wtcListEmail[factionInd[dup], c("tcname", "id", "cost")] <- t(vapply(
            X = factionData[dup], 
            FUN = getNameID, 
            FUN.VALUE = character(3)))
        # update factionInd
        factionInd <- factionInd[!dup]
        factionData <- factionData[!dup]
    }
    
    fdRes <- t(vapply(
        X = factionData, 
        FUN = function(x) { unlist(x[c("faction", "name", "id", "cost")]) }, 
        FUN.VALUE = character(4)))
    # report if correct results are not calculated
    if (all(dim(fdRes) != c(10, 4))) {
        warning("faction/caster issue found for ", emailname)
    }
    wtcListSummary[seq_len(nrow(fdRes)), c("faction", "tcname", "id", "cost")] <- fdRes
    wtcListSummary[, "list"] <- gsub(pattern = " \\(.+\\)$", 
        replacement = "", x = wtcListSummary[, "tcname"])
    wtcListSummary[, "list"] <- gsub(pattern = "^( )+", 
        replacement = "", x = wtcListSummary[, "list"])
    wtcListSummary[seq_len(nrow(fdRes)), "wrname"] <- email[factionInd]
    
    objs <- grep(pattern = "Objective (-|<e2><80><93>) ", 
        x = iconv(email, from = "", to = "ASCII", sub = "byte"))
    # consider adding $
    objectives <- c("Arcane Wonder", "Armory", "Bunker", 
        "(Effigy of ){0,1}Valo(u){0,1}r", "Fuel Cache", "Stockpile")
    if (length(objs) >= 10) {
        checkObjs <- sapply(X = objectives, FUN = grepl, x = email[objs])
    } else {
        checkObjs <- sapply(X = casefold(objectives, upper = FALSE), 
            FUN = grepl, 
            x = casefold(email, upper = FALSE))
        objs <- which(checkObjs)
    }
    if (length(objs) == 10 && sum(checkObjs) == length(objs)) {
        cat("\nmessage: objectives parsed correctly and ten found", 
            file = file.path(logdir, logname), append = TRUE)
    } else {
        cat("\n  error: ", length(objs), " found, and ", sum(checkObjs), 
            " recognised", 
            file = file.path(logdir, logname), append = TRUE)
    }
    # tidy up objective
    isObj <- gsub(
        pattern = "Objective( ){0,1}(-|<e2><80><93>)( ){0,1}" ,
        replacement = "",
        x = iconv(email[objs], from = "", to = "ASCII", sub = "byte"))
    if (length(isObj) != 10L) {
        warning("objective issue found for ", emailname)
    }
    isObj <- isObj[seq_len(min(length(isObj), 10))]
    wtcListSummary[seq_along(isObj), "objective"] <- isObj
    wtcListSummary[, "objective"] <- gsub(pattern = "^( )+", replacement = "", 
        x = wtcListSummary[, "objective"])
    wtcListSummary[wtcListSummary[, "objective"] == "Valour", "objective"] <- "Effigy of Valour"
    
    ###########################################################
    # update main output
    
    for (lst in seq_along(factionInd)) {
        # handle 10th list
        lsInd <- seq.int(from = factionInd[lst], 
            to = min(factionInd[lst + 1] - 1, length(email), na.rm = TRUE))
        wtcListEmail[lsInd, c("player", "faction", "list", "objective")] <- matrix(
            wtcListSummary[lst, 
                c("player", "faction", "list", "objective")], 
            nrow = length(lsInd), ncol = 4, byrow = TRUE)
        wtcListEmail[lsInd[1], c("tcname", "id", "cost")] <- matrix(
            wtcListSummary[lst, 
                c("tcname", "id", "cost")], 
            nrow = 1, ncol = 3, byrow = TRUE)
    }
    
    
    
    ###########################################################
    
    wtcLists[seq.int(from = wInd, 
        to = wInd + nrow(wtcListEmail) - 1L), 
        c("team", "player", "faction", "list", "objective", 
            "wrname", "tcname", "id", "cost"#, "bgtotal", 
            #"total", "status")
        )] <- wtcListEmail[,c("team", "player", "faction", 
            "list", "objective", 
            "wrname", "tcname", "id", "cost")]
    
    wInd <- wInd + nrow(wtcListEmail) + 1L
}

# emailname
# 
# sapply(X = factionData, FUN = function(x) x$name)

wtcLists <- wtcLists[
    !apply(X = wtcLists, 
        MARGIN = 1, 
        FUN = function(x) all(is.na(x))), ]

wtcLists1 <- wtcLists[!is.na(wtcLists[, "id"]) & 
        wtcLists[, "cost"] > 0, 1:9]
write.table(wtcLists1, file = "wtcLists2016.txt", 
    quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")
