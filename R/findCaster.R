

# @title Search Text for match with casters
# @description Search for each element of text for matches
# of faction$groups$entries[[el]]$name where el is the element with 
# id faction_x_warcaster/lock.
# @param txt character vector
# @param faction single character naming data object to interrogate
# @return list with element id, name, cost, faction
# @examples
# findCaster(txt = "Dominar Rasheth", faction = "skorne")
# findCaster(txt = "Dominar Rasheth")
# findCaster(txt = "Dominar Rasheth", faction = "cygnar")
# findCaster(txt = "Nemo", faction = "cygnar")
# findCaster(txt = "lord exhumator scaverous")
# findCaster(txt = "---")

findCaster <- function(txt, faction) {
    if (is.null(txt) || length(txt) < 1L) { stop("txt was length zero") }
    if (!is.character(txt)) { stop("txt must be character") }
    out <- list(id = NA_character_, name = NA_character_, 
        cost = NA_real_, faction = NA_character_)
    factions <- c("circle", "convergence", "cryx", "cygnar", "legion", "khador", 
        "protectorate", "mercenaries", "minions", "retribution", "skorne", 
        "trollbloods")
    if (!missing(faction) && any(faction %in% factions)) {
        if (length(faction) > 1) { 
            warning("only 1 guess permitted")
            faction <- faction[1]
        }
        res <- casterSearch(txt = txt, faction = faction)
        if (res$status) {
            out <- res[c("id", "name", "cost", "ind", "faction")]
        } else {
            # this faction is not the one!
            factions <- factions[factions != faction]
        }
    }
    if (is.na(out$name)) {
        for (ifac in seq_along(factions)) {
            res <- casterSearch(txt = txt, faction = factions[ifac])
            if (res$status) {
                out <- res[c("id", "name", "cost", "ind", "faction")]
                break
            }
        }
    }
    return(out)
}

# @title Get Faction, then Search Text for match with casters
# @description Search for each element of text for matches
# of faction$groups$entries[[2]]$name
# @param txt character vector
# @param faction single character naming data object to interrogate
# @return list with element status
# @examples
# casterSearch(txt = "Dominar Rasheth", faction = "skorne")
# casterSearch(txt = "Dominar Rasheth", faction = skorne)
# casterSearch(txt = "Dominar Rasheth", faction = "cygnar")
# casterSearch(txt = "Nemo", faction = "cygnar")
# casterSearch(txt = c("My List:", "The Terrible Tree 75 pts", "Wurmwood"), faction = "circle")
# casterSearch(txt = "Wurmwood, Tree of Fate & Cassius the Oathkeeper - WB: +27", faction = "circle")
# casterSearch(txt = "---", faction = "circle")
# casterSearch(txt = "Dr. Arkadius - WB: +32", faction = "minions")
# casterSearch(txt = "(Maddox 1) Major Beth Maddox [+30]", faction = "cygnar")
# casterSearch(txt = "Vyros2 (Vyros Incissar of the Dawnguard) (-27 Pts.)", faction = "retribution")
# casterSearch(txt = "Orsus Zoktavir, The Butcher of Khardov - WJ: +28", faction = "khador")
# casterSearch(txt = " Absylonia, Daughter of Everblight - WB: +28 ", faction = "legion")
# casterSearch(txt = "Major Victoria Haley - WJ: +25 - Fuel Cache", faction = "cygnar")
# casterSearch(txt = "Hunters Grimm", faction = "trollbloods")
# casterSearch(txt = "High Allegiant Amon Ad-Raza WJ:+29", faction = "protectorate")
# casterSearch(txt = "Hierarch Severius WJ:+26pts", faction = "protectorate")
# casterSearch(txt = "Haley2 (Major Victoria Haley)(-25 Pts.)", faction = "cygnar")
# casterSearch(txt = "Issyria, Sibyl of Dawn: +29", faction = "retribution")
# casterSearch(txt = "Lord Arcanist Ossyan - WJ: +28 (Fuel Cache)", faction = "retribution")

casterSearch <- function(txt, faction) {
    if (missing(faction)) { stop("faction is missing") }
    gfaction <- getFaction(faction = faction)
    faction <- gfaction$faction
    fname <- gfaction$fname
    out <- list(faction = fname, status = FALSE)
    # find element with warcasters/warlocks
    el <- vapply(X = faction$groups, 
        FUN = function(x) any(
            grepl(
                pattern = ifelse(
                    test = is.hordes(fname), 
                    yes = "warlocks$", 
                    no = "warcasters$"), 
                x = x$id)), 
        FUN.VALUE = logical(1))
    casters <- faction$groups[[which(el)]]$entries
    
    for (ic in seq_along(txt)) {
        # parse expecting war room, but with a little flexibility
        txti <- txt[ic]
        txti <- gsub(pattern = "( )*$", 
            replacement = "", 
            x = txti)
        nci <- nchar(txti)
        # em dash needs handling as byte code
        gdash <- "( )*(-|<e2><80><93>){0,1}( )*"
        txti <- gsub(pattern = paste0(
                "(", gdash, "W[BJ]:( )*\\+[0-9]+(pts){0,1}((", gdash, 
                "| \\(){0,1}(Arcane Wonder|Armory|Bunker|(Effigy of ){0,1}Valo(u){0,1}r|Fuel Cache|Stockpile)(\\)){0,1}){0,1}$",
                "| \\[\\+[1-4][0-9]\\]$|( )*\\(-[1-4][0-9] Pts.\\))"), 
            replacement = "", 
            x = iconv(txti, from = "", to = "ASCII", sub = "byte"))
        isWRCaster <- nchar(txti) < nci
        txti <- gsub(pattern = "^\\(.+\\) " , 
            replacement = "", 
            x = txti)
        txti <- casefold(gsub(pattern = "[[:punct:]]", replacement = "", 
                x = txti), upper = FALSE)
        txti <- casefold(gsub(pattern = "the ", replacement = "(the ){0,1}", 
                x = txti), upper = FALSE)
        # don't bother searching for short strings
        if (nchar(txti) > 3L) {
            # get names from faction, remove punctuation
            casterName <- casefold(vapply(X = casters, 
                FUN = function(x) x$name, 
                FUN.VALUE = character(1)), upper = FALSE)
            casterName <- gsub(pattern = "[[:punct:]]", replacement = "", 
                x = casterName)
            isCaster <- grepl(pattern = txti, 
                x = casterName)
            if (!all(is.na(isCaster)) && any(isCaster)) {
                isCaster2 <- grepl(pattern = paste0("\\(", txti), 
                    x = casterName)
                if (sum(isCaster2) == 1L) {
                    isCaster <- isCaster2
                } else {
                    if (sum(isCaster) > 1L) {
                        warning(paste("only first of", sum(isCaster), "matches to", 
                                txti, "will be used"))
                    }
                }
                out <- c(casters[isCaster][[1]][c("id", "name", "cost")], 
                    list(faction = fname, ind = ic, status = TRUE))
                break 
            }
        }
    }
    if (isWRCaster & !out$status) { 
        out <- list(id = NA, name = txti, cost = NA, faction = fname, ind = ic, 
            status = FALSE)
    }
    return(out)
}

# get faction data object, sourced from troop-creator
getFaction <- function(faction) {
    if (is.character(faction)) {
        if (length(faction) != 1L) { stop("faction must be length 1") }
        fname <- faction
        faction <- get(x = faction)
    } else {
       # allow unquoted faction object names
       fname <- as.character(match.call()[["faction"]])
    }
    if (!"groups" %in% names(faction)) {
        stop("invalid faction object")
    }
    list(fname = fname, faction = faction)
}

# test if character vector is string naming Hordes faction
# @return logical vector
# @examples
# is.hordes("Minions")
# is.hordes("khador")
# is.hordes(c("minions", "khador"))

is.hordes <- function(x) {
    x <- casefold(x, upper = FALSE)
    if (!all(x %in% c("circle", "convergence", "cryx", "cygnar", "legion", "khador", 
        "protectorate", "mercenaries", "minions", "retribution", "skorne", 
        "trollbloods"))) { stop("unrecognised faction") }
    x %in% c("circle", "legion", "minions", "skorne", "trollbloods")
}

