
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
get_numbers <- function(x) {as.numeric(regmatches(x, regexpr("[[:digit:]]+", x)))}

msg <- function(...) cat(paste(..., "\n"))
startsWithAny <- function(x, prefix) {
  sapply(prefix, function(pf) startsWith(x, pf)) %>%
    apply(1, any) %>%
    as.logical()
}

#' Reads stockoverview files
#'
#' @param fn filename
#' @param v if TRUE, show messages
#'
#' @return
#' @export

read_StockOverview <- function(fn, v = FALSE) {
  ##if(v) msg("Reading file:", fn)
  so <- read.delim(fn, stringsAsFactors = FALSE) %>% as_tibble()
  if(v) {
    catch_by_catch_category <- so %>% group_by(Catch.Cat.) %>%
      summarise("Total catch (tonnes)" = sum(Catch..kg) / 1000)
    move_to_discards <- catch_by_catch_category %>%
      filter(startsWithAny(Catch.Cat., c("BMS", "Logbook"))) %>%
      pull(`Total catch (tonnes)`) %>%
      sum
    msg("Catch categories:")
    msg(catch_by_catch_category)
    msg("BMS landing and Logbook registered discard are removed, in total: ", move_to_discards," kg")
  }
  so <- so %>%
    filter(! startsWithAny(Catch.Cat., c("BMS", "Logbook")))
  so
}

#' Make gear groupings and get the percent catch
#'
#' @param so Output from read_StockOverview
#' @param groups
#'
#' @return
#' @export

group_gears_add_percent_discard <- function(so, groups) {
  so %>%
    ## Remove raised discards (e.g. from Intercatch)
    filter(Discards.Imported.Or.Raised != "Raised Discard") %>%
    ## Take first 3 letters from Fleet to get the gear category
    mutate(gearcat = substr(Fleets, 1, 3)) %>%
    ## Group into gear categories
    mutate(group = case_when(!!! groups)) %>%
    ## Add metier column
    mutate(metier = paste(Area, Fleets, Season, Country)) %>%
    ## Metiers that appear more than once have registered discards
    ## Calculate discard percentage for each if those metiers
    group_by(metier) %>%
    mutate(count = n()) %>%
    ## Add D/L ratio in landings ((L+D)/L - 1 = D/L) and L/D in discards ((L+D)/D - 1 = L/D)
    mutate(perc = ifelse(count == 2, sum(Catch..kg, na.rm = TRUE) / Catch..kg - 1,  NA)) %>% ## & Catch.Cat. == "Landings"
    ungroup() %>%
    ## Remove Discard only metiers and zero landing/discards
    filter(! (count == 1 & Catch.Cat. == "Landings" & Catch..kg == 0)) %>%
    filter(! (count == 1 & Catch.Cat. == "Discards")) %>%
    filter(! is.nan(perc))
}


get_raised_discards <- function(so, nodiscardgroups = c("MIS"), allgroups = NULL, threshold = 1e10) {
  res <- so %>%
    filter(Catch.Cat. == "Landings", ! group %in% nodiscardgroups) %>%
    mutate(weight = ifelse(count == 2, round(Catch..kg), NA)) %>%
    group_by(group) %>%
    ## Add raising factor to landing metiers that do not have discards (i.e. count == 1)
    mutate(percraise = ifelse(count == 1,
                              ## Weighted mean of raising factors of the landings with corresponding discards
                              ## Weights equal to the landing weight
                              weighted.mean(ifelse(count == 2 & perc < threshold, perc, NA), ifelse(count == 2 & perc < threshold, weight, NA), na.rm = TRUE),
                              NA)) %>%
    mutate(percraise_includes = ifelse(count == 1,
                                       paste0(perc, collapse = ", "),
                                       NA),
           percraise_weights = ifelse(count == 1,
                                      paste0(weight, collapse = ", "),
                                      NA)) %>%
    ungroup()
  if (! is.null(allgroups)) {
    overall_mean <- with(res, weighted.mean(ifelse(count == 2, perc, NA),ifelse(count == 2, weight, NA), na.rm = TRUE))
    res[res$group %in% allgroups, ]$percraise <- overall_mean
  }
  res %>%
    filter(is.finite(percraise)) %>%
    mutate(Catch.Cat. = "Discards", Discards.Imported.Or.Raised = "Raised Discard", Catch..kg = Catch..kg * percraise)
}

#' Get the discard ratio for each category
#'
#' @param so
#'
#' @return
#' @export

get_discard_ratio_per_category <- function(so) {
  ungroup(so) %>%
    filter(is.finite(percraise)) %>%
    mutate(percraise_round = round(percraise, 7)) %>%
    group_by(group) %>%
    summarise(percdisc = unique(percraise_round)) %>%
    mutate(propdisc = (1 / percdisc + 1)^(- 1))
}

#' Read stock overview files and raise dicards according to a grouping scheme
#'
#' @param fn StockOverview.txt filenames
#' @param groups Grouppings of gears/areas/countries to do the raising
#' @param nodiscardgroups Grouips assumed to have no discards
#' @param allgroups Groups that use the overall mean discard ratio for raising discards
#' @param threshold Remove metiers with discard rates above this threshold from the group means
#' @param v Should I be verbose?
#'
#' @return
#' @export
raise_discards <- function(fn, groups, nodiscardgroups = c("NO", "MIS"), allgroups = NULL, threshold = 1e10, v = FALSE) {
  so <- read_StockOverview(fn, v = v) %>%
    group_gears_add_percent_discard(groups = groups)
  rd <- get_raised_discards(so, nodiscardgroups = nodiscardgroups, allgroups = allgroups, threshold = threshold)
  catch <- bind_rows(so, rd)
}

# raise_discards <- function(fn,
#                            categories = rlang::exprs(
#                              gearcat %in% "MIS" ~ "MIS",
#                              TRUE ~ "ALL"),
#                            v = FALSE) {
#   so <- read_StockOverview(fn, v) %>%
#     group_gears_add_percent_discard(categories = categories) %>%
#     filter( !is.nan(perc) )
#
#   stopifnot(year == first(so$Year))
#   so_with_percraise <- assign_percent_to_unallocated_landings(so)
#   ## Add discard ratio for the Netherlands (weighted mean from all imported discards)
#   so_with_percraise$percraise[so_with_percraise$Country == "Netherlands"] <-
#     so_with_percraise %>%
#     filter(geargroup != "NL") %>%
#     {weighted.mean(.$perc, .$weight, na.rm = TRUE)}
#
#   raising_factor_per_group <- so_with_percraise %>%
#     ungroup() %>%
#     filter(is.finite(percraise)) %>%
#     mutate(percraise_round = round(percraise, 7)) %>%
#     group_by(group) %>%
#     summarise(percdisc = unique(percraise_round)) %>%
#     mutate(Year = year)
#   raising_factor_per_group
#
#   rfpg[[year - 2001]] <- raising_factor_per_group
#
#   # aaa <-  so_with_percraise %>% select(percraise_includes, percraise_weights)
#   # bbb <- data.frame(perc = scan(text = unlist(aaa[1,1]), na.strings = "NA", sep = ","),
#   #          weight = scan(text = unlist(aaa[1,2]), na.strings = "NA", sep = ","))
#   # weighted.mean(bbb$perc, bbb$weight, na.rm = TRUE)
#
#   raised_discards <- so_with_percraise %>%
#     filter(is.finite(percraise)) %>%
#     mutate(Catch.Cat. = "Discards", Discards.Imported.Or.Raised = "Raised Discard", Catch..kg = Catch..kg * percraise)
#   raised_discards %>% summarise(sum(Catch..kg))
#
#   somyrd <- bind_rows(so, raised_discards)
#
#   ## Imported / raised
#   somyrd %>% group_by(Catch.Cat., Discards.Imported.Or.Raised, count) %>% summarise(sum(Catch..kg))
#
#   saveRDS(somyrd, paste0("data/intercatch/catch_data_raised_discards_", year, ".Rds"))
# }
