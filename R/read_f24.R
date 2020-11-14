#' Read OPTA-F24 from a single file.
#' Internal use only.
#' 
#' @keywords internal
#' @return A tibble wich contains the evnets of the file specified in `file_path`.
read_f24_single_file_ <- function(file_path) {
  # read xml file -----------------------------------------------------------
  f24_opta <- xml2::read_xml(file_path)
  
  # parse -------------------------------------------------------------------
  doc <- XML::xmlParse(f24_opta)
  
  # convert to dataframe ----------------------------------------------------
  # game infos
  game <- XML::getNodeSet(doc, "//Game")
  game_attributs <- 
    sapply(game, FUN = function(g) {
      comp <- XML::xmlGetAttr(g, "competition_name")
      season <- XML::xmlGetAttr(g, "season_name")
      j <- XML::xmlGetAttr(g, "matchday")
      away <- XML::xmlGetAttr(g, "away_team_name")
      home <- XML::xmlGetAttr(g, "home_team_name")
      return(dplyr::bind_cols(compet = comp, season = season, match_day = j, home_team = home, away_team = away))
    }, simplify = FALSE) %>%
    unlist() %>%
    tibble::as_tibble_row()
  
  # init firsts rows of the final tibble
  events <- XML::getNodeSet(doc, "//Event")
  
  # loop on every other events
  events_tibble <- NULL
  for (e in 1:length(events)) {
    
    node <- XML::xmlToList(events[[e]])
    if (!is.atomic(node)) {
      events_tibble_tmp <- 
        node[['.attrs']] %>% 
        tibble::as_tibble_row() 
    } else {
      events_tibble_tmp <-
        node %>%
        tibble::as_tibble_row() 
    }
    
    if (is.null(events_tibble)) {
      events_tibble <- events_tibble_tmp
    } else {
      events_tibble <- 
        events_tibble %>%
        dplyr::bind_rows(events_tibble_tmp)
    }
  }
  
  # add qualifiers info
  qualifiers <- NULL
  for (e in 1:nrow(events_tibble)) {
    qualifiers_events_e <- XML::getNodeSet(events[[e]], "./Q")
    
    qualifier_tmp <- NULL
    if (length(qualifiers_events_e) > 0) {
      for (q in 1:length(qualifiers_events_e)) {
        if (is.null(qualifier_tmp)) {
          qualifier_tmp <- 
            XML::xmlToList(qualifiers_events_e[[q]]) %>% 
            tibble::as_tibble_row()  
        } else {
          qualifier_tmp <- 
            XML::xmlToList(qualifiers_events_e[[q]]) %>% 
            tibble::as_tibble_row() %>%
            dplyr::bind_rows(qualifier_tmp) 
        }
      }
      
      qualifier_tmp <- qualifier_tmp %>%
        dplyr::mutate(parent_event_id = XML::xmlAttrs(events[[e]])[['id']])
      
      if (!("value" %in% names(qualifier_tmp))) {
        qualifier_tmp$value <- NA
      } 
    }
    
    if (is.null(qualifiers)) {
      qualifiers <- qualifier_tmp
    } else if (!is.null(qualifier_tmp)) {
      qualifiers <- 
        qualifiers %>%
        dplyr::bind_rows(qualifier_tmp)
    }
  }
  
  # join qualifiers ---------------------------------------------------------
  events_tibble_cplt <-
    events_tibble %>%
    dplyr::left_join(qualifiers, by=c("id"="parent_event_id")) %>%
    dplyr::bind_cols(game_attributs)

  # return ------------------------------------------------------------------
  return(events_tibble_cplt)
}



#' Read OPTA-F24 from a single file OR all files from a directory. 
#' 
#' @param file_path the OPTA-f24 file to parse. The extension has to be `.xml`.
#' @param dir_path the OPTA-f24 file to parse. The files in the dir have to be f24 with an `.xml` extension.
#' @return A tibble wich contains the events of the file specified in `file_path` or ones from `dir_path` all together.
#' @seealso read_opt
#' 
#' @examples  
#' \dontrun{
#' # use as follows
#' my_events_tibble <- read_f24('my_f24_file.xml')
#' }
#' @export
read_f24 <- function(file_path=NULL, dir_path=NULL) {
  # verify arguments --------------------------------------------------------
  if (is.null(file_path) & is.null(dir_path)) {
    stop("One of `file_path` OR `dir_path` argument has to be set.")
  } else if (!is.null(file_path) & !is.null(dir_path)) {
    stop("Only ONE of `file_path` OR `dir_path` argument has to be set.")
  }
  
  # read files --------------------------------------------------------------
  if (!is.null(dir_path)){ # case of a dir is provided
    files_path <- 
      list.files(path = dir_path, pattern = "*.xml", full.names = TRUE)
      
    events_tibble_cplt <- 
      lapply(files_path, read_f24_single_file_) %>% 
      dplyr::bind_rows()
  } else { # case of a single file is provided
    events_tibble_cplt <- 
      read_f24_single_file_(file_path)
  }
  
  return(events_tibble_cplt)
}