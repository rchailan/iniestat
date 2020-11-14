#' Collect all opta available files for a specified season.
#' 
#' @param user User to connect to the \code{root_url} account`;`
#' @param passwd Password of the \code{user} to use.
#' @param season Season of the games to collect. If set, nor \code{match_id} nor \code{matches_id} are read.
#' @param match_id Specific game id. Do not use with \code{season} set.
#' @param matches_id Specific games ids. Do not use with \code{season} set. If set, will overwrite \code{match_id}.
#' @param store_f7 Store as well corresponding F7 file. Default set to \code{TRUE}.
#' @param output_dir Output directory to write files to.
#' @param root_url The url to collect the f24 file. By default it is set to \url{http://omo.akamai.opta.net}.
#' 
#' @return Save to severals .xml files the collected .xml files.
#' @examples  
#' \dontrun{
#' # if you want all the available game of the season `season`
#' collect_f24(user="XX12345", passwd="mypasswd", season=2020)
#' 
#' # if you want the game of id `match_id`
#' collect_f24(user="XX12345", passwd="mypasswd", match_id="7890")
#' 
#' # if you want several games, use `matches_id` as
#' collect_f24(user="XX12345", passwd="mypasswd", match_id=c("7890", "7891", "7177"))
#' 
#' # if you need to specify the root url you can set
#' collect_f24(user="XX12345", passwd="mypasswd", season=2020, root_url="https://my_new_root_url")
#' }
#' @export
collect_f24 <- function(user, 
                        passwd, 
                        output_dir, 
                        season=NULL, match_id=NULL, matches_id=NULL, 
                        root_url="http://omo.akamai.opta.net", 
                        store_f7=TRUE, ...) {
  # verify arguments --------------------------------------------------------
  if (is.null(user)) {
    stop("user has to be specified.")
  } else if (is.null(passwd)) {
    stop("passwd has to be specified.")
  } else if (is.null(output_dir)) {
    stop("output_dir has to be set.")
  } else if (is.null(season) & is.null(match_id) & is.null(matches_id)) {
    stop("You need to set either a `season` or a `match_id` or `matches_id`.")
  } 
  
  # create dir if not exists ------------------------------------------------
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  

  # download(s) -------------------------------------------------------------
  if(!is.null(match_id)) { # single game
    
    f <- curl::curl_download(url = glue::glue("{root_url}/?game_id={as.numeric(match_id)}&feed_type=F24&user={user}&psw={passwd}"),
                        destfile = glue::glue("{output_dir}/F24-{match_id}.xml"))

    if(file.exists(f)) {print(glue::glue("File {f} collected."))} else {stop("Something went wrong.")}
    
    if(store_f7) {
      collect_f7_(user=user, passwd=passwd, match_id=match_id, output_dir=output_dir, root_url=root_url)
    }
  } else if (!is.null(matches_id)) { # various games
    # not working cause to fast?
    # do.call(collect_f24, list(match_id=matches_id,
    #                           user=user,
    #                           passwd=passwd,
    #                           output_dir=output_dir,
    #                           root_url=root_url,
    #                           store_f7=store_f7))
    
    for (ma in matches_id) {
      collect_f24(match_id=ma,
                  user=user,
                  passwd=passwd,
                  output_dir=output_dir,
                  root_url=root_url,
                  store_f7=store_f7)
    }
  }

}

#'
#' @internal
collect_f7_ <- function(user, 
                       passwd, 
                       output_dir, 
                       season=NULL, match_id=NULL, matches_id=NULL, 
                       root_url="http://omo.akamai.opta.net") {
  # create dir --------------------------------------------------------------
  if (!dir.exists(glue::glue("{output_dir}/F7"))) {
    dir.create(glue::glue("{output_dir}/F7"))
  }

  # download ----------------------------------------------------------------
  f <- curl::curl_download(url = glue::glue("{root_url}/?game_id={as.numeric(match_id)}&feed_type=F7&user={user}&psw={passwd}"),
                           destfile = glue::glue("{output_dir}/F7/F7-{match_id}.xml"))
  
  # check -------------------------------------------------------------------
  if(file.exists(f)) {print(glue::glue("File {f} collected."))} else {stop("Something went wrong.")}
}

#'
#' @return A tibble made from four columns: a \code{match_id}, \code{home_team}, \code{away_team},
#' @internal
collect_f1_match_id <- function(user, 
                                passwd,
                                team_id,
                                competition=24,
                                root_url="http://omo.akamai.opta.net") {
  
}
  

#' Collect teams' id for a \code{season} and a \code{competition}.
#'
#' @param user User to connect to the \code{root_url} account`;`
#' @param passwd Password of the \code{user} to use.
#' @param season Season of the games to collect. If set, nor \code{match_id} nor \code{matches_id} are  read.
#' @param competition Competition id. Default set to \code{24}.
#' @param root_url The url to collect the f24 file. By default it is set to \url{http://omo.akamai.opta.net/competition.php}.
#' 
#' @return A tibble containing two columns: a \code{team_id} and a \code{label}
#' 
#' @example
#' \dontrun{
#' iniestat::collect_f1_teams_id(user = "MYUSER", passwd = "MYPASSWD", season = 2020)
#' }
#' @export
collect_f1_teams_id <- function(user, 
                                passwd,
                                competition=24,
                                season,
                                root_url="http://omo.akamai.opta.net/competition.php") {
  require(dplyr)
  
  # collect f1 file ---------------------------------------------------------
  tmp <- curl::curl_download(
    url = glue::glue("{root_url}?competition={as.numeric(competition)}&season_id={as.numeric(season)}&feed_type=F1&user={user}&psw={passwd}"),
    destfile = tempfile()
    )
  
  # building tibble ---------------------------------------------------------
  f1_opta <- xml2::read_xml(tmp)
  doc <- XML::xmlParse(f1_opta)
  teams_tibble <- NULL
  nodes <- XML::getNodeSet(doc, "//Team")
  
  if (length(nodes) == 0) stop("Error while reading xml file. Please check your credentials or root_url or season.") 
  
  for (n in 1:length(nodes)) {
    node <- XML::xmlToList(nodes[[n]])

    teams_tibble_tmp <-
      node[['.attrs']] %>%
      tibble::as_tibble_row() %>%
      dplyr::bind_cols(team_name = node$Name)

    teams_tibble <-
      teams_tibble %>%
      dplyr::bind_rows(teams_tibble_tmp)
  }
  
  return(teams_tibble)
}