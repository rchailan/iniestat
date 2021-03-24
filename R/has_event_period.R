nantes_nimes <- iniestat::read_f24(file_path = "~/Desktop/nantes-nimes/J02_FCNNO.xml")
nantes_nimes %>% 
  dplyr::filter(type_id==16) %>% 
  dplyr::distinct(id, event_id ,type_id, period_id, min  , sec   ,team_id, outcome )


nantes_nimes <- 
  nantes_nimes %>% 
  dplyr::mutate(
    timestamp = lubridate::ymd_hms(timestamp)
  ) %>% 
  dplyr::select(type_id, team_id, timestamp, min  , sec)

df <- 
  nantes_nimes %>% 
  distinct() %>%
  group_by(team_id)
  

#' function to check if a specific type of event has occured within a `period` of time regarding a `timestamp_ref`
hasEventWithinPeriod <- function(df, team_id, timestamp_ref, type_id_searched, period=lubridate::period(1, units="second")) {
  has_event <- FALSE
  
  n_events <- 
    df %>%
    ungroup() %>%
    dplyr::filter(
      team_id == team_id,
      type_id == type_id_searched,
      `timestamp` > timestamp_ref & `timestamp` <= (timestamp_ref + period)
    ) %>%
    count() %>% 
    pull()
  
  if (n_events > 0) {has_event <- TRUE}
  
  return(has_event)
}


res <-
  df %>%
  dplyr::filter(type_id == "49") %>%
  rowwise() %>%
  mutate(
    has_event_m_1sec = hasEventWithinPeriod(df=df, team_id=team_id, timestamp_ref=timestamp,  type_id_searched="16", period= lubridate::period(1, units="second")),
    has_event_m_20sec = hasEventWithinPeriod(df=df, team_id=team_id, timestamp_ref=timestamp,  type_id_searched="16", period= lubridate::period(20, units="second"))
  )

res
res %>% View()

