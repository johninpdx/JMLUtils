# FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> PSQIScore <<
#______________________________________________________________________________
#' Calculates PSQI subscales and total scores
#'
#' @param inputTbl A tbl_df with SSID, then all raw PSQI numeric items in order.\cr
#'      Hence it is N x 19 (SSID + 18 PSQI items).\cr
#'      inputTbl cols 2 & 4 must be of type c('hms', 'difftime' )\cr
#'      inputTbl cols 3 and 5:19 must include either 'double' or 'numeric'
#'         in their class.\cr
#'      inputTbl col 1 (the ID column), the PSQI ID is normally some numeric
#'      but can be anything.\cr
#'      The 18 input items (ignoring SSID, the first column) must be
#'      included in this order, though they can be named anything:\cr
#'      - psq1\cr
#'      - psq2\cr
#'      - psq3\cr
#'      - psq4\cr
#'      - psq5a to psq5j (10 items)\cr
#'      - psq6\cr
#'      - psq7\cr
#'      - psq8\cr
#'      - psq9\cr
#'
#' @param wv Character string with the wave number (defaults to empty string).
#'      This string is appended (with a period) to the component names, which
#'      are all of the form >> pscN << (N = 1, 2, ...7, Tot). So if
#'      This string is nonempty, these names will all be of the form
#'      pscN.v(wv), where v() means "the value of".
#'
#' @return A tbl_df of SSID, PSQI scored items (7), and a total, hence N x 9. \cr
#'         All columns are converted to 'numeric'.
#' @export
PSQIScore <- function(inputTbl, wv = ""){
#' @import tidyverse
#' @import hms
  #Check that the input is OK (as far as possible)
  if(class(inputTbl[[4]])[1] != "hms" | class(inputTbl[[4]])[2] != "difftime") {
    cat("2nd column of input tbl is not class hms/difftime; aborting...")
    stop()
  }

  if(class(inputTbl[[4]])[1] != "hms" | class(inputTbl[[4]])[2] != "difftime") {
    cat("4th column of input tbl is not class hms/difftime; aborting...")
    stop()
  }

  if (length(names(inputTbl)) != 19) {
    cat("PSQIScore requires a 19 column tbl as input; aborting...")
    stop()
  }

  if (all(unlist(lapply(lapply(inputTbl, class)[c(3,5:19)],
            function(x){"double" %in% x | "numeric" %in% x}))) == T){
    #Just keep going
  } else {
    cat("At least 1 of input cols 3,5:19 is not type numeric or double; aborting...")
    stop()
  }

  if(any(is.na(inputTbl[,c(3,5:19)] ))){
    cat("Must set any NAs to 0 in all numeric cols of input table; aborting...")
    stop()
  }

  if(any(is.na(inputTbl[2,4]))){
    cat("Either q2 (time went to bed) or q3 (hr actual sleep) is missing; aborting...")
    stop()
  }

  # Assign raw item values:
  PSQ.items <- inputTbl
        #Makes scoring code compatible with any input as long as the
        #  variables are all included and in the right order.
        names(PSQ.items)[2] <- "ps_q1"
        names(PSQ.items)[3] <- "ps_q2"
        names(PSQ.items)[4] <- "ps_q3"
        names(PSQ.items)[5] <- "ps_q4"
        names(PSQ.items)[6] <- "ps_q5a"
        names(PSQ.items)[7] <- "ps_q5b"
        names(PSQ.items)[8] <- "ps_q5c"
        names(PSQ.items)[9] <- "ps_q5d"
        names(PSQ.items)[10] <- "ps_q5e"
        names(PSQ.items)[11] <- "ps_q5f"
        names(PSQ.items)[12] <- "ps_q5g"
        names(PSQ.items)[13] <- "ps_q5h"
        names(PSQ.items)[14] <- "ps_q5i"
        names(PSQ.items)[15] <- "ps_q5j"
        names(PSQ.items)[16] <- "ps_q6"
        names(PSQ.items)[17] <- "ps_q7"
        names(PSQ.items)[18] <- "ps_q8"
        names(PSQ.items)[19] <- "ps_q9"

  #Score the items
 PSQ.scales <- PSQ.items %>%
  # -- Component 1 (Subjective Sleep Quality) --
  mutate(psc1 = ps_q6) %>%
  # -- Component 3 (Sleep Duration)
  mutate(psc3 = case_when(ps_q4>7 ~ 0,
                            ps_q4<=7 & ps_q4>6 ~ 1,
                            ps_q4<=6 & ps_q4>=5 ~ 2,
                            ps_q4<5 ~ 3)) %>%
  # -- Component 6 -- (Use of Sleeping Meds)
  mutate(psc6 = ps_q7) %>%
  # -- Component 7 -- (Daytime Dysfunction)
  mutate(psc7 = ps_q8 + ps_q9) %>%
  mutate(psc7 = case_when(psc7==0 ~ 0,
                            psc7==1 | psc7==2 ~ 1,
                            psc7==3 | psc7==4 ~ 2,
                            psc7>4 ~ 3)) %>%
  # -- Component 2 -- (Sleep Latency)
  mutate(sl1 = case_when(ps_q2<=15 ~ 0,
                           ps_q2>15 & ps_q2<=30 ~ 1,
                           ps_q2>30 & ps_q2<=60 ~ 2,
                           ps_q2>60 ~ 3),
         sl2 = ps_q5a) %>%
  mutate(psc2 = sl1 + sl2) %>%
  mutate(psc2 = case_when(psc2==0 ~ 0,
                            psc2==1 | psc2==2 ~ 1,
                            psc2==3 | psc2==4 ~ 2,
                            psc2>4 ~ 3)) %>%
  # -- Component 4 -- (Habitual Sleep Efficiency)

  # ps_q1: usual bedtime (from 00:00:00 to 23:59:59)
  # ps_q3: usual time getting up
  # hib = Hours In Bed
  # hse = sleep efficiency (pct (sleep/time in bed))
  mutate(hib = if_else(ps_q1>=as.difftime("00:00:00") &
                          ps_q1<=as.difftime("12:00:00"), # > midnight, < noon
                        difftime(ps_q3, ps_q1, units="hours"),
                        difftime(as_hms("23:59:59"), ps_q1) + # < midnight
                          difftime(ps_q3, as_hms("00:00:00")))) %>%
  mutate(hse = (ps_q4/as.numeric(hib))*100) %>%
     #A few cases had the guy in bed longer than he slept. Set that to 100%
     mutate(hse = ifelse(hse>100,100, hse)) %>%
  mutate(psc4 = case_when(hse>85 ~ 0,
                          hse>=75 & hse<=85 ~ 1,
                          hse>=65 & hse<75 ~2,
                          hse<65 ~ 3)) %>%

   #Fixes NA problems on an "other" item which is sometimes missing...
   #...but the best is if all raw items with NAs are set to 0, except
   # for difftime variables.
   mutate(ps_q5j=ifelse(is.na(ps_q5j), 0, ps_q5j)) %>% #Fix NA prob here
   # -- Component 5 -- (Sleep Disturbances)
   mutate(psc5 = ps_q5b + ps_q5c + ps_q5d + ps_q5e +
          ps_q5f + ps_q5g + ps_q5h + ps_q5i +
          ps_q5j) %>%
   mutate(psc5 = case_when(psc5==0 ~ 0,
                           psc5>0 & psc5 <= 9 ~ 1,
                           psc5>9 & psc5 <= 18 ~ 2,
                           psc5>18 ~ 3)) %>%

   # -- Global PSQI Score --
   mutate(pscTot = psc1 + psc2 + psc3 + psc4 +
          psc5 + psc6 + psc7) %>%

  dplyr::select(SSID, psc1, psc2, psc3, psc4, psc5, psc6,
                psc7, pscTot)

# Add wave qualification to names, if desired:
  if(wv != ""){
    PSQ.names <- names(PSQ.scales)
    #Add '.wv' to the end of the scored scales
    vnams <- modify(names(PSQ.scales)[2:9], ~ glue(("{.x}.{wv}")))
    names(PSQ.scales) <- c("SSID", vnams)
  }
 # Coerce all variables to class numeric (which is sufficient)
 PSQ.scales <- lapply(PSQ.scales, function(x) {x<-as.numeric(x)}) %>% as_tibble()
return(PSQ.scales)

}
