#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> setupEmail <<
#______________________________________________________________________________
#' Setup auto-sending an Email after some other code runs
#'(only works for John's Gmail account, for now. Maybe generalized
#' someday).
#'
#' @return A list containing [[1]] the text message body, [[2]] credential key
#' @export
setupEmail <- function(){
  #' @import blastula
  #' @import keyring
  #This will ask for your gmail password
  credKey <- create_smtp_creds_key(
    id="gmail",
    user="johninsepdx@gmail.com",
    provider="gmail"
  )
  cat("Ready to send; use 'sendEmail' to do so.")
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> sendEmail <<
#______________________________________________________________________________
#' Send an email (for now, just from John's Gmail account)
#'
#' Requires running 'setupEmail' first. This function is called after some
#' other code runs, or whenever you like.
#'
#' @param subj Character string: the desired subject line
#' @param msg Character string: the desired message
#'
#' @return Nothing. It just sends the Email
#' @export
sendEmail <- function(subj, msg){
  #' @import blastula
  #' @import keyring
  txtmsg <- compose_email(
    body=md(c(
      msg
    ))
  )
  txtmsg %>%
    smtp_send(
      from="johninsepdx@gmail.com",
      to = "johninsepdx@gmail.com",
      subject = subj,
      credentials = creds_key(id="gmail")
    )
}

