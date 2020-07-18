#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> setupGmail <<
#______________________________________________________________________________
#' Setup auto-sending an Email after some other code runs
#'
#' @details You will be prompted for your Gmail account password when you run
#'  this function in a new session. After you have provided it, you do not
#'  need to rerun this setup function again within the same session. All it's
#'  doing is setting up a security key to access your gmail account. \cr \cr
#'     In addition, you will need to go into your Gmail account and set it
#'  to accept remote operation. To do this, go to
#'      'myaccount.google.com/security', or
#'  click on your picture (if you're signed into, say, Chrome with your Gmail
#'  account) to get to your 'myaccount' page. Then click on 'security' in the
#'  list of options to the left of the window, and scroll down to 'less
#'  secure app access', and turn this ON. Gmail will squawk, but you must do
#'  this or these functions will not work!
#'
#'@param gmailAcct Character string containing the name of the gmail account
#'  to send the Email from (sorry, this function can only send from gmail
#'  accounts at the moment). Do not include the '@gmail.com' part; it is
#'  assumed, and added automatically in the function code.
#'
#' @examples
#'   setupGmail("mygmailacct")
#'
#' @return A list containing [[1]] the text message body, [[2]] credential key
#' @export
setupGmail <- function(gmailAcct){
  #' @import blastula
  #' @import keyring
  #This will ask for your gmail password
  credKey <- create_smtp_creds_key(
    id="gmail",
    user=paste0(gmailAcct, "@gmail.com"),
    provider="gmail"
  )
  cat("Ready to send; use 'sendGmail' to do so.")
}

#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
# >> sendGmail <<
#______________________________________________________________________________
#' Send an email from a gmail account
#'
#' Requires running 'setupEmail' first. This function is called after some
#' other code runs, or whenever you like.
#'
#' @param gmailAcct Character string containing the name of the gmail account
#'  to send the Email from (the same account you used in 'setupEmail'). DO
#'  NOT include the '@gmail.com' part, just the account up to there.
#' @param subj Character string: the desired subject line
#' @param msg Character string: the desired message
#' @param sendToEmail Character string with the recipient Email (e.g.,
#'   "smith@smithsdomain.com", etc.). Unlike, 'gmailAcct', here you have to
#'   specify the entire Email, i.e. including the @whatever.xxx part.
#'
#' @examples
#'   setupGmail ("mygmailacct")
#'   # Notice that "mygmailacct" must be the same one used when you
#'   # ran 'setupEmail'
#'   sendGmail(gmailAcct="mygmailacct",
#'             subj="Your model is done",
#'             msg="V.10.1 model completed",
#'             sendToEmail="anyoldEmail@whatever.com")
#'
#' @return Nothing. It just sends the Email
#' @export
sendGmail <- function(gmailAcct, subj, msg, sendToEmail){
  #' @import blastula
  #' @import keyring
  txtmsg <- compose_email(
    body=md(c(
      msg
    ))
  )
  txtmsg %>%
    smtp_send(
      from=paste0(gmailAcct, "@gmail.com"),
      to = sendToEmail,
      subject = subj,
      credentials = creds_key(id="gmail")
    )
}

