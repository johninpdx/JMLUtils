% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/email.R
\name{sendGmail}
\alias{sendGmail}
\title{Send an email from a gmail account}
\usage{
sendGmail(gmailAcct, subj, msg, sendToEmail)
}
\arguments{
\item{gmailAcct}{Character string containing the name of the gmail account
to send the Email from (the same account you used in 'setupEmail'). DO
NOT include the '@gmail.com' part, just the account up to there.}

\item{subj}{Character string: the desired subject line}

\item{msg}{Character string: the desired message}

\item{sendToEmail}{Character string with the recipient Email (e.g.,
"smith@smithsdomain.com", etc.). Unlike, 'gmailAcct', here you have to
specify the entire Email, i.e. including the @whatever.xxx part.}
}
\value{
Nothing. It just sends the Email
}
\description{
Requires running 'setupEmail' first. This function is called after some
other code runs, or whenever you like.
}
\examples{
  setupGmail ("mygmailacct")
  # Notice that "mygmailacct" must be the same one used when you
  # ran 'setupEmail'
  sendGmail(gmailAcct="mygmailacct",
            subj="Your model is done",
            msg="V.10.1 model completed",
            sendToEmail="anyoldEmail@whatever.com")

}
