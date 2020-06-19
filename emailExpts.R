library(blastula)
txtmsg <- compose_email(
  body=md(c(
    msg
  ))
)
msg=" Up Yours"
create_smtp_creds_key(
  id="gmail",
  user="johninsepdx@gmail.com",
  provider="gmail"
)

txtmsg %>%
  smtp_send(
    from="johninsepdx@gmail.com",
    to = "johninsepdx@gmail.com",
    subject = "--- Your Analysis Is Done ---",
    credentials = creds_key(id="gmail")
  )
#After executing the smtp_send code, you are asked for your
# gmail password, which is
#          2s$uYpL$e#qT%WP5d!^jj0RLo28aI7

#It would be nice to find a way to securely store this. O
# think it's possible using a 'keyring' package 'key_set'
# function, of which there are several .

# maybe something like this, but it's not clear how to use it....
keyring::key_set_with_value(service = "gmail",
                            username = "johninsepdx@gmail.com",
                            password = "2s$uYpL$e#qT%WP5d!^jj0RLo28aI7")


setupEmail("johninsepdx")
sendEmail(gmailAcct="johninsepdx",
          subj="Test of v110.1",
          msg="Whee, it worked!!",
          sendToEmail="jlight@ori.org")
