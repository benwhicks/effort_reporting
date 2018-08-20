# mail test
library(mailR)
cc = strsplit("ben.hicks@oxley.nsw.edu.au; effort.grade@oxley.nsw.edu.au", split = "; ")
send.mail(from = "<octemp@oxley.nsw.edu.au>",
          to = cc,
          subject = "Multi send",
          body = "Dear Me,\nSorry, I need to test sending to three different addresses. You might get a few of these",
          smtp = list(host.name = "mail.oxley.nsw.edu.au",
                      user.name = "octemp",
                      passwd = "[t3mP1522]"),
          attach.files = "~/Desktop/Test pdf.pdf",
          authenticate = TRUE,
          send = TRUE)
