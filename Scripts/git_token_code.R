library(usethis)

#use git credentials
use_git_config(user.name="AlisonDeary-NOAA",user.email= "alison.deary@noaa.gov")

#May need to re-run code every so often to re-establish Git connection
usethis::create_github_token()
gitcreds::gitcreds_set()
#token is: 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

#link git to R studio- create github/github desktop repository first
usethis::use_git()

#connecting to github
