
# Checking configuration
usethis::git_sitrep()

# Set global configuration
usethis::use_git_config(user.name = "Noora Sissala", user.email ="noora.sissala@ki.se")

#Takes you to github.com create new token.
usethis::create_github_token()

#Use to set your new token in your configuration
gitcreds::gitcreds_set()

# Initialize git in a project that does not have it yet
usethis::use_git()

# Connect it to a remote repository
usethis::use_github()
