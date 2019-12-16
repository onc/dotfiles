alias yi="yay"
# remove ALL orphaned packages
alias yro="yay -Qdt"
# update all packages
alias yu="yay -Syua"
# update pacman
alias pacu="sudo pacman -Syu"

alias yclean="yay -Qdt && yay -Scc"

# List all files installed by a given package
alias paclf="yay -Ql"
# Mark one or more installed packages as explicitly installed
alias pacexpl="pacman -D --asexp"
# Mark one or more installed packages as non explicitly installed
alias pacimpl="pacman -D --asdep"

# list all git-packages
function pgit() {
    pacman -Qs '.*-git' | grep '.*-git' | awk '{print $1}' | cut -d '/' -f 2
}

# restart some stuff
alias re-httpd="sudo systemctl restart httpd"
alias re-mysql="sudo systemctl restart mysqld"
