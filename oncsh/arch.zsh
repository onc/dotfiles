alias yi="yaourt"
# remove ALL orphaned packages
alias yro="yaourt -Qdt"
# clean package
alias yc="yaourt -Scc"
# update all packages
alias yu="yaourt -Syua"
# update pacman
alias pacu="sudo pacman -Syu"

# clean up packages
alias yclean="yaourt -Qdt && yaourt -Scc"

# List all files installed by a given package
alias paclf="yaourt -Ql"
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
