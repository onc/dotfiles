alias cpu-performance="sudo cpupower frequency-set -g performance"
alias cpu-powersave="sudo cpupower frequency-set -g powersave"

function cpu-frequency() {
    watch grep \"cpu MHz\" /proc/cpuinfo
}

function cpu-toggle() {
    sudo cpupower frequency-set -g powersave
    sudo cpupower frequency-set -g performance
}

