alias screen-off="xset dpms force off"
# monitor-stuff
alias sdo=" xrandr \
--output LVDS-0 --auto --primary --rotate normal --pos 0x0 \
--output DP-0 --off --output DP-3 --off \
--output VGA-0 --off \
&& feh --bg-fill ~/.i3/back.png"

alias sd-only=" xrandr \
--output DP-0 --auto --primary --rotate normal --pos 0x0 \
--output VGA-0 --auto --above LVDS-0 \
--output LVDS-0 --off \
&& feh --bg-fill ~/.i3/back.png"

alias sda=" xrandr \
--output LVDS-0 --auto --primary --rotate normal --pos 0x0 \
--output DP-0 --auto --above LVDS-0 \
--output VGA-0 --auto --above LVDS-0 \
&& feh --bg-fill ~/.i3/back.png"

alias sdr=" xrandr \
--output LVDS-0 --auto --primary --rotate normal --pos 0x0 \
--output DP-0 --auto --right-of LVDS-0 \
--output VGA-0 --auto --right-of LVDS-0 \
&& feh --bg-fill ~/.i3/back.png"

alias sdl=" xrandr \
--output LVDS-0 --auto --primary --rotate normal --pos 0x0 \
--output DP-0 --auto --left-of LVDS-0 \
--output VGA-0 --auto --left-of LVDS-0 \
&& feh --bg-fill ~/.i3/back.png"

alias sd-mirror="xrandr \
--output VGA-0 --auto --primary --rotate normal --pos 0x0 \
--output LVDS-0 --auto --same-as VGA-0"

alias sda-full=" xrandr \
--output LVDS-0 --auto --primary --rotate normal --pos 0x0 \
--output DP-3 --auto --above LVDS-0 \
--output VGA-0 --auto --above LVDS-0 \
&& feh --bg-fill ~/.i3/back.png"
