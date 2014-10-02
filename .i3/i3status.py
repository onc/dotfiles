# -*- coding: utf-8 -*-

# Colors - base16:
white = "#E0E0E0"
gray = "#303030"
green = "#90A959"
orange = "#D28445"
red = "#AC4142"

import subprocess

from i3pystatus import Status
from i3pystatus.mail import imap

status = Status(standalone=True)

status.register("clock",
        format="%a %-d %b %H:%M",
        color=white)

status.register("load",
        critical_color=red,
        color=white,
        critical_limit=2,
        interval=5)

status.register("shell",
        color=white,
        command="cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor",
        interval=5)

status.register("mem",
        format="{used_mem:.0f}/{total_mem:.0f} MiB",
        round_size=0,
        color=white,
        warn_color=orange,
        alert_color=red)

status.register("temp",
    format="{temp:.0f}°C",
    color=white)

status.register("battery",
    format="{status} {consumption:.0f}W {percentage:.0f}% {remaining:%E%hh:%Mm}",
    alert=True,
    alert_percentage=5,
    status={
        "DIS": "",
        "CHR": "",
        "FULL": "",
    },
    color=orange,
    full_color=white,
    charging_color=green,
    critical_color=red)

status.register("network",
    interface="enp0s25",
    format_up="{v4cidr}",
    color_up=white,
    color_down=gray)

status.register("wireless",
    interface="wlp3s0",
    format_up=" {essid}/{v4} {quality:03.0f}%",
    color_up=white,
    color_down=gray)

status.register("alsa",
    format=" {volume}%",
    color=white,
    color_muted=gray)

status.register("backlight",
    format=" {brightness}/{max_brightness}",
    interval=1,
    color=white)

status.register("uptime",
        color=white)

status.register("shell",
        color=white,
        command="/mnt/hdd/dotfiles/yaUpdates.sh",
        interval=600)

status.run()
