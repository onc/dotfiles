# -*- coding: utf-8 -*-

import subprocess

from i3pystatus import Status
from i3pystatus.mail import imap

status = Status(standalone=True)

status.register("clock",
        format="%a %-d %b %H:%M")

status.register("load",
        critical_color="#f2777a")

status.register("temp",
    format="{temp:.0f}°C",)

status.register("battery",
    format="{status} {consumption:.2f}W {percentage:.2f}% {remaining:%E%hh:%Mm}",
    alert=True,
    alert_percentage=5,
    status={
        "DIS": "",
        "CHR": "",
        "FULL": "",
    },
    color="#cccccc",
    full_color="#99cc99",
    charging_color="#f99157",
    critical_color="#f2777a")

status.register("network",
    interface="enp0s25",
    format_up="{v4cidr}",
    color_up="#99cc99",
    color_down="#999999")

status.register("wireless",
    interface="wlp3s0",
    format_up="{essid}/{v4} {quality:03.0f}%",
    color_up="#99cc99",
    color_down="#999999")

status.register("alsa",
    format=" {volume}%",)

status.register("backlight",
    format=" {brightness}/{max_brightness}",
    interval=1)

status.register("uptime")

# status.register("mail",
#        backends=[
#            imap.IMAP(
#                host="imap.googlemail.com",
#                username="", 
#                password=""
#            )
#        ],
#        email_client="thunderbird",
#        hide_if_null=False)

status.run()
