# -*- coding: utf-8 -*-

# Colors - base16:
white = "#E0E0E0"
gray = "#303030"
green = "#90A959"
orange = "#D28445"
red = "#AC4142"

import subprocess

from i3pystatus import Status

status = Status(standalone=True)

status.register("clock",
        format=" %a %-d %b %H:%M",
        color=white)

status.run()
