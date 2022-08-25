# -*- coding: utf-8 -*-

from i3pystatus import Status

# Colors - base16:
white = "#E0E0E0"
gray = "#303030"
green = "#90A959"
orange = "#D28445"
red = "#AC4142"

status = Status(standalone=True)

status.register("clock",
                format=" %a %-d %b %H:%M",
                color=white)

status.register("mem",
                format="Mem: {used_mem:.0f}/{total_mem:.0f} MiB",
                round_size=0,
                color=white,
                warn_color=orange,
                alert_color=red)

status.register("cpu_usage",
                format="Cpu: {usage:03}%")

status.register("temp",
                format="Cpu: {temp:.0f}°C",
                color=white)

status.register("shell",
                color=white,
                command="echo Gpu:; nvidia-smi -q -d TEMPERATURE | grep 'Current' | awk '{print $5}'; echo °C;",
                interval=5)

status.register("shell",
                color=white,
                command="echo ; cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor",
                interval=5)

status.register("network",
                interface="eno1",
                format_up="{v4cidr}",
                color_up=white,
                color_down=gray)

status.register("pulseaudio",
                format=" {volume}%",
                color_unmuted=white,
                color_muted=gray)

status.run()
