#! /usr/bin/env python

from i3pystatus import IntervalModule
from i3pystatus.battery import BatteryChecker, Battery


class Lifebar(IntervalModule):

    format = '{bar}'
    color = '#FFFFFF'
    warn_color = '#FF6600'
    alert_color = '#FF0000'
    charging_color = '#00FF00'
    steps = 5
    full_symbol = ''
    empty_symbol = ''
    seperator = ' '
    warn_percentage = 20
    alert_percentage = 5

    settings = (
        ('color', 'default color'),
        ('warn_color', 'warning color'),
        ('alert_color', 'alert color'),
        ('charging_color', 'color to use while charging'),
        ('steps', 'number of steps'),
        ('full_symbol', 'symbol to use for full'),
        ('empty_symbol', 'symbol to use for empty'),
        ('seperator', 'seperator between symbols'),
        ('warn_percentage', 'percentage for warning'),
        ('alert_percentage', 'percentage for alert')
    )

    def run(self):
        batteryChecker = BatteryChecker()
        batteryChecker.init()
        paths = batteryChecker.paths

        batteries = []
        for path in paths:
            try:
                batteries.append(Battery.create(path))
            except FileNotFoundError:
                print('No batteries found')

        charging = batteryChecker.battery_status(batteries)

        percentage = batteryChecker.percentage(batteries)
        stepSize = 100 / self.steps
        fullSymbols = round(percentage / stepSize)
        emptySymbols = self.steps - fullSymbols

        bar = self.seperator.join(self.full_symbol * fullSymbols)
        bar += self.seperator
        bar += self.seperator.join(self.empty_symbol * emptySymbols)

        if charging == 'Charging':
            color = self.charging_color
        elif percentage <= self.alert_percentage:
            color = self.alert_color
        elif percentage <= self.warn_percentage:
            color = self.warn_color
        else:
            color = self.color

        cdict = {
            'bar': bar,
            'percentage': round(percentage)
        }

        self.data = cdict
        self.output = {
            'full_text': self.format.format(**cdict),
            'color': color
        }

if __name__ == "__main__":
    lifebar = Lifebar()
    lifebar.run()
