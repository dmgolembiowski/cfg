#!/usr/bin/python3

import sys
import json
import subprocess

INTERNAL = 'eDP-1'
EXTERNAL = 'DP-1'

outputs = json.loads(subprocess.check_output(['swaymsg', '-t', 'get_outputs']))

def swayoutput(name, state):
    subprocess.check_call(['swaymsg', '-t', 'command', 'output', name, state])

for output in outputs:
    if output['name'] == EXTERNAL and output['active']:
        swayoutput(INTERNAL, 'disable')
        sys.exit(0)

swayoutput(INTERNAL, 'enable')
