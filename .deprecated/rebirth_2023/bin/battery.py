#!python3

import sys, subprocess

output = subprocess.Popen(
        ['ioreg', '-rc', 'AppleSmartBattery'],
        stdout=subprocess.PIPE
    ).communicate()[0]

o_max = [l for l in output.splitlines() if b'MaxCapacity' in l][0]
o_cur = [l for l in output.splitlines() if b'CurrentCapacity' in l][0]

b_max = float(o_max.decode('utf-8').rpartition('=')[-1].strip())
b_cur = float(o_cur.decode('utf-8').rpartition('=')[-1].strip())

sys.stdout.write('{}%'.format(int(1000 * b_cur / b_max) / 10))
