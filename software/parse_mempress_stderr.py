# Keeps track of tags sent and received by mempress accelerator by parsing .out file
#
# Arguments
#     arg1: absolute path to .out file
#
# Example Usage
#     python3 parse_mempress_stderr.py /scratch/edwinlim/hyperscale-soc-chipyard/sims/firesim/deploy/results-workload/2022-11-20--20-23-31-mempress-bare-1stream/mempress-bare-1stream0/metasim_stderr.out

import sys
import re

filepath = sys.argv[1]
file = open(filepath, 'r')
tag_tracker = {}
addr_tracker = {}
sendtag_pattern = re.compile('sendtag:\s*\d*')
gettag_pattern = re.compile('gettag:\s*\d*')
add_back_tag_pattern = re.compile('tags_for_issue_Q add back tag\s*\d*')
AR_FIRE_pattern = re.compile('AR FIRE: adding address 0x.*')
AR_release_pattern = re.compile('releasing address 0x.*')

lines = file.readlines()

for line in lines:
    sendtag_list = sendtag_pattern.findall(line)
    gettag_list = gettag_pattern.findall(line)
    add_back_tag_list = add_back_tag_pattern.findall(line)
    AR_FIRE_list = AR_FIRE_pattern.findall(line)
    AR_release_list = AR_release_pattern.findall(line)

    # sendtag stuff
    if (len(sendtag_list) != 0):
        sendtag_num = int(sendtag_list[0].split()[1])
        if sendtag_num in tag_tracker:
            tag_tracker[sendtag_num] = tag_tracker[sendtag_num] + 1
        else:
            tag_tracker[sendtag_num] = 1;

    # add back tag stuff
    if (len(add_back_tag_list) != 0):
        add_back_tag_num = int(add_back_tag_list[0].split()[-1])
        if add_back_tag_num in tag_tracker:
            tag_tracker[add_back_tag_num] = tag_tracker[add_back_tag_num] - 1
        else:
            print("ERROR: This shouldn't have happened")
            break

    if (len(AR_FIRE_list) != 0):
        AR_FIRE_addr = AR_FIRE_list[0].split('x')[-1].split(' to')[0]
        if AR_FIRE_addr in addr_tracker:
            addr_tracker[AR_FIRE_addr] = addr_tracker[AR_FIRE_addr] + 1
        else:
            addr_tracker[AR_FIRE_addr] = 1
        #print('AR FIRE ', AR_FIRE_addr)

    if (len(AR_release_list) != 0):
        AR_release_addr = AR_release_list[0].split('x')[-1].split(' to')[0]
        if AR_release_addr in addr_tracker:
            addr_tracker[AR_release_addr] = addr_tracker[AR_release_addr] - 1
            if addr_tracker[AR_release_addr] == 0:
                del addr_tracker[AR_release_addr]
        else:
            print("ERROR: This shouldn't have happened")
            break
        #print('AR RELEASE ', AR_release_addr)

file.close()

for key, value in tag_tracker.items():
    print(key, '->', value)

for key, value in addr_tracker.items():
    print(key, '->', value)

