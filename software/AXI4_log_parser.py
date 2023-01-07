# Verifies correct implementation of AXI4 protocol based on specific prints from each AXI4 channel
#
# Arguments
#     arg1: absolute path to .out file
#
# Example Usage
#     python3 AXI4_log_parser.py /scratch/edwinlim/hyperscale-soc-chipyard/sims/firesim/deploy/results-workload/2023-01-01--02-38-35-mempress-bare-1stream/mempress-bare-1stream0/metasim_stderr.out

import sys
import re
from collections import deque

id_pattern = re.compile("id\s*:\s*0x\w*")
user_pattern = re.compile("user\s*:\s*0x\w*")
addr_pattern = re.compile("addr\s*:\s*0x\w*")
len_pattern = re.compile("len\s*:\s*\d*")
size_pattern = re.compile("size\s*:\s*\d*")
burst_pattern = re.compile("burst\s*:\s*\d*")
pendingReads_pattern = re.compile("pendingReads\s*:\s*\d*")
pendingAWReq_pattern = re.compile("pendingAWReq\s*:\s*\d*")
pendingWReq_pattern = re.compile("pendingWReq\s*:\s*\d*")
maxReads_pattern = re.compile("maxReads\s*:\s*\d*")
maxWrites_pattern = re.compile("maxWrites\s*:\s*\d*")
arQueueLength_pattern = re.compile("arQueueLength\s*:\s*\d*")
awQueueLength_pattern = re.compile("awQueueLength\s*:\s*\d*")
data_pattern = re.compile("data\s*:\s*0x\w*")
last_pattern = re.compile("last\s*:\s*0x\w*")
resp_pattern = re.compile("resp\s*:\s*0x\w*")

# Map IDs to addresses for read/write responses sent to the FASED backend
sentReads = {}
sentWrites = {}

# Queues (ID, address) tuples for read/write responses currently queued in the LLC model
queuedReads = deque()
queuedWrites = deque()

def nextAndIncr(lines_iter_ref, lineNum_ref):
    return (next(lines_iter_ref), lineNum_ref + 1)

filepath = sys.argv[1]
file = open(filepath, 'r')
lines = file.readlines()
lines_iter = iter(lines)
lineNum = 1

# TODO: detect address overlaps as well
# TODO: detect dependencies between read/write channels
for line in lines_iter:
    if "AR FIRE BEGIN" in line:
        # id, user, addr, len, size, burst, pendingReads, maxReads
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        user = user_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        addr = addr_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        length = len_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        size = size_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        burst = burst_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingReads = pendingReads_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxReads = maxReads_pattern.findall(line)[0].split()[-1]
        queuedReads.append((id, addr))

    if "ARQUEUE DEQUEUE FIRE BEGIN" in line:
        # id, len, arQueueLength, maxReads
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        length = len_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        arQueueLength = arQueueLength_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxReads = maxReads_pattern.findall(line)[0].split()[-1]
        idAddr = queuedReads.popleft()
        assert(idAddr[0] == id)
        if id in sentReads:
            print("LINE " + str(lineNum) + ": FATAL ERROR: sending duplicate read ID to backend")
            break
        sentReads[idAddr[0]] = idAddr[1]

    if "R FIRE BEGIN" in line:
        # id, user, data, last, resp, pendingReads, maxReads
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        user = user_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        data = data_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        last = last_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        resp = resp_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingReads = pendingReads_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxReads = maxReads_pattern.findall(line)[0].split()[-1]
        if "0x1" in last:
            del sentReads[id]

    if "AW FIRE BEGIN" in line:
        # id, user, addr, len, size, burst, pendingAWReq, pendingWReq, maxWrites
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        user = user_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        addr = addr_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        length = len_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        size = size_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        burst = burst_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingAWReq = pendingAWReq_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingWReq = pendingWReq_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxWrites = maxWrites_pattern.findall(line)[0].split()[-1]
        queuedWrites.append((id, addr))

    if "AWQUEUE DEQUEUE FIRE BEGIN" in line:
        # id, awQueueLength, maxWrites
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        awQueueLength = awQueueLength_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxWrites = maxWrites_pattern.findall(line)[0].split()[-1]
        idAddr = queuedWrites.popleft()
        assert(idAddr[0] == id)
        if id in sentWrites:
            print("LINE " + str(lineNum) + ": FATAL ERROR: sending duplicate write ID to backend")
            break
        sentWrites[idAddr[0]] = idAddr[1]

    if "B FIRE BEGIN" in line:
        # id, user, resp, pendingAWReq, pendingWReq, maxWrites
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        id = id_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        user = user_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        resp = resp_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingAWReq = pendingAWReq_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        pendingWReq = pendingWReq_pattern.findall(line)[0].split()[-1]
        line, lineNum = nextAndIncr(lines_iter, lineNum)
        maxWrites = maxWrites_pattern.findall(line)[0].split()[-1]
        del sentWrites[id]

    lineNum += 1

file.close()





