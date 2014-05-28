#!/usr/bin/env python
# encoding: utf-8

import os
import re

out_re = re.compile("\d+-\d+-\d\.out")
print("size,line,associativity,writes,write misses,reads,read misses,misses,accesses,miss\
        ratio");

for dirname, dirnames, filenames in os.walk("."):
    for filename in filenames:
        if out_re.match(filename):
            with open(filename, "r") as f:
                writes = 0
                wmisses = 0
                reads = 0
                rmisses = 0
                misses = 0
                accesses = 0
                miss_ratio = 0.0
                for line in f:
                    if line.startswith("  Writes:"):
                        writes = line.split(" ")[3].strip()
                    if line.startswith("  Write Misses:"):
                        wmisses = line.split(" ")[4].strip()
                    if line.startswith("  Reads:"):
                        reads = line.split(" ")[3].strip()
                    if line.startswith("  Read Misses:"):
                        rmisses = line.split(" ")[4].strip()
                    if line.startswith("  Misses:"):
                        misses = line.split(" ")[3].strip()
                    if line.startswith("  Accesses:"):
                        accesses = line.split(" ")[3].strip()
                    if line.startswith("  Miss Ratio:"):
                        miss_ratio = line.split(" ")[4].strip()

                filename = filename.split(".")[0]
                size = filename.split("-")[0]
                line = filename.split("-")[1]
                assoc = filename.split("-")[2]
                print("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s" % (size, line, assoc,writes, wmisses, reads, rmisses,
                    misses, accesses, miss_ratio))


