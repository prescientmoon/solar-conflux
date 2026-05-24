# This script was made to fix the sins of my past scripts
import sys
import re
import logging

log = logging.getLogger("cmf")
logging.basicConfig(level=logging.INFO)

if len(sys.argv) < 2:
	log.error("Usage: cmf <default-prefix>")
	sys.exit(1)

(defaultPrefix,) = sys.argv[1:]

messageRegex = re.compile("^(.+):\\s*(.*)$")

i = 0
firstPrefix = None
for line in sys.stdin:
	match = messageRegex.search(line)
	if i == 0:
		if match is None or match.group(2) == "3": # Detect :3
			print(f"{defaultPrefix}: {line}", end="")
		else:
			print(line, end="")
			firstPrefix = match.group(1)
	else:
		if match is None or match.group(1) != firstPrefix:
			print(line, end="")
		else:
			print(match.group(2), end="")

	i += 1
