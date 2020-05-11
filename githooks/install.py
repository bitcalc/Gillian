#!/usr/bin/env python3
import os
from common import *

for f in hook_names:
  absdest = os.path.join(destination, f)
  copyfile(os.path.join(githooksFolder, f), absdest)
  os.chmod(absdest, os.stat(absdest).st_mode | stat.S_IEXEC)
