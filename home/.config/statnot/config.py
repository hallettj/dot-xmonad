# coding=utf8

# Default time a notification is show, unless specified in notification
DEFAULT_NOTIFY_TIMEOUT = 5000 # milliseconds

# Maximum time a notification is allowed to show
MAX_NOTIFY_TIMEOUT = 10000 # milliseconds

# Maximum number of characters in a notification.
NOTIFICATION_MAX_LENGTH = 100 # number of characters

# Time between regular status updates
STATUS_UPDATE_INTERVAL = 5 # seconds

# Command to fetch status text from. We read from stdout.
# Each argument must be an element in the array
# os must be imported to use os.getenv
import os
STATUS_COMMAND = ['/bin/sh', '%s/.statusline.sh' % os.getenv('HOME')]

# Always show text from STATUS_COMMAND? If false, only show notifications
USE_STATUSTEXT=False

# Put incoming notifications in a queue, so each one is shown.
# If false, the most recent notification is shown directly.
QUEUE_NOTIFICATIONS=True

# update_text(text) is called when the status text should be updated
# If there is a pending notification to be formatted, it is appended as
# the final argument to the STATUS_COMMAND, e.g. as $1 in default shellscript

import codecs
import HTMLParser
import subprocess
import time
from tempfile import NamedTemporaryFile

fifo_dir = os.path.expanduser('~/.config/statnot')
fifo     = os.path.join(fifo_dir, 'notification.pipe')
writer   = codecs.getwriter('utf8')
h        = HTMLParser.HTMLParser()

if not os.path.exists(fifo_dir):
    os.makedirs(fifo_dir)
if not os.path.exists(fifo):
    os.mkfifo(fifo)

pipe = open(fifo, 'w', 0)

def update_text(text, icon=None):
    #with open(fifo, 'w', 0) as pipe:
        utf8_pipe = writer(pipe)
        if text != '':
            first_line = h.unescape(text.splitlines()[0])
            # scroll(utf8_pipe, first_line)
            blink(utf8_pipe, first_line)
            # write_message(utf8_pipe, first_line)
        else:
            utf8_pipe.write('\n')

def scroll(writer, line, frame_step=0.005):
    for i in range(0, len(line) - 1):
        write_message(writer, line[:i])
        time.sleep(frame_step)
    write_message(writer, line)

# This method of setting colors does not seem to work.
def blink(writer, line):
    # black on cyan
    write_message(writer, '<fc=#002b36,#2aa198>' + line + '</fc>')
    time.sleep(0.5)
    write_message(writer, line)

def write_message(writer, line):
    writer.write('> ')
    writer.write(line)
    writer.write('\n')
