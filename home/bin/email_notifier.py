#!/usr/bin/env python2
# -*- coding: utf-8; mode: python -*-
#
# Original script by Nicolas Estibals
# https://bbs.archlinux.org/viewtopic.php?pid=1154358

import pyinotify
import pynotify
import re
from os.path import abspath
from os.path import dirname
from os.path import expanduser
from os.path import join
from mailbox import MaildirMessage
from email.header import decode_header
from gtk.gdk import pixbuf_new_from_file

# Getting the path of all the boxes
boxes = []
with open(expanduser("~/.mutt/mailboxes"), 'r') as fd:
    boxes = [expanduser(b[1:-1]) for b in fd.readline()[10:-1].split(' ')]

# Get mail folder setting
folder = ""
with open(expanduser("~/.muttrc"), 'r') as f:
    pattern = r"^\s*set\s+folder\s*=\s*['\"]?(.+?)['\"]?\s*($|#)"
    for line in f:
        m = re.search(pattern, line)
        if m:
            folder = dirname(expanduser(m.group(1)))

pynotify.init('email_notifier.py')

# Handling a new mail
icon = expanduser("~/.xmonad/dzen-icons/mail.xbm")
icon_uri = 'file://' + abspath(icon)
dec_header = lambda h : ' '.join(unicode(s, e if bool(e) else 'ascii') for s, e in decode_header(h))
def newfile(event):
    fd = open(event.pathname, 'r')
    mail = MaildirMessage(message=fd)
    From = "From: " + dec_header(mail['From'])
    Subject = dec_header(mail['Subject'])
    n = pynotify.Notification("New mail in "+'/'.join(event.path.split('/')[-3:-1]),
            From+ " :: "+ Subject,
            icon_uri)
    fd.close()
    n.set_timeout(12000)
    n.show()

wm = pyinotify.WatchManager()
notifier = pyinotify.Notifier(wm, newfile)

for box in boxes:
    maildir = join(folder, box.replace('+', ''), 'new')
    wm.add_watch(maildir, pyinotify.IN_CREATE | pyinotify.IN_MOVED_TO)

notifier.loop()
