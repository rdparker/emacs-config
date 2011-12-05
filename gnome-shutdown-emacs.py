#!/usr/bin/env python

# Taken from
# http://www.linuxquestions.org/questions/linux-desktop-74/gnome-run-script-on-logout-724453/#post3560301

#Author: Seamus Phelan

#This program runs a custom command/script just before gnome shuts
#down.  This is done the same way that gedit does it (listening for
#the 'save-yourself' event).  This is different to placing scipts
#in /etc/rc#.d/ as the script will be run before gnome exits.
#If the custom script/command fails with a non-zero return code, a
#popup dialog box will appear offering the chance to cancel logout
#
#Usage: 1 - change the command in the 'subprocess.call' in
#           function 'session_save_yourself' below to be what ever
#           you want to run at logout.
#       2 - Run this program at every gnome login (add via menu System
#           -> Preferences -> Session)
#
#

import sys
import subprocess
import datetime

import gnome
import gnome.ui
import gtk


class Namespace: pass
ns = Namespace()
ns.dialog = None


def main():
    prog = gnome.init ("gnome_save_yourself", "1.0", gnome.libgnome_module_info_get(), sys.argv, [])
    client = gnome.ui.master_client()
    #set up call back for when 'logout'/'Shutdown' button pressed
    client.connect("save-yourself", session_save_yourself)
    client.connect("shutdown-cancelled", shutdown_cancelled)


def session_save_yourself( *args):
    #Lets try to unmount all truecrypt volumes
    retcode = subprocess.call("emacsclient -e '(shutdown-emacs-server)'",
                              shell=True)
    if retcode != 0:
        #command failed
        show_error_dialog()
    return True

def shutdown_cancelled( *args):
    if ns.dialog != None:
        ns.dialog.destroy()
    return True


def show_error_dialog():
    ns.dialog = gtk.Dialog("There was a problem running your pre-shutdown script",
                           None,
                           gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                           ("There was a problem running your pre-shutdown script - continue logout", gtk.RESPONSE_ACCEPT))
    if ns.test_mode == True:
        response = ns.dialog.run()
        ns.dialog.destroy()
    else:
        #when in shutdown mode gnome will only allow you to open a window using master_client().save_any_dialog()
        #It also adds the 'Cancel logout' button
        gnome.ui.master_client().save_any_dialog(ns.dialog)



#Find out if we are in test mode???
if len(sys.argv) >=2 and sys.argv[1] == "test":
    ns.test_mode = True
else:
    ns.test_mode = False

if ns.test_mode == True:
    main()
    session_save_yourself()
else:
    main()
    gtk.main()
