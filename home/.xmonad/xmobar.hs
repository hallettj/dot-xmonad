-- xmobar config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
--
-- customized by Jesse Hallett
-- http://github.com/hallettj/config_files

-- This is setup for 1920x1080 monitors
Config {
    font = "xft:monospace:size=9:antialias=true"
    bgColor = "#002b36",
    fgColor = "#93a1a1",
    position = Static { xpos = 0, ypos = 0, width = 1808, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Battery [ "-t", "AC: <acstatus>   Batt: <left>% / <timeleft>"
                    , "-h", "#859900"
                    , "-n", "#859900"
                    , "-l", "#dc322f"
                    , "--"
                    , "-O", "charging"
                    , "-i", "connected"
                    , "-o", "off"
                    ] 60,
        Run Date "%a %b %_d %l:%M" "date" 10,
        Run PipeReader "/home/jesse/.config/statnot/notification.pipe" "notifications",
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ <fc=#2aa198>%notifications%</fc>   %battery%   <fc=#93a1a1>%date%</fc>"
}
