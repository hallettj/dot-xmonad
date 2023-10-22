# dot-xmonad

**This is outdated** - my desktop environment configuration has moved to https://github.com/hallettj/home.nix/tree/main/home-manager/features/gnome

This is my xmonad configuration.  Some of it is specific to xmonad, and
some is general graphical environment configuration.  The configuration
is managed with [homeshick][], which creates symlinks from the
appropriate locations in my home directory.  Installing dependencies,
compiling, and other setup tasks are handled using [Ansible][], which
runs the steps listed in `init.yml`.

[homeshick]: https://github.com/andsens/homeshick
[Ansible]: http://www.ansibleworks.com/
