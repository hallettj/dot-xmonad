class { 'apt': }

ensure_packages([
  'haskell-platform',
  'libxinerama-dev',
  'libxrandr-dev',
  'libxss-dev',
  'libxft2-dev',
  'pkg-config',
  'xmonad',
  'xmobar',
  'alsa-utils',
  'dzen2',
  'rxvt-unicode',
  'stalonetray',
  'suckless-tools',
  'xbacklight',
  'xscreensaver'
])

exec { 'compile-xmonad-config':
  command     => "xmonad --recompile",
  cwd         => $home,
  user        => $user,
  provider    => shell,
  environment => [ "HOME=${home}" ]
}

exec { 'cabal-update':
  command     => "cabal update",
  cwd         => $home,
  user        => $user,
  provider    => shell,
  environment => [ "HOME=${home}" ]
}

exec { 'install-xmonad':
  command     => "cabal install xmonad xmonad-contrib xmonad-extras yeganesh",
  cwd         => $home,
  user        => $user,
  provider    => shell,
  environment => [ "HOME=${home}" ]
}

Package['haskell-platform'] -> Exec['cabal-update']

Exec['cabal-update']        -> Exec['install-xmonad']
Package['libxinerama-dev']  -> Exec['install-xmonad']
Package['libxrandr-dev']    -> Exec['install-xmonad']
Package['libxss-dev']       -> Exec['install-xmonad']
Package['libxft2-dev']      -> Exec['install-xmonad']
Package['pkg-config']       -> Exec['install-xmonad']

Package['xmonad']           -> Exec['compile-xmonad-config']
Exec['install-xmonad']      -> Exec['compile-xmonad-config']
