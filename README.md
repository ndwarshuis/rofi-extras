# rofi-extras

These are some personal programs that use the
[rofi](https://github.com/davatorium/rofi) interface.

## Installation

Clone this repo and run the following in the repo root.

Install packages needed for building:

```
pacman -S --needed - < make_pkgs
```

Build and install (choose individual targets as needed):

```
stack install
```

See individual sections for other dependencies to install.

## Bitwarden (rofi-bw)

[Bitwarden](https://bitwarden.com/) is an open-source password management server
and this program functions as a client. Unlike many other similar clients, this
only presents a simple lookup interface for usernames and passwords in the
vault; editing and creating entries is not in the intendend scope.

### Usage

This operates in a client/daemon fashion (locally, not with respect to the
bitwarden server). The daemon holds the session key and the client presents the
interface for username/password lookup.

Make sure `bw-cli` is properly configured to contact your bitwarden server using
`bw config server url.of.your.server`.

Start the daemon in `xinitrc` or similar. This will start the deamon with a
60 second timeout for the session key (after which you would need to enter a new
password):

``` sh
rofi-bw -d 60
```

Then launch the client to browse through the vault.

``` sh
rofi-bw -c
```

Any options after `-c` will be passed to rofi.

### Dependencies
- [bitwarden-cli](https://github.com/bitwarden/cli)
- dbus
- libnotify: desktop notifications

## Device Mounting (rofi-dev)

This is a manual mounting helper for removable drives, MTP devices, and fstab
entries. It will transparently handle mountpoint creation/destruction.

### Usage

Launch the device manager:

``` sh
rofi-dev
```

Any options after `--` will be passed to rofi.

Select a device to mount/unmount it. Asterisks indicate that the device is
mounted and that selecting it will unmount it. Removable devices will be mounted
at the default for `usdisksctl` (usually `/run/media/USER`) and everything else
will be mounted in `/media/USER` unless the `-d` option is specified (see
`rofi-dev -h`). In the latter case the directory should be owned by the user.

Fstab entries should specify the `users` mount option (not `user`) to enable
non-root users to mount and unmount. The mountpoint should either be in
`/media/USER` (or whatever is specified by `-d`) or already exist.

### Credentials

For fstab entries, `rofi-dev` will attempt to obtain a password if no options
are supplied in the mount options (eg keyfiles for sshfs or credential files for
cifs).

To specifify that `/media/USER/foo` should use `secret-tool` to find its
password, specify the `-s` option. This would lookup a password for the entry
whose `username` is `bar` and `hostname` is `example.com`:

### Veracrypt

This tool can mount veracrypt vaults...with some hacky effort. Since veracrypt
works at the block device level, it needs root permissions to mount a volume
(which actually involves mounting several devices). The easiest way to make sure
this works is to give veracrypt sudo access like so:

```
<user> ALL=(root) NOPASSWD: /usr/bin/veracrypt,/usr/bin/uptime
```

No idea why `uptime` is also needed for this.

``` sh
rofi-dev -s '/media/USER/foo:username=bar,hostname=example.com'
```

To simply prompt for a password, use the `-p` option:

``` sh
rofi-dev -p '/media/USER/foo'
```

### Dependencies
- udisks2: removable drive mounting
- sshfs: mounting network devices in fstab over ssh
- cifs-utils: mounting network devices in fstab using CIFS/Samba
- veracrypt: to mount veracrypt vaults
- [jmtpfs](https://github.com/JasonFerrara/jmtpfs): mounting MTP devices
- libnotify: desktop notifications
- libsecret: password lookup with `secret-tool`
- libnotify

## Autorandr (rofi-autorandr)

This allows selection of the
[autorandr](https://github.com/phillipberndt/autorandr) configuration via a rofi
menu.

### Dependencies

- autorandr

## Bluetooth (rofi-bw)

This presents a nice menu to select the current bluetooth device.

### Dependencies

- bluez (which should provide the dbus interface for this to work)

## ExpressVPN (rofi-evpn)

This presents a menu to select the current ExpressVPN gateway.

### Dependencies

- expressvpn (from AUR)
- libnotify

## Pinentry (pinentry-rofi)

Analogous to the default [pinentry](https://github.com/gpg/pinentry) prompts,
this presents a rofi prompt for a password with the GPG keyring is unlocked.

Requires the following in `gpg-agent.conf`:

```
pinentry-program /path/to/pinentry-rofi
```

Unlike the other pinentry programs, this one can integrate with bitwarden (via
the above client) by retrieving the password for the gpg keyring if it is stored
in bitwarden. This requires a yaml configuration in the gpg home directoring as
such:

```
bitwarden-name: <name of GPG bitwarden entry>
```

### Dependencies

- rofi-bw (see above): bitwarden integration

## Putting Rofi on the correct screen (current-output)

This is a total hack...actually it isn't because it's written in Haskell and not
bash.

The problem is that when used with xmonad, rofi doesn't place itself on the
"current" workspace since the concept of a "workspace" is weird and specific to
xmonad. The solution is to use this program to query `_NET_DESKTOP_VIEWPORT`
(which my xmonad config sets) and use this determine the name of the active
workspace which can then be fed to rofi using the `-m` flag.

See comments of this binary for details.

### Dependencies

- X11



