# rofi-extras

These are some personal programs that use the
[rofi](https://github.com/davatorium/rofi) interface.

## Rofi-Bitwarden

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

## Rofi-Devices

This is a manual mounting helper for removable drives, MTP devices, and fstab
entries. It will transparently handle mountpoint creation/destruction.

### Usage

Launch the device manager:

``` sh
rofi-dev
```

Select a device to mount/unmount it. Asterisks indicate that the device is
mounted and that selecting it will unmount it. Removable devices will be mounted
at the default for `usdisksctl` (usually `/run/media/USER`) and everything else
will be mounted in `/media/USER` unless the `-d` option is specified (see
`rofi-dev -h`). In either case the directory should be owned by the user.

Fstab entries should specify the `users` mount option (not `user`) to enable
non-root users to mount and unmount. The mountpoint should either be in
`/media/USER` (or whatever is specified by `-d`) or already exist.

### Dependencies
- udisks2: removable drive mounting
- sshfs: mounting network devices in fstab over ssh
- cifs-utils: mounting network devices in fstab using CIFS/Samba
- [jmtpfs](https://github.com/JasonFerrara/jmtpfs): mounting MTP devices
- libnotify: desktop notifications
