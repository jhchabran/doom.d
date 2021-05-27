# My Emacs configuration

It uses an Emacs distribution named [Doom](https://github.com/hlissner/doom).

I use Emacs 28.x builds from: https://github.com/jimeh/build-emacs-for-macos, with `nativecomp` enabled.
Because Emacs on OSX can be a bit slow when rendering things, this config is actively avoiding stuff that slows it down.

## Notable details

- Leader is on `SPC`, local leader is `SPC SPC`
- I use [a colemak keyboard layout](https://en.wikipedia.org/wiki/Colemak) and certain settings may reflect that.
  - for example, `window-switch` module uses Colemak home row keys as shortcuts rather than the QWERTY ones.
- `SPC w` is bound to save the current buffer because I have this hardwired from previous customization in my Vim config
- `SPC p $` asks for a name them open a vterm named `*term-NAME*`
  - I uses this to launch servers and others
- Customized theme ([`doom-monarized-dark`](https://github.com/jhchabran/doom-monarized-theme)) based on Solarized but reduces the amount of colors being used for code syntax highlighting, while keeping the others for everything else (diffs, org-mode, etc ...).

## Config structure

- `config.el`
  - global settings, evil, bindings that aren't related to a given language.
- `+*.el`
  - settings grouped by a given topic, all loaded at the end of `config.el`
