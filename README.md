# My Emacs configuration

It uses an Emacs distribution named [Doom](https://github.com/hlissner/doom).

## Notable details

- Leader is on `SPC`, local leader is `SPC SPC`
- I use colemak and certain settings may reflect that
  - for example, `window-switch` module uses Colemak home row keys as shortcuts
- `SPC w` is bound to save the current buffer because I have this hardwired from previous customization in my Vim files
- `SPC p $` asks for a name them open a vterm named `*term-NAME*`
  - I uses this to launch servers and others
- Customized theme ([`doom-monarized-dark`](https://github.com/jhchabran/doom-monarized-theme)) based on Solarized but with reduces the amount of colors being used for code syntax highlighting, but keep the others for everything else (diffs, org-mode, etc ...).
