---
date: 2020-10-26T10:30
title: The ABCs of vim
tags:
  - Vim
  - Essays
---

In a daze, I typed `abcdefghijklmnopqrstuvwxyz` into [[Vim]] to see what would
happen, and got `bcdefghijklmnopqrstuvwxyz`. Well, that was boring. Anyway,
it gave me the idea to write this short guide for what every letter of the
alphabet represents/does in vim, along with a few of the commands I use on a
(semi-)regular basis for each one. Enjoy!

### A is for After

- `a` - enter _Insert Mode_ _after_ the highlighted character.
- `A` - enter _Insert Mode_ _after_ the current line.

### B is for Back

- `b` - move _back_ one word.
- `B` - move _back_ one WORD.

### C is for Change

- `cw` - delete the following word and enters Insert Mode to _change_ it.
- `ctG` - _change_ all text _'till_ the next `G`.
- `C` - _change_ the remainder of the current line.

### D is for Delete

- - `diw` _delete_ the _inside_ of the current _word_.
- - `dd` _delete_ the current line.
- - `D` _delete_ the remainder of the current line.

### E is for End

- `e` - Navigate to the _end_ of the following word.
- `E` - Navigate to the _end_ of the following WORD.

### F is for Find

- `fG` - _Find_ the next occurrence of `G` on the current line.
- `FG` - _Find_ the previous occurrence of `G` on the current line.

### G is for Go

- `12g` - _Go_ to line 12 in the current file.
- `gg` - _Go_ to the top of the current file.
- `G` - _Go_ to the bottom of the current file.

### H is for Left

- `h` - move the cursor _left_

### I is for Insert or Inside

- `i` - enter _Insert_ Mode.
- `cip` - _change_ _inside_ the current _paragraph_.

### J is for Jerk

- `J` - _jerk_ the next line up, placing its contents at the end of the current line.
- `j` - move the cursor down.

### K is for Up

- `10k` - move the cursor _up_ 10 times.

### L is for Last

- `L` - jump to the _last_ visible line in the window.
- `l` - move the cursor _right_.

### M is for Mark

- `m'` - set a _mark_ that can be jumped back to using the command `''`.

### N is for Next

- `n` - Finds the _next_ occurrence of the current search term (argument to `/`
  or `?`)
- `N` - Finds the previous occurrence of the current search term (backwards _next_)

### O is for Over

- `o` - begin a new line _over_ (below) the current line.
- `O` - begin a new line _over_ (above) the current line.

### P is for Put or Paragraph

- `p` - _put_ the last yanked contents after the cursor.
- `P` - _put_ the last yanked contents before the cursor.
- `"mp` - _put_ the contents of register `m` at the current location.
- `dip` - _delete_ _inside_ the current _paragraph_.

### Q is for ... reQord? or Quit

- `qj` - _record_ a macro to register `j`. Hitting `q` again ends the macro. It
  can be executed later with the command `@j`.
- `:q` - _quit_ vim

### R is for Replace

- `rw` - _replace_ the highlighted character with `w`
- `R` - Enter _Replace_ Mode. Type to _replace_ characters until finished. Press
  Esc to quit _Replace_ Mode.

### S is for Supplant or Surround\*

- `s` - _supplant_ the highlighted character (delete it and enter Insert Mode.)
- `S` - _supplant_ the current line.
- `cs"(` - _change_ _surrounding_ quotation marks to parentheses.\*
- `ysw"` - _surround_ the current _word_ with parentheses.\*
- `dsw` - _delete_ the _surroundings_ of the current _word_.\*

\* Must have `vim-surround` installed.

### T is for 'Till

- `dt(` - _Delete_ _'till_ the next open parenthesis.

(`t` is just like `f`, but stops at the character before the occurrence)

### U is for Undo

- `u` - _undo_ the last change.
- `10u` - _undo_ the last 10 changes.

### V is for Visual

- `v` - enter _Visual_ mode.
- `V` - enter _Visual_ Line mode.
- `<CTRL>v` - enter _Visual_ Block mode.

### W is for Word or Write

- `daw` - _delete_ _around_ the highlighted _word_.
- `:w` - _write_ the current file to disk.

### X is for eXtract

- `x` - _extract_ (delete without mode change) the character under the cursor.
- `X` - _extract_ the character before the cursor.

### Y is for Yank

- `3yw` - _yank_ (insert into the unnamed register).the next 3 _words_
- `yy` - _yank_ the current line.
- `Y` - _yank_ the rest of the current line.

### Z is for (looks like) Fold

- `zf}` - _fold_ the rest of the current paragraph.
- `za` - Toggle the _fold_ (_fold_ or _unfold_) underneath the cursor.
