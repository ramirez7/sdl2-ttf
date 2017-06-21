# sdl2-ttf

[![Build Status](https://travis-ci.org/haskell-game/sdl2-ttf.svg?branch=master)](https://travis-ci.org/haskell-game/sdl2-ttf)

#### Haskell bindings to SDL2_ttf

Haskell bindings for the True Type Font library for SDL.

- libsdl <https://www.libsdl.org>
- sdl2-ttf <https://www.libsdl.org/projects/SDL_ttf/>

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_ttf library. Please report an issue if you encounter a bug
or feel that something is missing.

##### Install

```bash
git clone git@github.com:haskell-game/sdl2-ttf.git
cd sdl2-ttf
cabal install
```

##### Example

A small example executable is included with the library. It loads a given font
and uses it to display text in various ways. You can find it in the `example`
directory.

```bash
cd sdl2-ttf
cabal configure -fexample
cabal run path/to/some/font.type
```
