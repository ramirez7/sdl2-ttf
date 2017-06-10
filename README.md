# sdl2-ttf

[![Build Status](https://travis-ci.org/sbidin/sdl2-ttf.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-ttf)

#### Haskell bindings to SDL2_ttf

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_ttf library. Please report an issue if you encounter a bug
or feel that something is missing.

##### Install

```bash
git clone git@github.com:sbidin/sdl2-ttf.git
cd sdl2-ttf
cabal install
```

##### Documentation

You can find slightly outdated documentation [here](https://bidin.eu/docs/sdl2-ttf).

The
[original SDL2_ttf documentation](http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It loads a given font
and uses it to display text in various ways. You can find it in the `example`
directory.

```bash
cd sdl2-ttf
cabal run path/to/some/font.type
```
