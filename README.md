# sdl2-ttf

#### Haskell bindings to SDL_ttf

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL_ttf library. Please report an issue if you encounter a bug
or feel that something is missing.

##### Install

This library depends on and is meant to be used with the `new-api` branch of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2). After installing
haskell-game/sdl2, you can install sdl2-ttf manually from source:

```bash
git clone git@github.com:sbidin/sdl2-ttf.git
cd sdl2-ttf
cabal install
```

##### Documentation

You can find the documentation [here](https://bidin.eu/docs/sdl2-ttf). The
[original SDL_ttf documentation](http://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It loads a given font
and uses it to display text in various ways. You can find it in the `example`
directory.

```bash
cd sdl2-ttf
cabal run path/to/some/font.type
```
