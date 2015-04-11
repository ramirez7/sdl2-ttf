# sdl2-ttf

#### Haskell bindings to SDL_ttf, both high and low-level.

This library depends on and is meant to be used with the `new-api` branch of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2). Currently you can
only install it manually from source, after installing sdl2 itself.

```bash
git clone git@github.com:sbidin/sdl2-ttf.git
cd sdl2-ttf
cabal install
```

A small example executable is included with the library. It loads a given font
and uses it to display some text. You can find it in the `example` directory.

```bash
cd sdl2-ttf
cabal run path/to/some/font.(ttf|fon)
```

Both sets of bindings should allow you to use any aspect of the original
SDL_ttf library. Please report an issue if you encounter a problem/bug
or feel that something is missing or should be added.

#### Currently missing:

High-level bindings to:

* TTF_Size(Text, UTF8, UNICODE)
* Rendering latin1 strings and individual glyphs.
