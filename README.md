# Hax
Bullet hell game in haskell.


## Cool Screenshots!
![Bullets everywhere!](https://media.discordapp.net/attachments/251783968515555330/500567570533974026/unknown.png?width=423&height=590)


## Building
I've used `stack` for this project, but building with just `cabal` should
be similar.

The game depends on SDL2 for graphics, so the development libraries
for that need to be installed:

### Linux (Ubuntu)
There should be similar packages on other distributions.

```
sudo apt install libsdl2-dev
stack build
```

## OSX
(Note: not tested, submit an issue if this doesn't work!)

```
brew install sdl2
stack build
```

## Windows
There may be some problems with the SDL2 libs on windows, they're finicky
for whatever reason.

The best way to get them with `stack` is to use its built in mysys
installation:

```
stack exec -- pacman -Syu
stack exec -- pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-SDL2
stack build
```

## Running
After a successful `stack build`, all you need is:

```
stack exec hax-exe
```
