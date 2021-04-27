sarsi
=====

[![View on hackage](https://img.shields.io/hackage/v/sarsi.svg)](http://hackage.haskell.org/package/sarsi)
[![Join the chat at https://gitter.im/aloiscochard/sarsi](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/aloiscochard/sarsi)

A universal quickfix toolkit and its protocol.

### Quick fixing

It's fixing inside your text editor/IDE the warnings or errors returned by the compiler/interpreter.

# Philosophy

The core of *[sarsi](https://en.wiktionary.org/wiki/sarcio#Latin)* is a simple binary protocol to exchange quickfix messages. The tooling built around it approach the operating system as an integrated development environment following the [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy). On one side we produce messages from our build tools or interpreters, and on the other we want them to be consumed as soon as possible by our favorite text editors.

# Modules

#### Producers

 - `srs` - Command line wrapper for build tools (Rust, Haskell)
 - `sarsi` - Generic utility processing stdin (all supported languages)
 - `sarsi-sbt` - SBT specific wrapper for the Scala programming language

#### Consumers

 - `sarsi-nvim` - Neovim RPC plugin for realtime feedback and multilines error display
 - `sarsi-vi` - Quickfix file generator which use a vi compatible error format

# Install

#### Hackage

*Sarsi* is published on Hackage and can be installed using `cabal`.

    cabal install sarsi

#### Source

Alternatively, it can be installed from source using `stack`.

    git clone https://github.com/aloiscochard/sarsi.git
    cd sarsi
    stack install

# Usage

By default, when a consumer/producer start it will use a Unix pipe with a name generated according the directory in which it was launched.

It means you have to start consumers/producers from the same directory for them to be connected.

## Producers

*Generic*
It does allow you to run an arbitrary command and get it's output transparently feeded into all active consumers.

*Tailored*
It is specialized for an interactive command and will forward the arguments you pass to that specific program.

### Generic

Languages: Haskell (cabal, stack, ghc), Rust (cargo).

#### srs

The `srs` command line wrapper is *generic* and can be used with any build tool.

    srs cabal build

It works nicely with [velox](https://github.com/aloiscochard/velox/), [entr](http://entrproject.org/), `inotifywait`, or any other hook mechanism you would like to use to trigger the build automatically when the code change.

    vlx srs cargo build

#### sarsi

In some special case you might prefer using the command tool `sarsi` which process stdin and can be used in a pipeline.

    cargo build |& sarsi

### Tailored

Languages: Scala (sbt).

#### Scala (SBT)

You can use this *tailored* wrapper in place of your `sbt` command, interactively or not (you should surely prefer the former for performance reasons).

    sarsi-sbt

It will behind the scene call the `sbt` program available in the path and transparently forward the quick fixes produced to the available consumers.

## Consumers

### Neovim

Once `sarsi` installed, add the following line in your `init.vim`.

    let g:sarsi = jobstart(['sarsi-nvim'], {'rpc': v:true})

You might also want to add key bindings for the core functionatilites as shown below.

    noremap <silent> <C-F> :cfirst<CR>
    noremap <silent> <C-J> :cnext<CR>
    noremap <silent> <C-K> :cprevious<CR>

### Vi/Vim

First, you have to start the consumer in a dedicated terminal using the `sarsi-vi` command.

The process will continuously maintain a quickfix file located at `$(sarsi --topic).vi` which you can open in the editor using ```:cfile `sarsi`.vi```.

#### Error fomat

The output format used by `sarsi-vi` to generate the  `$(sarsi --topic).vi` file is backward compatible with the default one used by `vi`/`vim`.

The missing part is the level (warning/error), in order to have it taken into account you should add the following in your initialization script.

`set efm=%f:%l:%c:%t\ %m`
