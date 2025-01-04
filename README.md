# xframes-haskell

## Status

**Unstable**: It works, but I've yet to work out it occasionally terminates ahead of time. My suspicion is that the function pointers somehow get garbage collected too soon.

## Instructions

### Install Haskell

#### Windows

I followed the instructions found on this page: https://www.haskell.org/ghcup/

In particular, I ran the command in a PowerShell and installed GHCUP along with the MSYS2 toolchain, then GHC 9.4.8 and cabal.

### Install dependencies

`cabal install --lib aeson --package-env .`

### Running the application

![image](https://github.com/user-attachments/assets/cf3f7951-05d6-4864-8bad-ae03ae561ee9)


- `cabal build`
- `cabal run`

## Screenshots

Windows 11
