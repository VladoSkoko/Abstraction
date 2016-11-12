# Abstraction README

# 1. Fetching resources

It is necessary to install the Haskell platform for Windows, which you can find at https://www.haskell.org/platform/ 

Under Command Prompt with administrator privileges perform the following commands:

cabal update
cabal install strings

From http://sourceforge.net/projects/wxhaskell/files/wxInstall/ fetch one of the installation packages and unpack the .zip file into a directory, for example C:\wxHaskell\. In that directory you'll find a file Install.bat, which you need to run. After that, add two directories to your PATH environment (if your install directory was C:\wxHaskell\, the exact paths are following):

C:\wxHaskell\DLLs
C:\wxHaskell\wxWidgets\lib\gcc_dl


# 2. Compilation

Program is compiled with the following command:

ghc package wx apstrakcija.hs

which generates apstrakcija.exe
