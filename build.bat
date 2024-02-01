@echo off


set "CC=tcc"
set "CCFLAGS=-O0 -ggdb -DDEBUG -Wall -Wextra -Wpedantic -Wno-missing-braces -IInclude -Wno-unused-local-typedefs"
set "LDFLAGS= -flto "
set "LIBS= "

if "clean"=="%1" (
    if exist bin\ rmdir /q /s bin
    echo ---------------------------
    echo   removed build artifacts
    echo ---------------------------
) else if "cl"=="%1" (
    if "%VisualStudioVersion%"=="" call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64

    if not exist bin\ mkdir bin
    pushd bin
    cl /Zi /O2 /IP:\C\m68k\Include ..\build.c /Femain.exe
    popd 

    echo ---------------------------
    echo       build finished
    echo ---------------------------
) else (
    if not exist bin\ mkdir bin
    %CC% %LDFLAGS% %CCFLAGS% build.c -o bin\main.exe %LIBS%

    echo ---------------------------
    echo       build finished
    echo ---------------------------
)

