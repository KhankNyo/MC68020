@echo off


set "CC=gcc"
set "CCFLAGS=-O0 -ggdb -Wall -Wextra -Wpedantic -Wno-missing-braces -IInclude"
set "LDFLAGS= "
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
    cl /DEBUG /Zi /I"Include"  main.exe ..\build.c
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

