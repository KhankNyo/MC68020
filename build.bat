@echo off


set "CC=tcc"
set "CCFLAGS=-Og -g -Oz -Wall -Wextra -Wpedantic -Wno-missing-braces -IInclude"
set "LDFLAGS= "
set "LIBS= "

if "clean"=="%1" (
    if exist bin\ rmdir /q /s bin
    echo ---------------------------
    echo   removed build artifacts
    echo ---------------------------
) else (
    if not exist bin\ mkdir bin
    %CC% %LDFLAGS% %CCFLAGS% build.c -o bin\main.exe %LIBS%

    echo ---------------------------
    echo       build finished
    echo ---------------------------
)

