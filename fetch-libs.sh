#!/usr/bin/env bash
SDL2_VER="2.0.10"
SDL2_WIN_FNAME="SDL2-$SDL2_VER-win32-x64.zip"
SDL2_URL_BASE="https://www.libsdl.org/release/"
GORILLA_WIN_URL="https://github.com/Elronnd/gorilla-audio/releases/download/v0.3.3/gorilla.lib"
GORILLA_LIN_URL="https://github.com/Elronnd/gorilla-audio/releases/download/v0.3.3/libgorilla.a"
ASSIMP_WIN_URL="https://github.com/assimp/assimp/releases/download/v4.1.0/assimp-sdk-4.1.0-setup.exe"

S7_FNAME="s7.tar.gz"
S7_URL="ftp://ccrma-ftp.stanford.edu/pub/Lisp/$S7_FNAME"

fetch-win() {
	mkdir lib/win
	pushd lib/win

	# SDL
	wget $SDL2_URL_BASE$SDL2_WIN_FNAME
	7z -aoa x $SDL2_WIN_FNAME
	rm -f $SDL2_WIN_FNAME
	mv README-SDL.txt .. #it asks me to distribute it.  I'm not mean!

	wget $ASSIMP_WIN_URL
	innoextract --silent --include app/bin/x64/assimp-vc140-mt.dll assimp-sdk-4.1.0-setup.exe
	mv app/bin/x64/assimp-vc140-mt.dll assimp.dll
	rm -rf assimp-sdk-4.1.0-setup.exe app

	wget $GORILLA_WIN_URL

	popd
}
fetch-linux() {
	mkdir lib/linux
	pushd lib/linux

	wget $GORILLA_LIN_URL -O gorilla.lib

	popd
}
fetch-src() {
	pushd deps

	rm -f s7.c s7.h
	wget $S7_URL
	tar xf $S7_FNAME s7/s7.c s7/s7.h
	mv s7/s7.c s7/s7.h .
	rmdir s7
	rm -f $S7_FNAME

	sed -i 's/typedef double s7_double/typedef float s7_double/g' s7.h

	popd
}

rm -rf lib
mkdir lib
fetch-win
fetch-linux
fetch-src
