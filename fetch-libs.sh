#!/usr/bin/env bash
SDL2_VER="2.0.9"
SDL2_FNAME="SDL2-$SDL2_VER-win32-x64.zip"
SDL2_32BIT_FNAME="SDL2-$SDL2_VER-win32-x86.zip"
SDL2_URL_BASE="https://www.libsdl.org/release/"

fetch-win() {
	mkdir lib/win
	pushd lib/win

	# SDL
	wget "$SDL2_URL_BASE$SDL2_FNAME"
	7z -aoa x $SDL2_FNAME
	rm -f $SDL2_FNAME
	mv README-SDL.txt .. #it asks me to distribute it.  I'm not mean!

	popd
}

fetch-win32() {
	mkdir lib/win32
	pushd lib/win32
	wget $SDL2_URL_BASE$SDL2_32BIT_FNAME
	7z -aoa x $SDL2_32BIT_FNAME
	rm -f $SDL2_32BIT_FNAME README-SDL.txt # don't need this one because we got the 64-bit readme

	popd
}

rm -rf lib
mkdir lib
fetch-win
fetch-win32
