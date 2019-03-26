#!/usr/bin/env bash
SDL2_VER="2.0.9"
SDL2_WIN_FNAME="SDL2-$SDL2_VER-win32-x64.zip"
SDL2_URL_BASE="https://www.libsdl.org/release/"

fetch-win() {
	mkdir lib/win
	pushd lib/win

	# SDL
	wget "$SDL2_URL_BASE$SDL2_WIN_FNAME"
	7z -aoa x $SDL2_WIN_FNAME
	rm -f $SDL2_WIN_FNAME
	mv README-SDL.txt .. #it asks me to distribute it.  I'm not mean!

	popd
}

rm -rf lib
mkdir lib
fetch-win

#TODO: put gorilla and ecl libs somewhere they can be fetched and then fetch them
