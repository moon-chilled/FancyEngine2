#!/usr/bin/env bash
SDL2_VER="2.0.9"
SDL2_FNAME="SDL2-$SDL2_VER-win32-x64.zip"
SDL2_32BIT_FNAME="SDL2-$SDL2_VER-win32-x86.zip"
SDL2_URL_BASE="https://www.libsdl.org/release/"

SDL2_MIX_VER="2.0.4"
SDL2_MIX_FNAME="SDL2_mixer-$SDL2_MIX_VER-win32-x64.zip"
SDL2_MIX_32BIT_FNAME="SDL2_mixer-$SDL2_MIX_VER-win32-x86.zip"
SDL2_MIX_URL_BASE="https://www.libsdl.org/projects/SDL_mixer/release/"


fetch-win() {
	mkdir lib/win
	pushd lib/win

	# SDL
	wget "$SDL2_URL_BASE$SDL2_FNAME"
	7z -aoa x $SDL2_FNAME
	rm -f $SDL2_FNAME
	mv README-SDL.txt .. #it asks me to distribute it.  I'm not mean!



	# SDL mixer
	wget "$SDL2_MIX_URL_BASE$SDL2_MIX_FNAME"
	7z -aoa x $SDL2_MIX_FNAME
	rm -f $SDL2_MIX_FNAME
	# mp3 is patent-encumbered and SHIT, modplug is for music tracker formats like midi, I think?  Not very useful for a game
	# TXT files are various readmes and licenses, which are already covered elsewhere (toplevel 'licenses' dir
	rm -f *.txt libmpg123-0.dll libmodplug-1.dll

	popd
}

fetch-win32() {
	mkdir lib/win32
	pushd lib/win32
	wget $SDL2_URL_BASE$SDL2_32BIT_FNAME
	7z -aoa x $SDL2_32BIT_FNAME
	rm -f $SDL2_32BIT_FNAME

	wget "$SDL2_MIX_URL_BASE$SDL2_MIX_32BIT_FNAME"
	7z -aoa x $SDL2_MIX_32BIT_FNAME
	rm -f $SDL2_MIX_32BIT_FNAME *.txt libmpg123-0.dll libmodplug-1.dll

	popd
}

rm -rf lib
mkdir lib
fetch-win
fetch-win32
