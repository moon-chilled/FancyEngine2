#!/usr/bin/env bash
SDL2_VER="2.0.10"
SDL2_WIN_FNAME="SDL2-$SDL2_VER-win32-x64.zip"
SDL2_URL_BASE="https://www.libsdl.org/release/"
GORILLA_WIN_URL="https://github.com/moon-chilled/gorilla-audio/releases/download/v0.3.3/gorilla.lib"
GORILLA_LIN_URL="https://github.com/moon-chilled/gorilla-audio/releases/download/v0.3.3/libgorilla.a"
GORILLA_FBSD_URL="https://github.com/moon-chilled/gorilla-audio/releases/download/v0.3.3/libgorilla-freebsd.a"
ASSIMP_WIN_URL="https://github.com/assimp/assimp/releases/download/v4.1.0/assimp-sdk-4.1.0-setup.exe"
MOONJIT_WIN_URL="https://em.slashem.me/~elronnd/libluajit-5.1.dll.a"
MOONJIT_LIN_URL="https://em.slashem.me/~elronnd/libluajit-5.1.a"
MOONJIT_FBSD_URL="https://em.slashem.me/~elronnd/libluajit-5.1-freebsd.a"

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
	wget $MOONJIT_WIN_URL -O libluajit-5.1.a

	popd
}
fetch-linux() {
	mkdir lib/linux
	pushd lib/linux

	wget $GORILLA_LIN_URL -O gorilla.lib
	wget $MOONJIT_LIN_URL

	popd
}
fetch-freebsd() {
	mkdir lib/freebsd
	pushd lib/freebsd

	wget $GORILLA_FBSD_URL -O gorilla.lib
	wget $MOONJIT_FBSD_URL -O libluajit-5.1.a

	popd
}
fetch-src() {
	pushd deps

	rm -f s7.c s7.h
	wget $S7_URL
	tar xf $S7_FNAME s7/s7.c s7/s7.h
	mv s7/s7.c .

	# there is code in fe2 that RELIES on s7_double being float-sized,
	# so think twice before changing this
	# s7 relies on s7_int being pointer-sized, which is fine--although it's
	# currently typedefed to int64_t, which should probably be checked on
	# 32-bit platforms (if I care to support them)
	# todo - make s7 use float trig functions instead of double ones?
	sed 's/typedef double s7_double/typedef float s7_double/g' < s7/s7.h > s7.h

	rm -rf s7
	rm -f $S7_FNAME

	popd
}

rm -rf lib
mkdir lib
fetch-win&
fetch-linux&
fetch-freebsd&
fetch-src&

wait
