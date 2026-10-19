#!/usr/bin/env bash
rm -rf dist
mkdir dist
zip -9 dist/rocketrush.love \
	main.lua \
	scripts/*.lua \
	fonts/*.ttf \
	audio/*.wav \
	sprites/*.png \
	palettes/*.hex \
	libs/*.lua 
