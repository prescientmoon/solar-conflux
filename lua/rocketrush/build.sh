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

love-web-builder dist/rocketrush.love dist/rocketrush-web --name "RockeTrush"

chmod -R +w dist

cd dist/rocketrush-web
zip -9 ../rocketrush-web.zip *
cd ../..
