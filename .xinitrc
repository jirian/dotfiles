#!/bin/sh

xrdb .Xresources
remaps &

exec xmonad
