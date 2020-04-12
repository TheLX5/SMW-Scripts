SNES Toy Story's level 10 BG for SMW
by lx5

This package recreates the layer 2 BG seen in Toy Story's level 10

----------------------------------------

Notes:
- This resource requires a LOT of processing in order to work fine. You may encounter lag if you place too many sprites on screen on non SA-1 enhanced ROMs.
- snes9x 1.60 fails to draw the diagonal metal beams on SA-1 enhanced ROMs. Please use a more accurate emulator. More info: https://twitter.com/TheLX5/status/1249161864178741249
- The BG GFX requires a noticeable amount of data
- Non-SA1 enhanced ROMs may require an external patch in order to not mess the overworld. https://www.smwcentral.net/?p=section&a=details&id=19580
- It may generate black bars at the top of the screen due to the NMI routine uploading a 64x64 image every frame.

----------------------------------------

How to install:
1) Open the sample level in your ROM to access the BG and palette data and save them to any level you'd like
2) Import the map16 data for the BG
3) Open the included uberasm script, there are things in there you may want to change in there.
4) Insert the included uberasm script into your desired level(s)

----------------------------------------

Patreon: https://www.patreon.com/lx5
Twitter: https://twitter.com/TheLX5
Youtube: https://www.youtube.com/c/TheLX5
GitHub: https://github.com/TheLX5
SMWC ID: 12344
