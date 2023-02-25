# ADVM
Audio Driver for (Dreamcast) VMU
<br><p align="left"><img src="https://github.com/jvsTSX/ADVM/blob/main/soundtest_dot_s_screen.png?raw=true" alt="ADVM" width="240" height="160"/>

Port and cut-down version of ADPM for the Dreamcast VMU, written entirely in LC86000 assembly language. 
Use Waterbear in order to assemble the source (https://github.com/wtetzner/waterbear), 
Example song also comes in binary form, use Elysian EVMU to run the binary if you want to take a quick look.

Notice SFR.I is not made by me, it's originally at https://github.com/jahan-addison/snake

## How to assemble the source?

Clone this repo (or just download zip why not) and run waterbear on the folder, you need at least soundtest.s, ADVM.s and sfr.i to be in the same folder if you wish to just drag and drop these into the waterbear folder (not the best option but if you only wanna quick assemble this it should work fine)

then run `waterbear assemble audiotest.s -o audiotest.vms`

## Current version: 1.1
- cleaned up some unnecessary instructions
- added the Groove table (see ADVM.s)
- changed the soundtest's bitmap to look more similar to the logo in ADVM.s
- changed how the commands are fetched, it don't really matter to the song format, just reduces stack usage
- fixed the music part stopping when an SFX was running instead of just muting (how did i not notice this)
