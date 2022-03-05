# Pof Tools GUI
Still a work in progress!

If you encounter any bugs or have any ideas for features or problems with PCS2 you'd like to see addressed please make an issue for it.
# PCS3???

This is sort-of PCS3. I don't feel quite comfortable calling it that, but it borrows a lot of the fundamental design PCS2 uses to ease the transition.

# 'pof' crate

This comes with the rust crate 'pof' which handles reading/writing pof files and extracting all of the info into native rust data structures if you want to make a program that interacts with pof files but doesn't need any of the GUI stuff.

## For Debian/Ubuntu users:

You might have to install these libraries if you want to compile the program.
```
sudo apt install libxcb-shape0-dev libxcb-xfixes0-dev
```
