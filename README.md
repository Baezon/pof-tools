# Pof Tools
Still a work in progress!

Pof Tools is a program for Windows and Linux which let's you edit properties of Freespace's model format .pof as well as convert them to and from .dae that is meant as a successor to PCS2. If you're familiar with PCS2, this program will feel largely similar.
I'm more than willing to accept any code contributions, and if you have any bugs or feature proposals don't hesitate to post an issue.

One of the big goals of this project is to present an interface that is more intuitive and helps users in creating models. And to that end Pof Tools already has a few features over PCS2, such as displaying warnings and errors for problems in the model, realtime simulation of glow point 'blinking', and making sure that sub object fields are only populated with valid entries for that type.

HlP forums: https://www.hard-light.net/forums/index.php?topic=98096.0

# 'pof' crate

This comes with the rust crate 'pof' which handles reading/writing pof files and extracting all of the info into native rust data structures if you want to make a program that interacts with pof files but doesn't need any of the GUI stuff.

## For Debian/Ubuntu users:

You might have to install these libraries if you want to compile the program.
```
sudo apt install libxcb-shape0-dev libxcb-xfixes0-dev
```
