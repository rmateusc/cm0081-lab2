# cm0081-lab2
Name: Rafael Mateus Carrión & Daniel Otero Gómez

General description: our program reads an .xml file, parses it and writes a .csv
file with the parsed information. The information to be parsed corresponds to
the atributes of the meteorite data, if a specific meteorite data does not have
a specific attribute, in the .csv file it appears as empty. In order to parse
the file, our program recieves an inital string and divides it into strings that
contain a meteorite data, followed by this, parses each meteorite into a list
with the previoulsy mentioned attributes. Then, this list is transforme into a
vector that later on our program writes it in a .csv file.

To use our program: use the ./mine command followed by the address of the .xml
file and the name of the .csv file to be written.

GHC version: 8.6.5
