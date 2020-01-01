ca65 ncat.asm
ld65 -C ncat.cfg -o ncat.prg ncat.o
copy /b ncat.hdr+ncat.prg+g\ncat.spr+g\ncat.chr "Nintencat - The Parody.nes"
pause