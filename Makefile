ROMNAME := smurfed
HEADERNAME := "MAKRO19"

RGBPATH := ~/.local/bin

all: $(ROMNAME).gb

$(ROMNAME).gb: obj/$(ROMNAME).o
	$(RGBPATH)/rgblink -n $(ROMNAME).sym -o $(ROMNAME).gb obj/$(ROMNAME).o
	$(RGBPATH)/rgbfix -v -t $(HEADERNAME) $(ROMNAME).gb

obj/$(ROMNAME).o: $(ROMNAME).asm inc/* art/*.2bpp
	[ -d obj ] || mkdir obj
	$(RGBPATH)/rgbasm -h -o obj/$(ROMNAME).o $(ROMNAME).asm

art/ascii.2bpp: art/ascii.png
	$(RGBPATH)/rgbgfx -o art/ascii.2bpp art/ascii.png

art/intro_1.2bpp: art/intro_1.png
	$(RGBPATH)/rgbgfx -o art/intro_1.2bpp art/intro_1.png

art/lettering.2bpp: art/lettering.png
	$(RGBPATH)/rgbgfx -h -o art/lettering.2bpp art/lettering.png

art/logo.2bpp: art/logo.png
	$(RGBPATH)/rgbgfx -o art/logo.2bpp art/logo.png
clean:
	rm -r obj
	rm *.sym *.gb
